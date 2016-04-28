/***************************************************************************
 * Copyright (C) 2008-2013 by Cameron Wong                                 *
 * name in passport: HUANG GUANNENG                                        *
 * email: hgneng at gmail.com                                              *
 * website: http://www.eguidedog.net                                       *
 *                                                                         *
 * This program is free software; you can redistribute it and/or           *
 * modify it under the terms of the GNU General Public License             *
 * as published by the Free Software Foundation; either version 2          *
 * of the License, or any later version.                                   *
 *                                                                         *
 * This program is distributed in the hope that it will be useful,         *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
 * GNU General Public License for more details.                            *
 *                                                                         *
 * You should have received a copy of the GNU General Public License       *
 * along with this program; if not, write to the Free Software             *
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,              *
 * MA  02110-1301, USA.                                                    *
 **************************************************************************/
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <dirent.h>
#include <time.h>
#include <errno.h>
#include <signal.h>
#include "config.h"
#include "ekho_impl.h"
#include "ekho.h"
#include "ekho_dict.h"
#include "sr-convert.cpp"
#include "sonic.h"
#include "utf8.h"

#ifdef ENABLE_WIN32
#include <winsock2.h>
#include <windows.h>
/* We need the following two to set stdin/stdout to binary */
#include <io.h>
#include <fcntl.h>
#define sleep(seconds) Sleep((seconds)*1000)
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/wait.h>
#endif

#ifdef ENABLE_FESTIVAL
#include "festival/festival.h"
#define ENABLE_ENGLISH
#endif

using namespace ekho;
using namespace std;

#ifdef HAVE_MP3LAME
#include <lame/lame.h>
#endif

bool EkhoImpl::mDebug = false;

EkhoImpl::EkhoImpl() {
    this->init();
}

int EkhoImpl::init(void) {
  mPendingFrames = 0;
  mStripSsml = true;
  mSpeakIsolatedPunctuation = true;
  mSpeechQueueMutex = PTHREAD_MUTEX_INITIALIZER;
  mSpeechQueueCond = PTHREAD_COND_INITIALIZER;
  mEnglishVoice = "voice_kal_diphone";
  this->mSndFile = 0;
  this->isRecording = false;
  this->tempoDelta = 0;
  this->pitchDelta = 0;
  this->rateDelta = 0;

  this->isStopped = false;
  this->isPaused = false;
  this->isEnded = false;
  this->isSoundInited = false;
  this->isSpeechThreadInited = false;

  mSonicStream = 0;
  mPcmCache = true;

#ifdef HAVE_PULSEAUDIO
  this->stream = NULL;
#endif

#ifdef ANDROID
  mFliteVoice = 0;
#endif

  memset(mAlphabetPcmCache, 0, 26);
  memset(mAlphabetPcmSize, 0, 26);

  mPuncMode = EKHO_PUNC_SOME;

  return 0;
}

EkhoImpl::EkhoImpl(string voice) {
    this->init();
    this->setVoice(voice);
}

EkhoImpl::~EkhoImpl(void) {
  pthread_mutex_lock(&mSpeechQueueMutex);
  this->isEnded = true;
  pthread_cond_signal(&mSpeechQueueCond);
  pthread_mutex_unlock(&mSpeechQueueMutex);
  pthread_cond_destroy(&mSpeechQueueCond);

  if (this->isSpeechThreadInited) {
    void *ret;
    pthread_join(this->speechThread, &ret);
  }
  closeStream();

#ifndef ENABLE_SOUNDTOUCH
  if (mSonicStream) {
    sonicDestroyStream(mSonicStream);
    mSonicStream = 0;
  }
#endif

  // TODO: free mAlphabetPcmCache
}

int EkhoImpl::initSound(void) {
    if (! this->isSoundInited) {
        // launch speechDaemon
        pthread_create(&this->speechThread, NULL, speechDaemon, (void*)this);
        this->isSpeechThreadInited = true;

#ifdef HAVE_PULSEAUDIO
        this->isSoundInited = true;
        if (initStream() < 0) {
          cerr << "Fail to init audio stream." << endl;
          return -1;
        }
#else
        int hasMplayer = system("mplayer -really-quiet 1>/dev/null 2>/dev/null");
        if (hasMplayer == 0) {
            this->player = "mplayer -really-quiet";
        } else {
            int hasOgg = system("ogg123 -h 1>/dev/null 2>/dev/null");
            if (hasOgg == 0) {
                this->player = "ogg123 -q";
            } else {
                int hasPlay = system("play -h 1>/dev/null 2>/dev/null");
                if (hasPlay == 0) {
                    this->player = "play -q";
                } else {
                    cerr << "Fail to find OGG player. Please install ogg123 (vorbis-tools), mplayer or play (sox)." << endl;
                    return -1;
                }
            }
        }
#endif
    }

    return 0;
}

int EkhoImpl::initStream(void) {
  closeStream();
  if (mDict.getLanguage() == ENGLISH) {
    mDict.mSfinfo.samplerate = 16000;
    mDict.mSfinfo.channels = 1;
  } else if (mDict.mSfinfo.samplerate == 0) {
    cerr << "Sample rate not detected: " << mDict.getLanguage() << endl;
    return -1;
  }
#ifdef ENABLE_SOUNDTOUCH
  this->pSoundtouch.setSampleRate(mDict.mSfinfo.samplerate);
  this->pSoundtouch.setChannels(1);
  this->pSoundtouch.setSetting(SETTING_USE_QUICKSEEK, 1);
#else
  mSonicStream = sonicCreateStream(mDict.mSfinfo.samplerate, 1);
  // sonicSetQuality(mSonicStream, 1); // high quality but slower`
#endif

#ifdef HAVE_PULSEAUDIO
  /* create stream */
  if (this->isSoundInited) {
    pa_sample_spec ss;
    ss.channels = mDict.mSfinfo.channels;
    ss.rate = mDict.mSfinfo.samplerate;
    ss.format = PA_SAMPLE_S16LE;
    int error;

    if (EkhoImpl::mDebug) {
      cerr << "pa_sample_spec(format=" << ss.format <<
        ",rate=" << ss.rate << ",channels=" << ss.channels <<
        "ch=" << mDict.mSfinfo.channels << ")" << endl;
    }

    this->stream = pa_simple_new(NULL, "Ekho", PA_STREAM_PLAYBACK, NULL, "playback", &ss, NULL, NULL, &error);
    
    if (!this->stream) {
      cerr << "pa_simple_new() failed: " << pa_strerror(error) << endl;
      cerr << "pa_sample_spec(format=" << ss.format <<
        ",rate=" << ss.rate << ",channels=" << ss.channels <<
        "ch=" << mDict.mSfinfo.channels << ")" << endl;
      return -1;
    }
  }
#endif // end of HAVE_PULSEAUDIO

  return 0;
} // end of initStream

void EkhoImpl::closeStream(void) {
  if (mSonicStream) {
    sonicDestroyStream(mSonicStream);
    mSonicStream = 0;
  }

#ifdef HAVE_PULSEAUDIO
  if (this->stream) {
    // flush stream
    int error;
    if (pa_simple_drain(this->stream, &error) < 0) {
      cerr << "pa_simple_drain() failed: " << pa_strerror(error) << endl;
    }

    pa_simple_free(this->stream);
    this->stream = 0;
  }
#endif
}

static bool gsIsFestivalInited = false;
int EkhoImpl::initFestival(void) {
#ifdef ENABLE_FESTIVAL
  if (! gsIsFestivalInited) {
    int heap_size = 2100000; // scheme heap size
    int load_init_files = 0; // don't load default festival init files
    festival_initialize(load_init_files, heap_size);

    // set libdir of festival
    string path(mDict.mDataPath);
    path += "/festival/lib";
    siod_set_lval("libdir", strintern(path.c_str()));

    path = mDict.mDataPath;
    path += "/festival/lib/init.scm";
    festival_load_file(path.c_str());
    //festival_eval_command("(Parameter.set 'Duration_Stretch 1.5)");

    gsIsFestivalInited = true;

    if (EkhoImpl::mDebug) {
      cerr << mDict.mDataPath << "/festival inited." << endl;
    }
  } else {
    festival_tidy_up();
  }
#endif
  return 0;
}

int EkhoImpl::saveWav(string text, string filename) {
    initFestival();
    if (EkhoImpl::mDebug) {
        cerr << "Writting WAV file " << filename << " ..." << endl;
    }

    // open record file
    SF_INFO sfinfo;
    memcpy(&sfinfo, &mDict.mSfinfo, sizeof(SF_INFO));
    sfinfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
    if (EkhoImpl::mDebug) {
      cerr << "sfinfo format: samplerate=" << sfinfo.samplerate <<
          ", channel=" << sfinfo.channels;
      cerr.setf(ios::hex);
      cerr << ", format=" << sfinfo.format << endl;
      cerr.unsetf(ios::hex);
    }
    mSndFile = sf_open(filename.c_str(), SFM_WRITE, &sfinfo);
    if (!mSndFile) {
        cerr << "Fail to open file " << filename
            << " at " << __LINE__ << endl;
    }

    synth2(text, writePcm);
    finishWritePcm();

    // close record file
    sf_close(mSndFile);

    if (EkhoImpl::mDebug)
        cerr << "Finish writting WAV file " << filename << endl;

    return 0;
}

int EkhoImpl::saveOgg(string text, string filename) {
    initFestival();
    if (EkhoImpl::mDebug) {
        cerr << "Writting OGG file " << filename << " ..." << endl;
    }

    // open record file
    SF_INFO sfinfo;
    memcpy(&sfinfo, &mDict.mSfinfo, sizeof(SF_INFO));
    sfinfo.format = SF_FORMAT_OGG | SF_FORMAT_VORBIS;
    if (EkhoImpl::mDebug) {
      cerr << "sfinfo format: samplerate=" << sfinfo.samplerate <<
          ", channel=" << sfinfo.channels;
      cerr.setf(ios::hex);
      cerr << ", format=" << sfinfo.format << endl;
      cerr.unsetf(ios::hex);
    }
    mSndFile = sf_open(filename.c_str(), SFM_WRITE, &sfinfo);
    if (!mSndFile) {
        cerr << "Fail to open file " << filename
            << " at " << __LINE__ << endl;
        cerr << "error code: " << sf_error(0) << endl;
        cerr << "error: " << sf_strerror(0) << endl;
    }

    synth2(text, writePcm);
    finishWritePcm();

    // close record file
    sf_close(mSndFile);

    if (EkhoImpl::mDebug) {
        cerr << "Finish writting WAV file " << filename << " ..." << endl;
    }
    return 0;
}

// generate temp filename
// to be improve...
string EkhoImpl::genTempFilename() {
#ifdef ENABLE_WIN32
    string tmpfile("\\TEMP\\ekho_");
#else
    string tmpfile("/tmp/ekho_");
#endif
    char port_str[10];
    sprintf(port_str, "%d", mPort);
    tmpfile += port_str;
    /* this is a bit slow, use a fixed filename instead
    int fd = mkstemp(tmpfile);
    close(fd);
    string filename = tmpfile;

    return filename;
    */
    return tmpfile;
}

#if 0 // no longer used
#ifdef HAVE_OGG
// This function should no longer use.
// Instead, we use libsndfile to write OGG files.
int EkhoImpl::saveOgg(string text, string filename) {

    string tmp_wav = genTempFilename() + ".wav";

    // save to wav first
    this->saveWav(text, tmp_wav);

    if (EkhoImpl::mDebug) {
        cerr << "Writting OGG file " << filename << endl;
    }

    ogg_stream_state os; /* take physical pages, weld into a logical
                            stream of packets */
    ogg_page         og; /* one Ogg bitstream page.  Vorbis packets are inside */
    ogg_packet       op; /* one raw packet of data for decode */
    vorbis_info      vi; /* struct that stores all the static vorbis bitstream settings */
    vorbis_comment   vc; /* struct that stores all the user comments */
    vorbis_dsp_state vd; /* central working state for the packet->PCM decoder */
    vorbis_block     vb; /* local working space for packet->PCM decode */
    int i;
    int eos = 0;

#ifdef ENABLE_WIN32
    /* We need to set stdin/stdout to binary mode. */
    /* if we were reading/writing a file, it would also need to in
       binary mode, eg, fopen("file.wav","wb"); */
    /* Beware the evil ifdef. We avoid these where we can, but this one we 
       cannot. Don't add any more, you'll probably go to hell if you do. */
    _setmode( _fileno( stdin ), _O_BINARY );
    _setmode( _fileno( stdout ), _O_BINARY );  
#endif  

    /* open input file */
    SF_INFO sfinfo;
    memset(&sfinfo, 0, sizeof(sfinfo));
    SNDFILE *wav_file = sf_open(tmp_wav.c_str(), SFM_READ, &sfinfo);
    if (!wav_file) {
        cerr << "Fail to open file " << tmp_wav << ": " << sf_strerror(NULL)
            << " at " << __LINE__ << endl;
        return -1;
    }

    /* open output file */
    FILE *ogg_file = fopen(filename.c_str(), "wb");

    vorbis_info_init(&vi);
    /*********************************************************************
     * Encoding using a VBR quality mode.  The usable range is -.1
     * (lowest quality, smallest file) to 1. (highest quality, largest file).
     */
    int ret = vorbis_encode_init_vbr(&vi, 1, mDict.mSfinfo.samplerate, 0.1);
    if (ret) {
        cerr << "vorbis_encode_init_vbr fail" << endl;
        return -1;
    }

    /* add a comment */
    vorbis_comment_init(&vc);
    vorbis_comment_add_tag(&vc, (char*)"ENCODER", (char*)"Ekho");

    /* set up the analysis state and auxiliary encoding storage */
    vorbis_analysis_init(&vd,&vi);
    vorbis_block_init(&vd,&vb);

    /* set up our packet->stream encoder */
    /* pick a random serial number; that way we can more likely build
       chained streams just by concatenation */
    srand(time(NULL));
    ogg_stream_init(&os,rand());
    /* Vorbis streams begin with three headers; the initial header (with
       most of the codec setup parameters) which is mandated by the Ogg
       bitstream spec.  The second header holds any comment fields.  The
       third header holds the bitstream codebook.  We merely need to
       make the headers, then pass them to libvorbis one at a time;
       libvorbis handles the additional Ogg bitstream constraints */

    ogg_packet header;
    ogg_packet header_comm;
    ogg_packet header_code;

    vorbis_analysis_headerout(&vd, &vc, &header, &header_comm, &header_code);
    ogg_stream_packetin(&os, &header); // automatically placed in its own page
    ogg_stream_packetin(&os, &header_comm);
    ogg_stream_packetin(&os, &header_code);

    /* This ensures the actual
     * audio data will start on a new page, as per spec
     */
    while (!eos) {
        int result = ogg_stream_flush(&os, &og);
        if (result == 0) break;
        fwrite(og.header, 1, og.header_len, ogg_file);
        fwrite(og.body, 1, og.body_len, ogg_file);
    }

    short *wav_buf = new short[BUFFER_SIZE];

    while (!eos) {
        int samples = sf_readf_short(wav_file, (short*)wav_buf, BUFFER_SIZE);

        if (samples == 0){
            /* end of file.  this can be done implicitly in the mainline,
               but it's easier to see here in non-clever fashion.
               Tell the library we're at end of stream so that it can handle
               the last frame and mark end of stream in the output properly */
            vorbis_analysis_wrote(&vd,0);

        } else {
            /* data to encode */

            /* expose the buffer to submit data */
            float **buffer = vorbis_analysis_buffer(&vd, samples + samples);

            /* uninterleave samples */
            for (i = 0; i < samples; i++) {
                buffer[0][i] = wav_buf[i] / 32768.f;
            }

            /* tell the library how much we actually submitted */
            vorbis_analysis_wrote(&vd, i);
        }

        /* vorbis does some data preanalysis, then divvies up blocks for
           more involved (potentially parallel) processing.  Get a single
           block for encoding now */
        while (vorbis_analysis_blockout(&vd, &vb) == 1) {

            /* analysis, assume we want to use bitrate management */
            vorbis_analysis(&vb,NULL);
            vorbis_bitrate_addblock(&vb);

            while (vorbis_bitrate_flushpacket(&vd,&op)){

                /* weld the packet into the bitstream */
                ogg_stream_packetin(&os, &op);

                /* write out pages (if any) */
                while (!eos) {
                    int result = ogg_stream_pageout(&os,&og);
                    if (result == 0) break;
                    fwrite(og.header, 1, og.header_len, ogg_file);
                    fwrite(og.body, 1, og.body_len, ogg_file);

                    /* this could be set above, but for illustrative purposes, I do
                       it here (to show that vorbis does know where the stream ends) */
                    if (ogg_page_eos(&og)) eos = 1;
                }
            }
        }
    }

    delete(wav_buf);

    /* clean up and exit.  vorbis_info_clear() must be called last */
    ogg_stream_clear(&os);
    vorbis_block_clear(&vb);
    vorbis_dsp_clear(&vd);
    vorbis_comment_clear(&vc);
    vorbis_info_clear(&vi);

    fclose(ogg_file);
    sf_close(wav_file);

    //remove(tmp_wav.c_str());

    if (EkhoImpl::mDebug) {
        cerr << "Finish writting " << filename << endl;
    }

    return 0;
}
#endif
#endif

#ifdef HAVE_MP3LAME
int EkhoImpl::saveMp3(string text, string filename) {
    int     pcmswapbytes;
    FILE   *musicin;
    lame_global_flags *gf;
    FILE   *outf;

    string tmp_wav = genTempFilename() + ".wav";
    this->saveWav(text, tmp_wav);

    /* open the input file */
    pcmswapbytes = 0; // don't swap bytes

    /* Try to open the sound file */
    SF_INFO gs_wfInfo;
    memset(&gs_wfInfo, 0, sizeof(gs_wfInfo));
    SNDFILE *gs_pSndFileIn = sf_open(tmp_wav.c_str(), SFM_READ, &gs_wfInfo);
    if (gs_pSndFileIn == NULL) {
        sf_perror(gs_pSndFileIn);
        cerr << "Could not open sound file " << tmp_wav << endl;
        return -1;
    }
    musicin = (FILE *) gs_pSndFileIn;

    /* initialize libmp3lame */
    if (NULL == (gf = lame_init())) {
        cerr << "fatal error during initialization" << endl;
        return -1;
    }

    (void) lame_set_num_samples(gf, gs_wfInfo.frames);
    if (-1 == lame_set_num_channels(gf, gs_wfInfo.channels)) {
        cerr << "Unsupported number of channels: " << gs_wfInfo.channels << endl;
        return -1;
    }

    (void) lame_set_in_samplerate(gf, gs_wfInfo.samplerate);
    if (lame_init_params(gf) < 0) {
        cerr << "fatal error during initialization" << endl;
        lame_close(gf);
        return -1;
    }

    /* open output file */
    if ((outf = fopen(filename.c_str(), "w+b")) == NULL) {
        lame_close(gf);
        return -1;
    }

    if (EkhoImpl::mDebug) {
        cerr << "Writting MP3 file " << filename << endl;
    }

    /* encode until we hit eof */
    unsigned char mp3buffer[LAME_MAXMP3BUFFER];
    int Buffer[2][BUFFER_SIZE];
    int samples_read;
    int imp3;
    int owrite;
    size_t totalread = 0; // for debug
    do {
        /* read in 'iread' samples */
        int     num_channels = lame_get_num_channels(gf);
        int     insamp[2 * BUFFER_SIZE];
        int     framesize;
        int     samples_to_read;
        unsigned int tmp_num_samples;
        int     i;
        int    *p;

        samples_to_read = framesize = lame_get_framesize(gf);
        if (framesize > BUFFER_SIZE) {
            cerr << "framesize: " << framesize << endl;
            return -1;
        }

        /* get num_samples */
        tmp_num_samples = lame_get_num_samples(gf);

        samples_read = sf_read_int((SNDFILE *) musicin, insamp, num_channels * samples_to_read);
        totalread += samples_read;

        if (samples_read < 0) {
            return samples_read;
        }
        p = insamp + samples_read;
        samples_read /= num_channels;
        if (Buffer != NULL) { /* output to int buffer */
            if (num_channels == 2) {
                for (i = samples_read; --i >= 0;) {
                    Buffer[1][i] = *--p;
                    Buffer[0][i] = *--p;
                }
            } else if (num_channels == 1) {
                memset(Buffer[1], 0, samples_read * sizeof(int));
                for (i = samples_read; --i >= 0;) {
                    Buffer[0][i] = *--p;
                }
            } else {
                cerr << "Bad channel number: " << num_channels << endl;
                return -1;
            }
        }

        if (samples_read >= 0) {
            /* encode */
            imp3 = lame_encode_buffer_int(gf, Buffer[0], Buffer[1], samples_read, mp3buffer, sizeof(mp3buffer));
            if (imp3 < 0) {
                if (imp3 == -1) {
                    cerr << "mp3 buffer is not big enough... " << endl;
                } else {
                    cerr << "mp3 internal error:  error code=" << imp3 << endl;
                }
                return -1;
            }
            owrite = (int) fwrite(mp3buffer, 1, imp3, outf);
            if (owrite != imp3) {
                cerr << "Error writing mp3 output" << endl;
                return -1;
            }
        }
    } while (samples_read > 0);

    /* Add some blank to the end of mp3 file.
     * This can avoid some wave missing
     */
    memset(Buffer, 0, sizeof(Buffer));
    /* 500 is a experience number, try a better one */
    imp3 = lame_encode_buffer_int(gf, Buffer[0], Buffer[1], BUFFER_SIZE, mp3buffer, sizeof(mp3buffer));
    fwrite(mp3buffer, 1, imp3, outf);

    imp3 = lame_encode_flush(gf, mp3buffer, sizeof(mp3buffer)); /* may return one more mp3 frame */

    if (imp3 < 0) {
        if (imp3 == -1) {
            cerr << "mp3 buffer is not big enough... " << endl;
        } else {
            cerr << "mp3 internal error:  error code=" << imp3 << endl;
        }
        return -1;
    }

    owrite = (int) fwrite(mp3buffer, 1, imp3, outf);
    if (owrite != imp3) {
        cerr << "Error writing mp3 output" << endl;
        return -1;
    }

    fclose(outf); /* close the output file */
    if (sf_close((SNDFILE *) musicin) != 0) {
        cerr << "Could not close sound file" << endl;
    }
    lame_close(gf);

    //remove(tmp_wav.c_str());

    if (EkhoImpl::mDebug) {
        cerr << "Finish writing MP3 file " << filename << endl;
    }

    return 0;
}
#endif

int EkhoImpl::speakPcm(short *pcm, int frames, void *arg,
    bool in_word_context, bool forbid_overlap) {
  short buffer[1024];
  int error;

  if (!pcm)
    return -1;
  EkhoImpl *pEkho = (EkhoImpl*)arg;
  
#ifdef HAVE_PULSEAUDIO
  if (!pEkho->mSonicStream)
    return 0;
  //frames = sonicWriteShortToStream(pEkho->mSonicStream, pcm, frames);
  frames = pEkho->writeToSonicStream(pcm, frames, in_word_context, forbid_overlap);
  if (!frames)
    sonicFlushStream(pEkho->mSonicStream);

  do {
    frames = sonicReadShortFromStream(pEkho->mSonicStream, buffer, 1024);
    if (frames > 0 && !pEkho->isStopped) {
      int ret = pa_simple_write(pEkho->stream, buffer, frames * 2, &error);
      if (ret < 0)
        cerr << "pa_simple_write failed: " << pa_strerror(error) << endl;
    }
  } while (frames > 0 && pEkho->mSonicStream);
#endif

  return 0;
}

int EkhoImpl::writeToSonicStream(short *pcm, int frames,
    bool in_word_context, bool forbid_overlap) {
  if (!mSonicStream)
    return 0;

  float percent = 0; //0.1;
  if (forbid_overlap) {
    percent = 0;
  } else if (in_word_context) {
    //percent = 0.1;
  }

  if (mPendingFrames > 0) {
    // make a liner joining. fade out + fade in
    // Reference: splice.c of sox
    for (int i = 1; i < mPendingFrames && i <= frames; i++) {
      double p = i / mPendingFrames;
      mPendingPcm[i] = mPendingPcm[i] * (1 - p) + pcm[i] * p;
    }
  }

  if (mPendingFrames > frames) {
    frames = sonicWriteShortToStream(mSonicStream, mPendingPcm, mPendingFrames);
    mPendingFrames = 0;
  } else {
    int ori_frames = frames;
    frames = sonicWriteShortToStream(mSonicStream, mPendingPcm, mPendingFrames);
    int new_pending_frames = (ori_frames - mPendingFrames) * percent;
    if (new_pending_frames > PENDING_PCM_FRAMES)
      new_pending_frames = PENDING_PCM_FRAMES;

    frames += sonicWriteShortToStream(mSonicStream, pcm + mPendingFrames,
        ori_frames - mPendingFrames - new_pending_frames);
    mPendingFrames = new_pending_frames;
    memcpy(mPendingPcm, &pcm[ori_frames - mPendingFrames - 1], mPendingFrames * 2);
  }

  return frames;
}

void EkhoImpl::finishWritePcm(void) {
  writePcm(0, 0, this);
}

int EkhoImpl::writePcm(short *pcm, int frames, void *arg,
    bool in_word_context, bool forbid_overlap) {
  short buffer[BUFFER_SIZE];

  if (!pcm)
    return -1;
  EkhoImpl *pEkho = (EkhoImpl*)arg;
  if (!pEkho->mSonicStream)
    return -1;

  //frames = sonicWriteShortToStream(pEkho->mSonicStream, pcm, frames);
  frames = pEkho->writeToSonicStream(pcm, frames, in_word_context);

  if (!frames)
    sonicFlushStream(pEkho->mSonicStream);

  do {
    frames = sonicReadShortFromStream(pEkho->mSonicStream, buffer, BUFFER_SIZE);
    if (frames > 0) {
      int writtenFrames = sf_writef_short(pEkho->mSndFile, buffer, frames);
      if (frames != writtenFrames) {
        cerr << "Fail to write WAV file " 
          << writtenFrames << " out of " << frames 
          << " written" << endl;
        return -1;
      }
    }
  } while (frames > 0);

  return 0;
}

void* EkhoImpl::speechDaemon(void *args) {
    EkhoImpl* pEkho = (EkhoImpl*)args;

    pEkho->initFestival();

    while (1) {
        pthread_mutex_lock(&pEkho->mSpeechQueueMutex);
        if (pEkho->mSpeechQueue.empty() && !pEkho->isEnded)
          pthread_cond_wait(&pEkho->mSpeechQueueCond, &pEkho->mSpeechQueueMutex);

        if (pEkho->isEnded) {
          pthread_mutex_unlock(&pEkho->mSpeechQueueMutex);
          break;
        }

        if (pEkho->mSpeechQueue.empty()) {
          pthread_mutex_unlock(&pEkho->mSpeechQueueMutex);
          continue;
        }
        SpeechOrder order = pEkho->mSpeechQueue.front();
        pthread_mutex_unlock(&pEkho->mSpeechQueueMutex);

        if (EkhoImpl::mDebug) {
            cerr << "speaking '" << order.text << "'" << endl;
        }

        // It seems that Sonic doesn't work on threads. Set arguments again. It's fixed before. But it doesn't work again...
        pEkho->setSpeed(pEkho->tempoDelta);
        pEkho->setPitch(pEkho->pitchDelta);
        pEkho->setVolume(pEkho->volumeDelta);
        pEkho->setRate(pEkho->rateDelta);

        pEkho->synth2(order.text, speakPcm);

        // FIXME: following statement seems not flush rest PCM
        pEkho->speakPcm(0, 0, pEkho, false);

        int error;
#ifdef HAVE_PULSEAUDIO
        if (pEkho->isStopped)
          pa_simple_flush(pEkho->stream, &error);
        else
          pa_simple_drain(pEkho->stream, &error);
#endif

        if (pEkho->isStopped)
          continue;

        if (order.pCallback) {
            order.pCallback(order.pCallbackArgs);
        }

        pthread_mutex_lock(&pEkho->mSpeechQueueMutex);
        if ((!pEkho->isStopped) && (!pEkho->mSpeechQueue.empty()))
          pEkho->mSpeechQueue.pop();
        pthread_mutex_unlock(&pEkho->mSpeechQueueMutex);
    }

    return 0;
} // end of speechDaemon

/**
 * Only strip one level SSML like <speak>...</speak>
 */
static string stripSsml(string text) {
  int first_lt = text.find_first_of('<');
  if (first_lt == 0) {
    int first_gt = text.find_first_of('>');
    if (first_gt > 0) {
      string tag = text.substr(first_lt + 1, first_gt - first_lt - 1);
      string endtag("</");
      endtag += tag + ">";
      int last_endtag = text.find_last_of(endtag);
      if (last_endtag == text.length() - 1)
        return text.substr(first_gt + 1, last_endtag - first_gt - endtag.length());
    }
  }
  
  return text;
}

// @TODO: remove this deprecared method
int EkhoImpl::synth(string text, SynthCallback *callback, void *userdata) {
#ifdef DEBUG_ANDROID
  LOGD("Ekho::synth(%s, %p, %p) voiceFileType=%s lang=%d", text.c_str(), callback,
      userdata, mDict.mVoiceFileType, mDict.getLanguage());
#endif

  // init
  string unknown_str = "";
  bool has_unknown_char = false;
  bool has_festival_char = false;
  float pause = 0;
  const char *pPcm = NULL;
  int size = 0;
  this->isStopped = false;
  this->isPaused = false;

  if (mDict.getLanguage() == ENGLISH) {
#ifdef ENABLE_ENGLISH
    if (EkhoImpl::mDebug) {
      cerr << "speaking '" << text
        << "' with Festival" << endl;
    }
    pPcm = this->getPcmFromFestival(text, size);
    // output pcm data
    if (pPcm) {
      if (userdata)
        callback((short*)pPcm, size / 2, userdata, false, true);
      else	
        callback((short*)pPcm, size / 2, this, false, true);
    }
#endif
    return 0;
  }

  // check whehter voice file available
  if (!mDict.mVoiceFileType && mDict.getLanguage() != ENGLISH) {
    cerr << "Voice file not found." << endl;
    return -1;
  }

  // filter SSML
  if (mStripSsml)
    text = stripSsml(text);

  // check punctuation
  if (mSpeakIsolatedPunctuation && text.length() <= 3) {
    const char *c = text.c_str();
    int code = utf8::next(c, c + text.length());
    if (!*c && mDict.isPunctuationChar(code))
      text = mDict.getPunctuationName(code);
  }

  // translate punctuations
  if (mPuncMode == EKHO_PUNC_ALL)
    translatePunctuations(text);

  // filter spaces
  filterSpaces(text);

  list<PhoneticSymbol*> phons = mDict.lookup(text);

  // get word context array
  int phons_len = phons.size();
  char *in_word_context = (char*)malloc(phons_len);
  mDict.getWordContext(text.c_str(), in_word_context, phons_len);
  int wi = 0;

  for (list<PhoneticSymbol*>::iterator li = phons.begin();
      li != phons.end() || has_unknown_char;
      li++) {

#ifdef DEBUG_ANDROID
    LOGD("speak symbol: %s", (*li)->symbol);
#endif

    // stop/pause control
    if (this->isStopped) {
#ifdef DEBUG_ANDROID
      LOGD("isStopped");
#endif
      return 1;
    }

    while (this->isPaused) {
#ifdef DEBUG_ANDROID
      LOGD("isPaused");
#endif
      sleep(1);
    }

    bool speaknow = false;
    // check whether it's unknown character (English)
#ifdef ENABLE_ENGLISH
    if (li != phons.end() &&
        ((!*li) || ((*li)->symbol && (((*li)->symbol[0] == '\\') ||
                                      (has_unknown_char &&
                                       strstr((*li)->symbol, "pause") > 0))))) {
      has_unknown_char = true;

      if (*li && strstr((*li)->symbol, "pause") > 0) {
        unknown_str += " ";
        // break ,. etc. for Festival
        if (strstr((*li)->symbol, "fullpause") > 0 ||
            strstr((*li)->symbol, "halfpause") > 0) {
          speaknow = true;
        }
      } else {
        if (*li && strlen((*li)->symbol) == 2 && (*li)->symbol[1] >= 65 && (*li)->symbol[1] <= 122) {
          unknown_str += (*li)->symbol + 1;
          has_festival_char = true;
        } else if (*li && strlen((*li)->symbol) == 3) {
          int code = utf8::peek_next((*li)->symbol + 1, (*li)->symbol + 3);
          if (code <= 0xff) {
            unknown_str += (unsigned char)code;
            has_festival_char = true;
          } else {
            unknown_str += " ";
          }
        } else {
          // illegal characters for Festival
          unknown_str += " ";
        }
      }
    } else
#endif
    if (li != phons.end() && (*li) &&
        (*li)->symbol && strstr((*li)->symbol, "fullpause") > 0) {
      pause += 1;
    } else if (li != phons.end() && (*li) &&
        (*li)->symbol && strstr((*li)->symbol, "halfpause") > 0) {
      pause += 0.5;
    } else if (li != phons.end() && (*li) &&
        (*li)->symbol && strstr((*li)->symbol, "quaterpause") > 0) {
      pause += 0.25;
    } else {
      speaknow = true;
    }

    if (speaknow) {
      // begin legal char precessing
      bool shouldFreePcm = false;
      pPcm = 0;
      if (has_unknown_char) {
        if (has_festival_char) {
#ifdef DEBUG_ANDROID
          LOGD("has_unknown_char");
#endif
          pPcm = 0;
#ifdef ENABLE_ENGLISH
          // output wave of unknown string with Festival
          if (EkhoImpl::mDebug) {
            cerr << "speaking '" << unknown_str
              << "' with Festival" << endl;
          }
          char c;
          if ((unknown_str.length() == 1) && (c = tolower(unknown_str[0])) &&
              c >= 'a' && c <= 'z') {
            if (!mAlphabetPcmCache[c - 'a'])
              mAlphabetPcmCache[c - 'a'] = getPcmFromFestival(unknown_str,
                  mAlphabetPcmSize[c - 'a']);

            pPcm = mAlphabetPcmCache[c - 'a'];
            size = mAlphabetPcmSize[c - 'a'];
            has_festival_char = false;
          } else {
            pPcm = this->getPcmFromFestival(unknown_str, size);
          }
#endif
          if (!pPcm) {
            has_unknown_char = false;
            pPcm = this->mDict.getFullPause()->getPcm(size);
          }
          pause = 0;
          unknown_str.clear();
          li--; // Play PCM from Festival, play current char next round
          wi--;
        } else if (unknown_str.size() > 1) {
          pause += 0.25;
          unknown_str.clear();
          li--;
          wi--;
        } else {
          unknown_str.clear();
          li--; // drop unknown_str, restart from current char next round
          wi--;
        }
      }
      
      if (pause > 0) {
#ifdef DEBUG_ANDROID
        LOGD("pause");
#endif
        if (pause >= 1)
          pPcm = this->mDict.getFullPause()->getPcm(size);
        else if (pause >= 0.5)
          pPcm = this->mDict.getHalfPause()->getPcm(size);
        else
          pPcm = this->mDict.getQuaterPause()->getPcm(size);
        pause = 0;
        if (!has_unknown_char) {
          li--;
          wi--;
        }
      } else if (!pPcm && li != phons.end() && (*li)) {
        string path(mDict.mDataPath);
        path += "/";
        path += mDict.getVoice();
        pPcm = (*li)->getPcm(path.c_str(), mDict.mVoiceFileType, size);
        
        // speak Mandarin for Chinese
        if (!pPcm && mDict.getLanguage() == TIBETAN) {
          path = mDict.mDataPath + "/pinyin";
          pPcm = (*li)->getPcm(path.c_str(), mDict.mVoiceFileType, size);
        }

        if (!mPcmCache)
          shouldFreePcm = true;
#ifdef DEBUG_ANDROID
        LOGD("voice path: %s, type: %s, size: %d, pPcm: %p", path.c_str(),
            mDict.mVoiceFileType, size, pPcm);
#endif
      }

      // output pcm data
      if (pPcm) {
        bool forbid_overlap = false;

        if (userdata)
          callback((short*)pPcm, size / 2, userdata, in_word_context[wi], forbid_overlap);
        else	
          callback((short*)pPcm, size / 2, this, in_word_context[wi], forbid_overlap);
      }

      if (has_unknown_char) {
        if (has_festival_char) {
          has_festival_char = false;
          if (pPcm)
            delete[] pPcm;
        }
        has_unknown_char = false;
      } else if (shouldFreePcm) {
        (*li)->setPcm(0, 0);
      }
    } // end of legal char processing

    if (phons.end() == li)
      break;

    wi++;
  } // end of for

  // send a signal to abort for Android
  if (userdata)
    callback(0, 0, userdata, false, false);
  else
    callback(0, 0, this, false, false);

  free(in_word_context);

  return 0;
}

int EkhoImpl::play(string file) {
    system((this->player + " " + file + " 2>/dev/null").c_str());

    return 0;
}

// It's caller's responsibility to delete the returned pointer
const char* EkhoImpl::getPcmFromFestival(string text, int& size) { 
#ifdef ANDROID
  if (mFliteVoice) {
    cst_wave *flite_wave = flite_text_to_wave(text.c_str(), mFliteVoice);
    short *pcm = flite_wave->samples;
    size = flite_wave->num_samples * 2;
    free(flite_wave);
#ifdef DEBUG_ANDROID
    LOGD("Ekho::getPcmFromFestival(%s, %d)", text.c_str(), size);
#endif
    return (const char*)pcm;
  } else {
    return 0;
  }
#endif

#ifdef ENABLE_FESTIVAL
  /*
  // replace illegal char
  replace(text.begin(), text.end(), '\\', ' ');

  // trim spaces
  while (text.size() > 0 && text[0] == ' ') {
  text.erase(0, 1);
  }
  while (text.size() > 0 && text[text.size() - 1] == ' ') {
  text.erase(text.size() - 1, 1);
  }

  if (text.empty()) {
  return NULL;
  }
  */

  // set voice
  static const char *current_voice = "voice_kal_diphone";
  static int current_samplerate = 16000;
  if (strcmp(current_voice, mEnglishVoice) != 0) {
    current_voice = mEnglishVoice;
    char cmd[256];
    strcpy(cmd + 1, mEnglishVoice);
    cmd[0] = '(';
    cmd[strlen(mEnglishVoice) + 1] = ')';
    cmd[strlen(mEnglishVoice) + 2] = 0;
    festival_eval_command(cmd);
    //festival_eval_command("(voice_kal_diphone)");
    //festival_eval_command("(voice_cmu_us_slt_arctic_hts)");

    // make English slower (only work for voice_kal_diphone)
    //festival_eval_command("(set! hts_duration_stretch 0.4)");
    //festival_eval_command("(Parameter.set 'Duration_Stretch 5)");

    if (strcmp(current_voice, "voice_kal_diphone") == 0)
      current_samplerate = 16000;
    else if (strcmp(current_voice, "voice_cmu_us_slt_arctic_hts") == 0)
      current_samplerate = 32000;
  }

  if (EkhoImpl::mDebug) {
    cerr << "Festival speak: '" << text << "' in voice of " <<
        mEnglishVoice << endl;
  }

  EST_Wave wave;
  festival_text_to_wave(text.c_str(), wave);

  if (mDict.mSfinfo.samplerate != current_samplerate) {
    wave.resample(this->mDict.mSfinfo.samplerate);
  }

  EST_TVector<short> tvector;
  wave.channel(tvector, 0);
  size = tvector.p_num_columns * 2;
  char *pPcm = new char[size];
  short *shortPcm = (short*)pPcm;
  tvector.get_values(shortPcm, 1, 0, tvector.p_num_columns);
  return pPcm;
#else
  size = 0;
  return 0;
#endif
}

int EkhoImpl::setVoice(string voice) {
  if (voice.compare(mDict.getVoice()) == 0)
    return 0;

  if (voice.compare("Cantonese") == 0 || voice.compare("yue") == 0) {
    voice = "jyutping";
    setEnglishVoice("voice_kal_diphone");
  } else if (voice.compare("Mandarin") == 0 ||
      voice.compare("zh") == 0 || voice.compare("cmn") == 0) {
    voice = "pinyin";
    setEnglishVoice("voice_kal_diphone");
    //setEnglishVoice("voice_cmu_us_slt_arctic_hts");
  } else if (voice.compare("Korean") == 0 ||
      voice.compare("ko") == 0) {
    voice = "hangul";
    setEnglishVoice("voice_kal_diphone");
    //setEnglishVoice("voice_cmu_us_slt_arctic_hts");
  } else if (voice.compare("Hakka") == 0) {
    voice = "hakka";
    setEnglishVoice("voice_kal_diphone");
  } else if (voice.compare("Tibetan") == 0 ||
      voice.compare("bo") == 0) {
    voice = "tibetan";
    setEnglishVoice("voice_kal_diphone");
    //setEnglishVoice("voice_cmu_us_slt_arctic_hts");
  } else if (voice.compare("Ngangien") == 0) {
    voice = "ngangien";
    setEnglishVoice("voice_kal_diphone");
  } else if (voice.compare("en") == 0) {
    voice = "English";
  }
  
  if (voice.find("jyutping") == 0) {
    mDict.setLanguage(CANTONESE);
  } else if (voice.find("pinyin") == 0) {
    mDict.setLanguage(MANDARIN);
  } else if (voice.find("hangul") == 0) {
    mDict.setLanguage(KOREAN);
  } else if (voice.find("hakka") == 0) {
    mDict.setLanguage(HAKKA);
  } else if (voice.find("tibetan") == 0) {
    mDict.setLanguage(TIBETAN);
  } else if (voice.find("ngangien") == 0) {
    mDict.setLanguage(NGANGIEN);
  } else if (voice.find("English") == 0) {
    mDict.setLanguage(ENGLISH);
  } else {
    cerr << "Invalid voice: " << voice << ". Fall back to Mandarin" << endl;
    mDict.setLanguage(MANDARIN);
    voice = "pinyin";
  }
  
  if (mDict.setVoice(voice.c_str()) != 0)
    return -2;
  
  this->initStream();
  
  return 0;
}

string EkhoImpl::getVoice(void) {
    return this->mDict.getVoice();
}

int EkhoImpl::speak(string text,
        void (*pCallback)(void*),
        void* pCallbackArgs) {
  this->initSound();
  this->isPaused = false;
  //this->isStopped = false;
  SpeechOrder order;
  order.text = text;
  order.pCallback = pCallback;
  order.pCallbackArgs = pCallbackArgs;
  pthread_mutex_lock(&mSpeechQueueMutex);
  mSpeechQueue.push(order);
  pthread_cond_signal(&mSpeechQueueCond);
  pthread_mutex_unlock(&mSpeechQueueMutex);

  return 0;
}

int EkhoImpl::stopAndSpeak(string text,
    void (*pCallback)(void*),
    void* pCallbackArgs) {
  this->stop();
  this->speak(text, pCallback, pCallbackArgs);
}

int EkhoImpl::blockSpeak(string text) {
#ifdef HAVE_PULSEAUDIO
  if (initSound() < 0) {
    cerr << "Fail to init sound." << endl;
    return -1;
  }
  this->isPaused = false;
  this->isStopped = false;
  SpeechOrder order;
  order.text = text;
  order.pCallback = NULL;
  order.pCallbackArgs = NULL;
  pthread_mutex_lock(&mSpeechQueueMutex);
  mSpeechQueue.push(order);
  pthread_cond_signal(&mSpeechQueueCond);
  pthread_mutex_unlock(&mSpeechQueueMutex);

  while (mSpeechQueue.size() > 0) {
    sleep(1);
  }

  int error;
  pa_simple_drain(this->stream, &error);
#endif
  return 0;
}

int EkhoImpl::pause(void) {
    if (!this->isPaused) {
        this->isPaused = true;
        return 0;
    } else {
        return 1;
    }
}

int EkhoImpl::resume(void) {
    if (this->isPaused) {
        this->isPaused = false;
        return 0;
    } else {
        return 1;
    }
}

int EkhoImpl::stop(void) {
    this->isPaused = false;
    this->isStopped = true;
    this->mPendingFrames = 0;
    pthread_mutex_lock(&mSpeechQueueMutex);
    while (not mSpeechQueue.empty()) {
        mSpeechQueue.pop();
    }
    pthread_mutex_unlock(&mSpeechQueueMutex);
    return 0;
}

void EkhoImpl::setSpeed(int tempo_delta) {
#ifdef ENABLE_SOUNDTOUCH
  if (tempo_delta >= -50 && tempo_delta <= 300) {
    this->tempoDelta = tempo_delta;
  } else {
    this->tempoDelta = 0;
  }
  this->pSoundtouch.setTempoChange(this->tempoDelta);
#else
  if (tempo_delta >= -50 && tempo_delta <= 300) {
    if (mSonicStream)
      sonicSetSpeed(mSonicStream, (float)(100 + tempo_delta) / 100);
    this->tempoDelta = tempo_delta;
  }
#endif
}

int EkhoImpl::getSpeed(void) {
  return this->tempoDelta;
}

void EkhoImpl::setPitch(int pitch_delta) {
  if (EkhoImpl::mDebug) {
    cerr << "Ekho::setPitch(" << pitch_delta << ")" << endl;
  }
#ifdef ENABLE_SOUNDTOUCH
  if (pitch_delta >= -100 && pitch_delta <= 100) {
    this->pitchDelta = pitch_delta;
  } else {
    this->pitchDelta = 0;
  }
  this->pSoundtouch.setPitchOctaves((float)this->pitchDelta / 100);
#else
  if (mSonicStream) {
//    sonicSetChordPitch(mSonicStream, 1);
    sonicSetPitch(mSonicStream, (float)(100 + pitch_delta) / 100);
  }
  this->pitchDelta = pitch_delta;
#endif
}

int EkhoImpl::getPitch(void) {
  return this->pitchDelta;
}

void EkhoImpl::setVolume(int volume_delta) {
  if (volume_delta >= -100 && volume_delta <= 100) {
    this->volumeDelta = volume_delta;
// Using sonic's setVolume doesn't work. Don't know why...
//  } else {
  if (mSonicStream)
    sonicSetVolume(mSonicStream, (float)(100 + volume_delta) / 100);    
//    this->volumeDelta = volume_delta;
  }
}

int EkhoImpl::getVolume(void) {
  return this->volumeDelta;
}

void EkhoImpl::setRate(int rate_delta) {
#ifdef ENABLE_SOUNDTOUCH
  if (rate_delta >= -50 && rate_delta <= 100) {
    this->rateDelta = rate_delta;
  } else {
    this->rateDelta = 0;
  }
  this->pSoundtouch.setRateChange(this->rateDelta);
#else
  if (mSonicStream)
    sonicSetRate(mSonicStream, (float)(100 + rate_delta) / 100);
  this->rateDelta = rate_delta;
#endif
}

int EkhoImpl::getRate(void) {
  return this->rateDelta;
}

int EkhoImpl::startServer(int port) {
  int sockfd, clientFd;  // listen on sock_fd, new connection on clientFd
  struct sockaddr_in my_addr;    // my address information
  struct sockaddr_in their_addr; // connector's address information
  unsigned int sin_size;
  //  struct sigaction sa;
  const char yes = 1;
  int numbytes;
  char buffer[BUFFER_SIZE];
  mPort = port;

#ifdef ENABLE_WIN32	
  WSADATA wsaData;   // if this doesn't work
  //WSAData wsaData; // then try this instead

  if (WSAStartup(MAKEWORD(1, 1), &wsaData) != 0) {
    fprintf(stderr, "WSAStartup failed.\n");
    exit(1);
  }
#endif

  if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    perror("socket");
    exit(1);
  }

  if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) == -1) {
    perror("setsockopt");
    exit(1);
  }

  my_addr.sin_family = AF_INET;         // host byte order
  my_addr.sin_port = htons(port);  // short, network byte order
  my_addr.sin_addr.s_addr = INADDR_ANY; // automatically fill with my IP
  memset(my_addr.sin_zero, '\0', sizeof my_addr.sin_zero);

  if (bind(sockfd, (struct sockaddr *)&my_addr, sizeof my_addr) == -1) {
    perror("bind");
    exit(1);
  }
  if (listen(sockfd, MAX_CLIENTS) == -1) {
    perror("listen");
    exit(1);
  }

#ifndef ENABLE_WIN32
  // disable SIGPIPE 
  struct sigaction act, oact;
  act.sa_handler = SIG_IGN;
  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;
  act.sa_flags |= SA_RESTART;
  if (sigaction(SIGPIPE, &act, &oact) < 0) {
    fprintf(stderr, "sigaction fail!\n");
  }
#endif

  while(1) {  // main accept() loop
    sin_size = sizeof their_addr;
#ifdef ENABLE_WIN32
    if ((clientFd = accept(sockfd, (struct sockaddr *)&their_addr, (int*)&sin_size)) == -1) {
#else
    if ((clientFd = accept(sockfd, (struct sockaddr *)&their_addr, (socklen_t*)&sin_size)) == -1) {
#endif
      perror("accept");
      continue;
    }

    if (EkhoImpl::mDebug) {
      cerr << "got connection from "
        << inet_ntoa(their_addr.sin_addr) << endl;
    }

    // process request
    if ((numbytes = recv(clientFd, buffer, BUFFER_SIZE - 1, 0)) == -1) {
      cerr << "Fail to receive request" << endl;
    }
    buffer[numbytes] = 0;

    string tmpfile;

    if (buffer[0] == SAVEOGG) {
      // get audio data in OGG format
      if (EkhoImpl::mDebug) {
        cerr << "cmd=SAVEOGG, speedDelta=" << buffer[1] << ", pitchDelta=" <<
            buffer[2] << ", volumeDelta=" << buffer[3] << ", text=" <<
            buffer + 4 << endl;
      }
      this->setSpeed(buffer[1]);
      this->setPitch(buffer[2]);
      this->setVolume(buffer[3]);
      tmpfile = genTempFilename() + ".ogg";
      this->saveOgg(buffer + 4, tmpfile);
    } else if (buffer[0] == GETPHONSYMBOLS) {
      // get phonetic symbos of text
      if (EkhoImpl::mDebug) {
        cerr << "cmd=GETPHONSYMBOLS, text=" << buffer + 1 << endl;
      }
      tmpfile = genTempFilename() + ".sym";
      list<PhoneticSymbol*> phons = mDict.lookup(buffer + 1);
      ofstream fs;
      fs.open(tmpfile.c_str());
      for (list<PhoneticSymbol*>::iterator li = phons.begin();
          li != phons.end(); ++li) {
        fs << (*li)->symbol << " ";
      }
      fs.close();
    } else {
      // get audio data in MP3 format (default)
      if (EkhoImpl::mDebug) {
        cerr << "cmd=SAVEMP3, speedDelta=" << buffer[1] << ", pitchDelta=" <<
            buffer[2] << ", volumeDelta=" << buffer[3] << ", text=" <<
            buffer + 4 << endl;
      }
      this->setSpeed(buffer[1]);
      this->setPitch(buffer[2]);
      this->setVolume(buffer[3]);
      tmpfile = genTempFilename() + ".mp3";
#ifdef HAVE_MP3LAME
      this->saveMp3(buffer + 4, tmpfile);
#endif
    }

    FILE *tmpf = fopen(tmpfile.c_str(), "rb");

    if (tmpf) {
      int size = 0;
      int total_size = 0;
      do {
        size = fread(buffer, 1, BUFFER_SIZE, tmpf);
        if (size < 0) {
          cerr << "Fail to read " << tmpfile
            << " at line" << __LINE__ << endl;
          break;
        }
        if (send(clientFd, buffer, size, 0) == -1) {
          cerr << "Fail to send " << tmpfile
            << " to client at line " << __LINE__ << endl;
          break;
        }
        total_size += size;
      } while (size == static_cast<size_t>(BUFFER_SIZE));

      fclose(tmpf);

      if (EkhoImpl::mDebug) {
        cerr << total_size << " bytes sent." << endl;
      }
    } else {
      cerr << "Fail to open " << tmpfile << endl;
    }
    
    close(clientFd);
    if (EkhoImpl::mDebug) {
      cerr << "close connection from "
        << inet_ntoa(their_addr.sin_addr) << endl;
    }

    // remove(tmpfile.c_str());
  }

  close(sockfd); // This will never be executed
  return 0;
}

// the first byte is tempo(speed) delta
int EkhoImpl::request(string ip, int port, Command cmd, string text, string outfile) {
#ifdef ENABLE_WIN32
  WSADATA wsaData;   // if this doesn't work
  //WSAData wsaData; // then try this instead

  if (WSAStartup(MAKEWORD(1, 1), &wsaData) != 0) {
    fprintf(stderr, "WSAStartup failed.\n");
    exit(1);
  }
#endif

  int sockfd;
  long numbytes;
  char buf[BUFFER_SIZE];
  struct hostent *he;
  struct sockaddr_in their_addr; // connector's address information

  if ((he = gethostbyname(ip.c_str())) == NULL) {  // get the host info 
    fprintf(stderr, "gethostbyname error\n");
    exit(1);
  }

  if ((sockfd = socket(PF_INET, SOCK_STREAM, 0)) == -1) {
    perror("socket");
    exit(1);
  }

  their_addr.sin_family = AF_INET;    // host byte order 
  their_addr.sin_port = htons(port);  // short, network byte order 
  their_addr.sin_addr = *((struct in_addr *)he->h_addr);
  memset(their_addr.sin_zero, 0, sizeof their_addr.sin_zero);

  // connect socket, retry 3 times 
  if (connect(sockfd, (struct sockaddr *)&their_addr, sizeof their_addr) == -1) {
    sleep(1);
    if (connect(sockfd, (struct sockaddr *)&their_addr, sizeof their_addr) == -1) {
      sleep(1);
      if (connect(sockfd, (struct sockaddr *)&their_addr, sizeof their_addr) == -1) {
        perror("connect");
        exit(1);
      }
    }
  }

  // set data
  char *data;
  if (cmd == GETPHONSYMBOLS) {
    data = new char[text.size() + 2];
    data[0] = cmd;
    strcpy(data + 1, text.c_str());
  } else {
    data = new char[text.size() + 5];
    data[0] = cmd;
    data[1] = (char)this->tempoDelta;
    data[2] = (char)this->pitchDelta;
    data[3] = (char)this->volumeDelta;
    strcpy(data + 4, text.c_str());
  }

  // send text
  if (send(sockfd, data, text.size() + 4, 0) == -1) {
    fprintf(stderr, "Fail to send %s\n", text.c_str());
  }

  if (EkhoImpl::mDebug) {
    cerr << "Receiving " << outfile << "..." << endl;
  }

  size_t total_size = 0;
  FILE *mp3 = fopen(outfile.c_str(), "wb");
  if (!mp3) {
    cerr << "Fail to open file " << outfile << endl;
    close(sockfd);
    return -1;
  }

  do {
    if ((numbytes = recv(sockfd, buf, BUFFER_SIZE, 0)) == -1) {
      cerr << "Fail to receive " << outfile 
        << " at line " << __LINE__ << endl;
      break;
    }
    size_t size = fwrite(buf, 1, numbytes, mp3);
    if (size != static_cast<size_t>(numbytes)) {
      cerr << "Fail to write " << outfile
        << "(" << numbytes << " -> " << size << ")" << endl;
      total_size += numbytes;
      break;
    }
    total_size += numbytes;
  } while (numbytes == BUFFER_SIZE);
  fclose(mp3);

  close(sockfd);

  if (EkhoImpl::mDebug) {
    cerr << total_size << " bytes received" << endl;
  }

  delete[] data;

  return 0;  
}

void EkhoImpl::filterSpaces(string& text) {
  bool changed = false;

  string text2;
  bool in_chinese_context = true;

  int c;
  string::iterator it = text.begin();
  string::iterator it2 = text.begin();
  string::iterator end = text.end();

  while (it != end) {
    it2 = it;
#ifdef DISABLE_EXCEPTIONS
    c = utf8::next(it, end);
#else
    try {
      c = utf8::next(it, end);
    } catch (utf8::not_enough_room &) {
      text = text2;
      return;
    } catch (utf8::invalid_utf8 &) {
      cerr << "Invalid UTF8 encoding" << endl;
      text = text2;
      return;
    }   
#endif

    if (in_chinese_context && (c == 32 || c == 12288)) {
      changed = true;
    } else {
      while (it2 != it) {
        text2.push_back(*it2);
        it2++;
      }
      in_chinese_context = (c > 128);
    }

    while (it2 != it)
      it2++;
  }   

  if (changed) {
    text = text2;
  }
}

void EkhoImpl::translatePunctuations(string& text) {
  bool changed = false;

  string text2;
  bool in_chinese_context = true;

  int c;
  string::iterator it = text.begin();
  string::iterator it2 = text.begin();
  string::iterator end = text.end();

  while (it != end) {
    it2 = it;
#ifdef DISABLE_EXCEPTIONS
    c = utf8::next(it, end);
#else
    try {
      c = utf8::next(it, end);
    } catch (utf8::not_enough_room &) {
      text = text2;
      return;
    } catch (utf8::invalid_utf8 &) {
      cerr << "Invalid UTF8 encoding" << endl;
      text = text2;
      return;
    }   
#endif

    if (in_chinese_context && mDict.isPunctuationChar(c)) {
      text2 += mDict.getPunctuationName(c);
      changed = true;
    } else {
      while (it2 != it) {
        text2.push_back(*it2);
        it2++;
      }
      in_chinese_context = (c > 128);
    }

    while (it2 != it)
      it2++;
  }   

  if (changed) {
    text = text2;
  }
}

int EkhoImpl::synth2(string text, SynthCallback *callback, void *userdata) {
#ifdef DEBUG_ANDROID
  LOGD("Ekho::synth2(%s, %p, %p) voiceFileType=%s lang=%d", text.c_str(), callback,
      userdata, mDict.mVoiceFileType, mDict.getLanguage());
#endif

  if (!userdata)
    userdata = this;
  this->isStopped = false;
  this->isPaused = false;
  float pause = 0;
  int size = 0;
  const char *pPcm = 0;

  if (mDict.getLanguage() == ENGLISH) {
#ifdef ENABLE_ENGLISH
    if (EkhoImpl::mDebug) {
      cerr << "speaking '" << text
        << "' with Festival" << endl;
    }
    pPcm = this->getPcmFromFestival(text, size);
    // output pcm data
    if (pPcm && size > 0) {
      callback((short*)pPcm, size / 2, userdata, false, true);
    }
#endif
    return 0;
  }

  // check whehter voice file available
  if (!mDict.mVoiceFileType && mDict.getLanguage() != ENGLISH) {
    cerr << "Voice file not found." << endl;
    return -1;
  }

  // filter SSML
  if (mStripSsml)
    text = stripSsml(text);

  // check punctuation
  if (mSpeakIsolatedPunctuation && text.length() <= 3) {
    const char *c = text.c_str();
    int code = utf8::next(c, c + text.length());
    if (!*c && mDict.isPunctuationChar(code))
      text = mDict.getPunctuationName(code);
  }

  // translate punctuations
  if (mPuncMode == EKHO_PUNC_ALL)
    translatePunctuations(text);

  // filter spaces
  filterSpaces(text);

#ifdef DEBUG_ANDROID
  LOGD("Ekho::synth2 filtered text=%s", text.c_str());
#endif

  list<Word> wordlist = mDict.lookupWord(text);
  list<PhoneticSymbol*>::iterator phon_symbol;
  for (list<Word>::iterator word = wordlist.begin();
      word != wordlist.end(); word++) {
    switch (word->type) {
      case FULL_PAUSE: pause += 1; break;
      case HALF_PAUSE: pause += 0.5; break;
      case QUATER_PAUSE: pause += 0.25; break;
      case PHONETIC:
        phon_symbol = word->symbols.begin();
        pPcm = (*phon_symbol)->getPcm(mDict.mVoiceFile, size);
        if (pPcm && size > 0)
          callback((short*)pPcm, size / 2, userdata, false, false);
        break;

      case ENGLISH_TEXT: 
        if (pause > 0) {
          word--; // turn back pointer
          if (pause > 1)
            pPcm = this->mDict.getFullPause()->getPcm(size);
          else if (pause >= 0.5)
            pPcm = this->mDict.getHalfPause()->getPcm(size);
          else
            pPcm = this->mDict.getQuaterPause()->getPcm(size);
          pause = 0;
          callback((short*)pPcm, size / 2, userdata, false, false);
        } else {
          char c;
          if ((word->text.length() == 1) && (c = tolower(*word->text.c_str()) &&
              c >= 'a' && c <= 'z')) {
            if (!mAlphabetPcmCache[c - 'a'])
              mAlphabetPcmCache[c - 'a'] = getPcmFromFestival(word->text, mAlphabetPcmSize[c - 'a']);

            pPcm = mAlphabetPcmCache[c - 'a'];
            size = mAlphabetPcmSize[c - 'a'];
            callback((short*)pPcm, size / 2, userdata, false, false);
          } else {
            pPcm = this->getPcmFromFestival(word->text, size);
            if (pPcm && size > 0) {
              callback((short*)pPcm, size / 2, userdata, false, false);
              if (pPcm)
                delete[] pPcm;
            }
            pPcm = 0;
          }
        }
        break;

      case NON_ENGLISH:
        if (pause > 0) {
          word--; // turn back pointer
          if (pause > 1)
            pPcm = this->mDict.getFullPause()->getPcm(size);
          else if (pause >= 0.5)
            pPcm = this->mDict.getHalfPause()->getPcm(size);
          else
            pPcm = this->mDict.getQuaterPause()->getPcm(size);
          pause = 0;
          callback((short*)pPcm, size / 2, userdata, false, false);
        } else {
          if (word->bytes) {
            PhoneticSymbol word_symbol("", word->offset, word->bytes);
            pPcm = word_symbol.getPcm(mDict.mVoiceFile, size);
            callback((short*)pPcm, size / 2, userdata, false, false);
          } else {
            // speak the word one by one
            for (list<PhoneticSymbol*>::iterator symbol = word->symbols.begin();
                symbol != word->symbols.end(); symbol++) {
#ifdef DEBUG_ANDROID
  LOGD("Ekho::synth2 speak %s", (*symbol)->symbol);
#endif
              Language lang = mDict.getLanguage();
              if (lang == MANDARIN || lang == CANTONESE) {
                pPcm = (*symbol)->getPcm(mDict.mVoiceFile, size);
                if (pPcm && size > 0)
                  callback((short*)pPcm, size / 2, userdata, false, false);
              } else {
                string path = mDict.mDataPath + "/" + mDict.getVoice();
                pPcm = (*symbol)->getPcm(path.c_str(), mDict.mVoiceFileType, size);
                if (pPcm && size > 0)
                  callback((short*)pPcm, size / 2, userdata, false, false);
              }

              // speak Mandarin for Chinese
              if (!pPcm && lang == TIBETAN) {
                string path = mDict.mDataPath + "/pinyin";
                pPcm = (*symbol)->getPcm(path.c_str(), mDict.mVoiceFileType, size);
                if (pPcm && size > 0)
                  callback((short*)pPcm, size / 2, userdata, false, false);
              }

              if (!mPcmCache)
                (*symbol)->setPcm(0, 0);
            }
          }
        }
        break;
    }
  } // end of for

  // send a signal to abort for Android
  if (userdata)
    callback(0, 0, userdata, false, false);
  else
    callback(0, 0, this, false, false);

  return 0;
}

