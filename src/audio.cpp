/***************************************************************************
 * Copyright (C) 2008-2022 by Cameron Wong                                 *
 * name in passport: HUANG GUANNENG                                        *
 * email: hgneng at gmail.com                                              *
 * website: https://eguidedog.net                                          *
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
#include <string>
#include <stdlib.h>
#include <sndfile.h>
#include <pthread.h>
#include <fstream>
#include <sys/stat.h>
#include "audio.h"
#include "ekho_impl.h"

#ifdef HAVE_MPG123
#include <mpg123.h>
#include <out123.h>
#endif

#ifdef HAVE_MP3LAME
#include <lame/lame.h>
#endif

using namespace std;

namespace ekho {
string Audio::tempDirectory = "";
bool Audio::debug = false;

Audio::Audio(void) {
  this->processorStream = NULL;
  this->pitchDelta = 0;
  this->volumeDelta = 0;
  this->rateDelta = 0;
  this->tempoDelta = 0;
  this->sampleRate = 0; // Ekho voice source sample rate
  this->currentSampleRate = 0; // sonic processing sample (change for espeak)
  this->outputSampleRate = 0; // for readShortFrames
  this->channels = 1;
  this->speechdSynthCallback = NULL;
  this->hasProcessorInited = false;
#ifdef HAVE_MPG123
  this->mpg123Handle = NULL;
#endif

#ifdef HAVE_PULSEAUDIO
  this->pulseAudio = NULL;
#endif
}

Audio::~Audio(void) {
#ifndef ENABLE_SOUNDTOUCH
  if (this->processorStream) {
    sonicDestroyStream(this->processorStream);
    this->processorStream = 0;
  }
#endif

#ifdef HAVE_MPG123
  if (this->mpg123Handle) {
    mpg123_delete(this->mpg123Handle);
    mpg123_exit();
    this->mpg123Handle = NULL;
  }
#endif
}


void Audio::initProcessor() {
  if (this->outputSampleRate == 0) {
    cerr << "Audio::initProcessor: Fail! You should setSampleRate first." << endl;
    return;
  }

  this->initProcessor(this->outputSampleRate, this->channels);
}

void Audio::initProcessor(int samplerate, int channels) {
  if (Audio::debug) {
    cerr << "[" << pthread_self() << "] Audio::initProcessor: " << samplerate << endl;
  }

  this->outputSampleRate = samplerate;
  this->channels = channels;

#ifdef ENABLE_SOUNDTOUCH
  this->pSoundtouch.setSampleRate(mDict.mSfinfo.samplerate);
  this->pSoundtouch.setChannels(1);
  this->pSoundtouch.setSetting(SETTING_USE_QUICKSEEK, 1);
#else
  this->processorStream = sonicCreateStream(samplerate, 1);
  sonicSetQuality(this->processorStream, 1); // high quality but slower
#endif

  if (this->outputSampleRate != this->sampleRate) {
    this->setPitchFloat(1);
    this->setTempoFloat(1);
  }

  this->hasProcessorInited = true;
}

void Audio::destroyProcessor() {
  if (this->processorStream) {
    sonicDestroyStream(this->processorStream);
    this->processorStream = 0;
  }
}

#ifdef HAVE_PULSEAUDIO
void Audio::initPulseAudio() {
  if (Audio::debug) {
    cerr << "[" << pthread_self() << "] Audio::initPulseAudio" << endl;
  }

  pa_sample_spec ss;
  ss.channels = this->channels; // uint8_t seems cannot output to console directly
  ss.rate = this->outputSampleRate;
  ss.format = PA_SAMPLE_S16LE;
  int error;

  if (Audio::debug) {
    cerr << "pa_sample_spec(format=" << ss.format << ",rate=" << ss.rate
         << ",channels=" << (int)ss.channels << ")" << endl;
  }

  this->pulseAudio = pa_simple_new(NULL, "Ekho", PA_STREAM_PLAYBACK, NULL,
      "playback", &ss, NULL, NULL, &error);

  if (!this->pulseAudio) {
    cerr << "pa_simple_new() failed: " << pa_strerror(error) << endl;
    cerr << "pa_sample_spec(format=" << ss.format << ",rate=" << ss.rate
         << ",channels=" << ss.channels << ")" << endl;
  }
}

void Audio::destroyPulseAudio() {
  if (this->pulseAudio) {
    // flush stream
    int error;
    if (pa_simple_drain(this->pulseAudio, &error) < 0) {
      cerr << "pa_simple_drain() failed: " << pa_strerror(error) << endl;
    }

    pa_simple_free(this->pulseAudio);
    this->pulseAudio = 0;
  }
}

void Audio::pulseAudioDrain() {
  int error;
  pa_simple_drain(this->pulseAudio, &error);
}

void Audio::pulseAudioFlush() {
  int error;
  pa_simple_flush(this->pulseAudio, &error);
}

int Audio::pulseAudioWrite(const void *buffer, size_t bytes) {
  int error;
  int ret = pa_simple_write(this->pulseAudio, buffer, bytes, &error);
  if (ret < 0) {
    cerr << "pa_simple_write failed: " << pa_strerror(error) << endl;
  }

  return ret;
}
#endif

int Audio::setTempo(int delta) {
#ifdef ENABLE_SOUNDTOUCH
  if (delta >= -50 && delta <= 300) {
    this->tempoDelta = delta;
  } else {
    this->tempoDelta = 0;
  }
  this->pSoundtouch.setTempoChange(this->tempoDelta);
#else
  if (delta >= -50 && delta <= 300) {
    this->setTempoFloat((float)(100 + delta) / 100);
    this->tempoDelta = delta;
  } else {
    cerr << "Audio::setTempo out of range: " << delta << endl;
  }
#endif

  return this->tempoDelta;
}

// 1 means no change. 2 means double speed
void Audio::setTempoFloat(float factor) {
  if (!this->processorStream) {
    cerr << "Audio::processorStream not init" << endl;
  }

  float finalFactor = factor * this->sampleRate / this->outputSampleRate;

  if (Audio::debug) {
    cerr << "Audio::setTempoFloat: " << factor <<
      ", finalFactor=" << finalFactor << endl;
  }

#ifdef DEBUG_ANDROID
  LOGD("Audio::setTempoFloat(%f) finalFactor=%f", factor, finalFactor);
#endif

  sonicSetSpeed(this->processorStream, finalFactor);
}

// 1 means no change. 2 means double high pitch
void Audio::setPitchFloat(float factor) {
  if (!this->processorStream) {
    cerr << "Audio::processorStream not init" << endl;
    return;
  }

  float finalFactor = factor * this->sampleRate / this->outputSampleRate;

  if (Audio::debug) {
    cerr << "Audio::setPitchFloat: " << factor <<
        ", finalFactor=" << finalFactor << endl;
  }

  sonicSetPitch(this->processorStream, finalFactor);
}

// 设置（英语）输入源的sample rate，sonic需要调整当前PCM流的语速，
// 让后来不同的sample rate PCM流和之前的语速一致
int Audio::setSampleRate(int rate) {
  float r = (float)rate / this->outputSampleRate;
  if (Audio::debug) {
    cerr << "Audio::setSampleRate: " << rate <<
        ", outputSampleRate: " << this->outputSampleRate <<
        ", sampleRate: " << this->sampleRate <<
        ", target rate: " << r << endl;
  }
#ifdef DEBUG_ANDROID
  LOGD("Audio::setSampleRate(%d) main samplerate=", rate, this->sampleRate);
#endif

  flushFrames();
  //sonicSetRate(this->processorStream, r);
  sonicSetRate(this->processorStream, (float)rate / this->sampleRate);
  this->currentSampleRate = rate;
  return rate;
}

void Audio::setInputSampleRate(int rate) {
  this->sampleRate = rate;
  if (this->outputSampleRate == 0) {
    this->outputSampleRate = rate;
  }
}

void Audio::setOutputSampleRate(int rate) {
  if (Audio::debug) {
    cerr << "Audio::setOutputSampleRate: " << rate << endl;
  }

  if (rate > 0) {
    this->outputSampleRate = rate;
  }
}

void Audio::setChannels(int channels) {
  if (Audio::debug) {
    cerr << "Audio::setChannels: " << channels << endl;
  }

  this->channels = channels;
}

int Audio::setPitch(int delta) {
  /*
  if (!hasProcessorInited) {
    cerr << "Audio processor has not initialized." << endl;
    return 0;
  }*/

  if (Audio::debug) {
    cerr << "Audio::setPitch: " << delta << endl;
  }

  if (delta < -100) {
    delta = -100;
  }
  if (delta > 100) {
    delta = 100;
  }
  this->pitchDelta = delta;

#ifdef ENABLE_SOUNDTOUCH
  this->pSoundtouch.setPitchOctaves((float)this->pitchDelta / 100);
#else
  if (delta < 0) {
    this->setPitchFloat(((float)delta / 2 + 100) / 100);
  } else {
    this->setPitchFloat((float)(100 + delta) / 100);
  }
#endif

  return this->pitchDelta;
}

int Audio::setVolume(int delta) {
  /*
  if (!hasProcessorInited) {
    cerr << "Audio processor has not initialized." << endl;
    return 0;
  }*/

  if (delta >= -100 && delta <= 100) {
    this->volumeDelta = delta;
    // @TODO: Using sonic's setVolume doesn't work. Don't know why...
    if (this->processorStream) {
      sonicSetVolume(this->processorStream, (float)(100 + delta) / 100);
    }
  }

  return this->volumeDelta;
}

int Audio::setRate(int delta) {
  if (Audio::debug) {
    cerr << "Audio::setRate: " << delta << endl;
  }

#ifdef ENABLE_SOUNDTOUCH
  if (delta >= -50 && delta <= 100) {
    this->rateDelta = delta;
  } else {
    this->rateDelta = 0;
  }
  this->pSoundtouch.setRateChange(this->rateDelta);
#else
  if (this->processorStream) {
    sonicSetRate(this->processorStream, (float)(100 + delta) / 100);
  }
  this->rateDelta = delta;
#endif

  return this->rateDelta;
}

int Audio::readShortFrames(short buffer[], int size) {
  if (!this->processorStream) {
    cerr << "processorStream not initialized" << endl;
    return 0;
  }
  // sonic会自动剪去一些空白的frame?

  if (this->channels == 2) {
    int monoSize = sonicReadShortFromStream(this->processorStream, buffer, size / 2);
    for (int i = monoSize; i >= 0; i--) {
      buffer[i + i + 1] = buffer[i];
      buffer[i + i] = buffer[i];
    }
    return monoSize + monoSize;
  } else {
    // mono
    return sonicReadShortFromStream(this->processorStream, buffer, size);
  }
}

int Audio::writeShortFrames(short buffer[], int size) {
  if (!this->processorStream) {
    cerr << "processorStream not initialized" << endl;
    return 0;
  }
  return sonicWriteShortToStream(this->processorStream, buffer, size);
}

void Audio::flushFrames() {
  if (!this->processorStream) {
    cerr << "processorStream not initialized" << endl;
    return;
  }
  sonicFlushStream(this->processorStream);
}

void Audio::play(const string& path) {
  //cerr << "Audio::play:" << path << endl;

  SF_INFO sfinfo;
  SNDFILE *sndfile = sf_open(path.c_str(), SFM_READ, &sfinfo);
  if (!sndfile) {
    return;
  }

  if (sfinfo.frames <= 0) {
    sf_close(sndfile);
    return;
  }

  short *pcm = new short[sfinfo.frames];
  sf_readf_short(sndfile, pcm, sfinfo.frames);

  if (this->speechdSynthCallback) {
    // @TODO: read real bits
    speechdSynthCallback(pcm, sfinfo.frames, 16, sfinfo.channels, sfinfo.samplerate, 0);
#ifdef HAVE_PULSEAUDIO
  } else if (this->pulseAudio) {
    int error;
    //cerr << "pa_simple_write" << endl;
    int ret = pa_simple_write(this->pulseAudio, pcm, sfinfo.frames * 2, &error);
    if (ret < 0) {
      cerr << "pa_simple_write failed: " << pa_strerror(error) << endl;
    }
    ret = pa_simple_drain(this->pulseAudio, &error);
    if (ret < 0) {
      cerr << "pa_simple_drain failed: " << pa_strerror(error) << endl;
    }
  } else {
    cerr << "pulseAudio not inited: audio=%p, pulseAudio=%p" << this << this->pulseAudio << endl;    
#endif    
  }

  delete[] pcm;
  sf_close(sndfile);
}

// generate temp filename
// to be improve...
string Audio::genTempFilename() {
  string tempFilePath;
  if (Audio::tempDirectory.empty()) {
#ifdef ENABLE_WIN32
    tempFilePath = "\\TEMP\\ekho";
#else
    tempFilePath = "/tmp/ekho";
#endif
  } else {
    tempFilePath = Audio::tempDirectory;
  }

  static int count = 0;
  count++;
  tempFilePath.append(to_string(count));

  return tempFilePath;
}

void Audio::setTempDirectory(string dir) {
  Audio::tempDirectory = dir;
  struct stat st;
  if (stat(dir.c_str(), &st) == 0) {
    if ((st.st_mode & S_IFDIR) != 0) {
#ifdef DEBUG_ANDROID
      LOGI("%s exitsts", dir.c_str());
#endif
    }
  } else {
    const int dir_err = mkdir(dir.c_str(), S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
#ifdef DEBUG_ANDROID
    if (-1 == dir_err) {
      LOGI("Error creating directory: %s", dir.c_str());
    } else {
      LOGI("%s created", dir.c_str());
    }
#else
    if (-1 == dir_err) {
      cerr << "Error creating directory: %s" << dir.c_str() << endl;
    }
#endif
  }
}

// It's caller's responsibility to delete return short space
short* Audio::readPcmFromAudioFile(string filepath, int& size) {
  if (filepath.back() == '3') {
    // SNDFILE默认配置下不支持MP3: http://www.mega-nerd.com/libsndfile/FAQ.html#Q020
    return this->readPcmFromMp3File(filepath, size);
  }

  SF_INFO sfinfo;
  SNDFILE *sndfile = sf_open(filepath.c_str(), SFM_READ, &sfinfo);
  if (!sndfile) {
    cerr << "Fail to open " << filepath << endl;
    return NULL;
  }

  if (sfinfo.frames <= 0) {
    cerr << filepath << " is empty." << endl;
    sf_close(sndfile);
    return NULL;
  }

  short *pcm = new short[sfinfo.frames];
  sf_readf_short(sndfile, pcm, sfinfo.frames);
  sf_close(sndfile);
  size = sfinfo.frames;

  return pcm;
}

void Audio::initMp3Processor() {
#ifdef HAVE_MPG123
  if (!mpg123Handle) {
    mpg123_init();
    int err;
    this->mpg123Handle = mpg123_new(NULL, &err);
  }
#endif
}

// It's caller's responsibility to delete return short space
short* Audio::readPcmFromMp3File(string filepath, int& size) {
#ifdef HAVE_MPG123
  this->initMp3Processor();

  size_t blockSize = mpg123_outblock(this->mpg123Handle);
  size_t bufferSize = blockSize;
  unsigned char* buffer = (unsigned char*)malloc(bufferSize * sizeof(unsigned char));
  unsigned char* bufferPtr = buffer;
  int channels, encoding;
  long rate;

  mpg123_open(this->mpg123Handle, filepath.c_str());
  mpg123_getformat(this->mpg123Handle, &rate, &channels, &encoding);
  size_t readBytes = 0;
  size_t totalBytes = 0;
  int ret = 0;

  while ((ret = mpg123_read(this->mpg123Handle, bufferPtr, blockSize, &readBytes)) != MPG123_DONE) {
    if (readBytes != blockSize) {
      cerr << "something wrong with mpg123_read: readBytes=" <<
       readBytes << ", blockSize=" << blockSize << ", ret=" << ret <<
       ", MPG123_DONE=" << MPG123_DONE << endl;
    }
    bufferSize += blockSize;
    buffer = (unsigned char*)realloc(buffer, bufferSize * sizeof(unsigned char*));
    bufferPtr = buffer + blockSize;
    totalBytes += readBytes;
  }
  mpg123_close(this->mpg123Handle);
  totalBytes += readBytes;
  size = totalBytes / 2;
  // cerr << "size: " << size << endl;

  return (short*)buffer;
#else
  return 0;
#endif
}

int EkhoImpl::saveWav(string text, string filename) {
  initEnglish();
  if (EkhoImpl::mDebug) {
    cerr << "Writting WAV file " << filename << " ..." << endl;
  }

  // open record file
  SF_INFO sfinfo;
  memcpy(&sfinfo, &mDict.mSfinfo, sizeof(SF_INFO));
  sfinfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
  sfinfo.samplerate = this->audio->outputSampleRate;
  sfinfo.channels = this->audio->channels;

  if (EkhoImpl::mDebug) {
    cerr << "sfinfo format: samplerate=" << sfinfo.samplerate
         << ", channel=" << sfinfo.channels;
    cerr.setf(ios::hex);
    cerr << ", format=" << sfinfo.format << endl;
    cerr.unsetf(ios::hex);
  }
  mSndFile = sf_open(filename.c_str(), SFM_WRITE, &sfinfo);
  if (!mSndFile) {
    cerr << "Fail to open file " << filename << " at " << __LINE__ << endl;
  }

  synth2(text, writePcm);
  finishWritePcm();

  // close record file
  sf_close(mSndFile);

  if (EkhoImpl::mDebug) cerr << "Finish writting WAV file " << filename << endl;

  return 0;
}

int EkhoImpl::saveOgg(string text, string filename) {
  initEnglish();
  if (EkhoImpl::mDebug) {
    cerr << "Writting OGG file " << filename << " ..." << endl;
  }

  // open record file
  SF_INFO sfinfo;
  memcpy(&sfinfo, &mDict.mSfinfo, sizeof(SF_INFO));
  sfinfo.format = SF_FORMAT_OGG | SF_FORMAT_VORBIS;
  if (EkhoImpl::mDebug) {
    cerr << "sfinfo format: samplerate=" << sfinfo.samplerate
         << ", channel=" << sfinfo.channels;
    cerr.setf(ios::hex);
    cerr << ", format=" << sfinfo.format << endl;
    cerr.unsetf(ios::hex);
  }
  mSndFile = sf_open(filename.c_str(), SFM_WRITE, &sfinfo);
  if (!mSndFile) {
    cerr << "Fail to open file " << filename << " at " << __LINE__ << endl;
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

#ifdef HAVE_MP3LAME
int EkhoImpl::saveMp3(string text, string filename) {
  FILE *musicin;
  lame_global_flags *gf;
  FILE *outf;

  string tmp_wav = Audio::genTempFilename() + ".wav";
  this->saveWav(text, tmp_wav);

  /* open the input file */
  // int pcmswapbytes = 0;  // don't swap bytes

  /* Try to open the sound file */
  SF_INFO gs_wfInfo;
  memset(&gs_wfInfo, 0, sizeof(gs_wfInfo));
  SNDFILE *gs_pSndFileIn = sf_open(tmp_wav.c_str(), SFM_READ, &gs_wfInfo);
  if (gs_pSndFileIn == NULL) {
    sf_perror(gs_pSndFileIn);
    cerr << "Could not open sound file " << tmp_wav << endl;
    return -1;
  }
  musicin = (FILE *)gs_pSndFileIn;

  /* initialize libmp3lame */
  if (NULL == (gf = lame_init())) {
    cerr << "fatal error during initialization" << endl;
    return -1;
  }

  (void)lame_set_num_samples(gf, gs_wfInfo.frames);
  if (-1 == lame_set_num_channels(gf, gs_wfInfo.channels)) {
    cerr << "Unsupported number of channels: " << gs_wfInfo.channels << endl;
    return -1;
  }

  (void)lame_set_in_samplerate(gf, gs_wfInfo.samplerate);
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
  do {
    /* read in 'iread' samples */
    int num_channels = lame_get_num_channels(gf);
    int insamp[2 * BUFFER_SIZE];
    int framesize;
    int samples_to_read;
    int i;
    int *p;

    samples_to_read = framesize = lame_get_framesize(gf);
    if (framesize > BUFFER_SIZE) {
      cerr << "framesize: " << framesize << endl;
      return -1;
    }

    /* get num_samples */
    //unsigned int tmp_num_samples = lame_get_num_samples(gf);

    samples_read =
        sf_read_int((SNDFILE *)musicin, insamp, num_channels * samples_to_read);

    if (samples_read < 0) {
      return samples_read;
    }
    p = insamp + samples_read;
    samples_read /= num_channels;
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

    if (samples_read >= 0) {
      /* encode */
      imp3 = lame_encode_buffer_int(gf, Buffer[0], Buffer[1], samples_read,
                                    mp3buffer, sizeof(mp3buffer));
      if (imp3 < 0) {
        if (imp3 == -1) {
          cerr << "mp3 buffer is not big enough... " << endl;
        } else {
          cerr << "mp3 internal error:  error code=" << imp3 << endl;
        }
        return -1;
      }
      owrite = (int)fwrite(mp3buffer, 1, imp3, outf);
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
  imp3 = lame_encode_buffer_int(gf, Buffer[0], Buffer[1], BUFFER_SIZE,
                                mp3buffer, sizeof(mp3buffer));
  fwrite(mp3buffer, 1, imp3, outf);

  imp3 = lame_encode_flush(
      gf, mp3buffer, sizeof(mp3buffer)); /* may return one more mp3 frame */

  if (imp3 < 0) {
    if (imp3 == -1) {
      cerr << "mp3 buffer is not big enough... " << endl;
    } else {
      cerr << "mp3 internal error:  error code=" << imp3 << endl;
    }
    return -1;
  }

  owrite = (int)fwrite(mp3buffer, 1, imp3, outf);
  if (owrite != imp3) {
    cerr << "Error writing mp3 output" << endl;
    return -1;
  }

  fclose(outf); /* close the output file */
  if (sf_close((SNDFILE *)musicin) != 0) {
    cerr << "Could not close sound file" << endl;
  }
  lame_close(gf);

  // remove(tmp_wav.c_str());

  if (EkhoImpl::mDebug) {
    cerr << "Finish writing MP3 file " << filename << endl;
  }

  return 0;
}
#endif
}