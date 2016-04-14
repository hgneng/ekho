/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                         Copyright (c) 1996                            */
/*                        All Rights Reserved.                           */
/*                                                                       */
/*  Permission is hereby granted, free of charge, to use and distribute  */
/*  this software and its documentation without restriction, including   */
/*  without limitation the rights to use, copy, modify, merge, publish,  */
/*  distribute, sublicense, and/or sell copies of this work, and to      */
/*  permit persons to whom this work is furnished to do so, subject to   */
/*  the following conditions:                                            */
/*   1. The code must retain the above copyright notice, this list of    */
/*      conditions and the following disclaimer.                         */
/*   2. Any modifications must be clearly marked as such.                */
/*   3. Original authors' names are not deleted.                         */
/*   4. The authors' names are not used to endorse or promote products   */
/*      derived from this software without specific prior written        */
/*      permission.                                                      */
/*                                                                       */
/*  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        */
/*  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      */
/*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   */
/*  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     */
/*  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   */
/*  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          */
/*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       */
/*  THIS SOFTWARE.                                                       */
/*                                                                       */
/*************************************************************************/
/*                  Author :  Alan Black and Paul Taylor                 */
/*                  Date   :  June 1996                                  */
/*-----------------------------------------------------------------------*/
/*   File I/O functions specific to various file formats                 */
/*                                                                       */
/*   Note that internally data will always be shorts and                 */
/*   native byte order, conversions to/from other byte formats           */
/*   or encodings happend at read/write time                             */
/*                                                                       */
/*=======================================================================*/
#include <cstdlib> 
#include <cstdio>
#include "EST_unix.h"
#include <cstring>
#include "EST_wave_aux.h"
#include "EST_wave_utils.h"
#include "EST_strcasecmp.h"
#include "waveP.h"
#include "EST_FileType.h"

static int def_load_sample_rate = 16000;

/*************************************************************************/
/*                                                                       */
/* Functions specific for each file format                               */
/*                                                                       */
/*************************************************************************/

/*=======================================================================*/
/* Sphere Nist files                                                     */
/*=======================================================================*/

static const char *NIST_SIG = "NIST_1A\n   1024\n";
static const char *NIST_END_SIG = "end_head\n";
#define NIST_HDR_SIZE 1024

int nist_get_param_int(const char *hdr, const char *field, int def_val)
{
    char *p;
    int val;

    if (((p=strstr((char *)hdr,field)) != NULL) &&
	(strncmp(" -i ",p+strlen(field),4) == 0))
    {
	sscanf(p+strlen(field)+4,"%d",&val);
	return val;
    }
    else
	return def_val;

}

char *nist_get_param_str(const char *hdr, const char *field, const char *def_val)
{
    char *p,*val;
    int size;

    if (((p=strstr((char *)hdr,field)) != NULL) &&
	(strncmp(" -s",p+strlen(field),3) == 0))
    {
	sscanf(p+strlen(field)+3,"%d",&size);
	val = walloc(char,size+1);
	/* Hmm don't know how long the %d is so do it again */
	sscanf(p+strlen(field)+3,"%d %s",&size,val);
	return val;
    }
    else
	return wstrdup(def_val);


}

const char *sample_type_to_nist(enum EST_sample_type_t sample_type)
{
    const char *c;
    switch (sample_type) {
    case st_unknown:  
	c = ""; break;
    case st_schar:  
	c = "PCM-1"; break;
    case st_mulaw:
	c = "ULAW"; break;
    case st_short: 
	c = "pcm"; break;
    case st_int:   
	c = "PCM-4"; break;
    case st_float:
	c = "REAL"; break;
      case st_double:
	c = "REAL"; break;
    default:
	fprintf(stderr,"Unknown sample type for nist");
	c = "";
    }
    return c;
}

enum EST_sample_type_t nist_to_sample_type(char *type)
{
    if ((streq(type,"pcm")) ||
	(streq(type,"PCM")) ||
	(streq(type,"pcm-2")))
	return st_short;
    if (strcmp(type,"pcm,embedded-shorten-v1.1") == 0)
	return st_shorten;
    else if ((EST_strcasecmp(type,"ULAW",NULL) == 0) ||
	     (EST_strcasecmp(type,"U-LAW",NULL) == 0) ||
	     (EST_strcasecmp(type,"mu-law",NULL) == 0) ||
	     (EST_strcasecmp(type,"mulaw",NULL) == 0))
	return st_mulaw;
    else if (strcmp(type,"alaw") == 0)
	return st_alaw;
    else if (strcmp(type,"PCM-1") == 0)
	return st_schar;
    else if (strcmp(type,"PCM-4") == 0)
	return st_int;
    else if (strcmp(type,"REAL") == 0)
	return st_float;
    else

    {
	fprintf(stderr,"NIST: unknown sample type: %s\n",type);
	return st_unknown;
    }
}

enum EST_read_status load_wave_nist(EST_TokenStream &ts, short **data, int
		       *num_samples, int *num_channels, int
		       *word_size, int *sample_rate, enum
		       EST_sample_type_t *sample_type, int *bo , int
		       offset, int length)

{
    char header[NIST_HDR_SIZE];
    int samps,sample_width,data_length,actual_bo;
    unsigned char *file_data;
    enum EST_sample_type_t actual_sample_type;
    char *byte_order, *sample_coding;
    int n;
    int current_pos;

    current_pos = ts.tell();
    if (ts.fread(header,NIST_HDR_SIZE,1) != 1)
	return misc_read_error;

    if (strncmp(header,NIST_SIG,sizeof(NIST_SIG)) != 0)
	return wrong_format;

    samps = nist_get_param_int(header,"sample_count",-1);
    *num_channels = nist_get_param_int(header,"channel_count",1);
    sample_width = nist_get_param_int(header,"sample_n_bytes",2);
    *sample_rate = 
	nist_get_param_int(header,"sample_rate",def_load_sample_rate);
    byte_order = nist_get_param_str(header,"sample_byte_format",
				    (EST_BIG_ENDIAN ? "10" : "01"));
    sample_coding = nist_get_param_str(header,"sample_coding","pcm");
    if (streq(byte_order,"mu-law"))
    {
	byte_order = wstrdup((EST_BIG_ENDIAN ? "10" : "01"));
	sample_coding = wstrdup("ULAW");
    }

    /* code for reading in Tony Robinson's shorten files.
       This is a temporary fix which calls the unshorten program on the
       speech file and reads in the answer by calling this function.
       It would be nice to have a simple library routine which did the
       unshortening.
       */
    
    if (streq(sample_coding,"pcm,embedded-shorten-v1.1"))
    {
	char *tmpfile, *cmdstr;
	enum EST_read_status rval;

	tmpfile = cmake_tmp_filename();
	cmdstr = walloc(char,strlen(tmpfile)+200);
	sprintf(cmdstr,"cstrshorten %s %s",
		(const char*)ts.filename(),tmpfile);
	printf("Command: %s\n", cmdstr);
	system(cmdstr);
	EST_TokenStream tt;
	tt.open(tmpfile);
	
	rval = load_wave_nist(tt, data, num_samples,
			      num_channels, word_size, sample_rate,
			      sample_type, bo, offset, length);
	unlink(tmpfile);
	wfree(tmpfile);
	wfree(cmdstr);
	tt.close();
	return rval;
    }

    if (length == 0)
	data_length = (samps - offset)*(*num_channels);
    else
	data_length = length*(*num_channels);

    file_data = walloc(unsigned char,sample_width * data_length);

    ts.seek(current_pos+NIST_HDR_SIZE+(sample_width*offset*(*num_channels)));

    n = ts.fread(file_data,sample_width,data_length);

    if ((n < 1) && (n != data_length))
    {
	wfree(file_data); 
	wfree(sample_coding);
	wfree(byte_order);
	return misc_read_error;
    }
    else if ((n < data_length) && (data_length/(*num_channels) == n))
    {
	fprintf(stderr,"WAVE read: nist header is (probably) non-standard\n");
	fprintf(stderr,"WAVE read: assuming different num_channel interpretation\n");
	data_length = n;   /* wrongly headered file */
    }
    else if (n < data_length)
    {
	fprintf(stderr,"WAVE read: short file %s\n",
		(const char *)ts.filename());
	fprintf(stderr,"WAVE read: at %d got %d instead of %d samples\n",
		offset,n,data_length);
	data_length = n;
    }

    actual_sample_type = nist_to_sample_type(sample_coding);
    actual_bo = ((strcmp(byte_order,"10") == 0) ? bo_big : bo_little);

    *data = convert_raw_data(file_data,data_length,
			     actual_sample_type,actual_bo);

    *num_samples = data_length/ (*num_channels);
    *sample_type = st_short;
    *bo = EST_NATIVE_BO;
    *word_size = 2;
    wfree(sample_coding);
    wfree(byte_order);

    return format_ok;
}

enum EST_write_status save_wave_nist(FILE *fp, const short *data, int offset,
				     int num_samples, int num_channels, 
				     int sample_rate,
				     enum EST_sample_type_t sample_type, int bo)   
{
    char h[1024], p[1024];
    const char *t;
    
    memset(h,0,1024);
    
    strcat(h, NIST_SIG);
    sprintf(p, "channel_count -i %d\n", num_channels);
    strcat(h, p);
    sprintf(p, "sample_count -i %d\n", num_samples);	
    strcat(h, p);
    sprintf(p, "sample_rate -i %d\n", sample_rate);	
    strcat(h, p);
    
    t = sample_type_to_nist(sample_type);
    if (t)
    {
	sprintf(p, "sample_coding -s%d %s\n", (signed)strlen(t), t);  
	strcat(h, p);
	sprintf(p, "sample_n_bytes -i %d\n", get_word_size(sample_type));
	strcat(h, p);
    }
    
    if (get_word_size(sample_type) > 1)
    {
	sprintf(p, "sample_byte_format -s%d %s\n", 2, 
		((bo == bo_big) ? "10" : "01"));
	strcat(h, p);
    }
    
    strcat(h, NIST_END_SIG);
    /*makes it nice to read */
    strcat(h, "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"); 
    
    if (fwrite(&h, 1024, 1, fp) != 1)
	return misc_write_error;
    
    return save_raw_data(fp,data,offset,num_samples,num_channels,
			 sample_type,bo);
    
}

/*=======================================================================*/
/* EST's own format                                                      */
/*=======================================================================*/

enum EST_read_status load_wave_est(EST_TokenStream &ts, short **data, int
				   *num_samples, int *num_channels, int
				   *word_size, int *sample_rate, enum
				   EST_sample_type_t *sample_type, int *bo, 
				   int offset, int length)
{
    int data_length, actual_bo;
    short *file_data;
    EST_String byte_order;
    int n;
    EST_EstFileType t;
    EST_Option hinfo;
    bool ascii;
    EST_read_status r;
    EST_sample_type_t actual_sample_type;
    
    offset = 0;
    
    if ((r = read_est_header(ts, hinfo, ascii, t)) != format_ok)
	return r;
    if (t != est_file_wave)
	return misc_read_error;
    
    *num_samples = hinfo.ival("NumSamples");
    *num_channels = hinfo.ival("NumChannels");
    *sample_rate = hinfo.ival("SampleRate");
    
    byte_order = hinfo.val("ByteOrder");
    
    if (length == 0)
	data_length = (*num_samples)*(*num_channels);
    else
	data_length = length*(*num_channels);
    
    file_data = walloc(short, data_length);
    
    n = ts.fread(file_data, sizeof(short), data_length);
    if ((n != data_length) && (n < 1))
    {
	cerr << "EST wave load: " << ts.pos_description() << endl;
	cerr << "failed to read file\n";
	wfree(file_data); 
	return misc_read_error;
    }
    else if (n != data_length)
    {
	cerr << "Wrong number of samples/channels in EST wave file\n";  
	cerr << ts.pos_description() << " ";
	cerr << "expected " << data_length << " got " << n << endl;
	data_length = n;
    }
    
    actual_bo = (byte_order == "10") ? bo_big : bo_little;
    if (hinfo.present("SampleType"))
	actual_sample_type = str_to_sample_type(hinfo.val("SampleType"));
    else
	actual_sample_type = st_short; // some older files don't have this
    
    *data = convert_raw_data((unsigned char *)file_data, 
			     data_length, actual_sample_type, actual_bo);
    // because internally data is always shorts
    *sample_type = st_short;
    *bo = EST_NATIVE_BO;
    *word_size = 2;
    
    return format_ok;
}

enum EST_write_status save_wave_est(FILE *fp, const short *data, int offset,
				       int num_samples, int num_channels, 
				       int sample_rate,
				       enum EST_sample_type_t sample_type, int bo)   
{
    fprintf(fp, "EST_File wave\n");
    fprintf(fp, "DataType binary\n");
    fprintf(fp, "SampleRate %d\n", sample_rate);
    fprintf(fp, "NumSamples %d\n", num_samples);
    fprintf(fp, "NumChannels %d\n", num_channels);
    fprintf(fp, "SampleType %s\n", sample_type_to_str(sample_type));
    if (get_word_size(sample_type) > 1)
	fprintf(fp, "ByteOrder %s\n", ((bo == bo_big) ? "10" : "01"));
    
    fprintf(fp, "EST_Header_End\n");
    
    return save_raw_data(fp, data, offset, num_samples, num_channels,
			 sample_type, bo);
    
}

/*=======================================================================*/
/*  Microsoft RIFF (.wav) audio files                                    */
/*                                                                       */
/*  The information on this format was gained by reading a document      */
/*  found on the net called "Multimedia Programming Interface and        */
/*  Data Specification v1.0" and by looking at Rick Richardson,          */
/*  Lance Norskog And Sundry Contributors code in SOX.  All this code    */
/*  is rewritten from scratch though, but I couldn't do it without       */
/*  other's explanations. I would have used the SOX code directly but    */
/*  was not really in the right form so starting again was easier        */
/*=======================================================================*/
#define WAVE_FORMAT_PCM    0x0001
#define WAVE_FORMAT_ADPCM  0x0002
#define WAVE_FORMAT_ALAW   0x0006
#define WAVE_FORMAT_MULAW  0x0007

enum EST_read_status load_wave_riff(EST_TokenStream &ts, short **data, int
				    *num_samples, int *num_channels, int
				    *word_size, int *sample_rate, enum
				    EST_sample_type_t *sample_type, int *bo , int
				    offset, int length)
{
    char info[4];
    int samps,sample_width,data_length;
    short shortdata;
    int dsize,intdata;
    unsigned char *file_data;
    enum EST_sample_type_t actual_sample_type;
    
    if (ts.fread(info,sizeof(char),4) != 4)
	return wrong_format;	/* its almost definitely an error */
    if (strncmp(info,"RIFF",4) != 0)
	return wrong_format;

    /* We've got a riff file */
    ts.fread(&dsize,4,1);
    /* .wav files are always little endian */
    if (EST_BIG_ENDIAN) dsize = SWAPINT(dsize);
    if ((ts.fread(info,sizeof(char),4) != 4) ||
	(strncmp(info,"WAVE",4) != 0))
    {
	fprintf(stderr, "RIFF file is not of type WAVE\n");
	return misc_read_error;	/* not a wave file */
    }
    if ((ts.fread(info,sizeof(char),4) != 4) ||
	(strncmp(info,"fmt ",4) != 0))
	return misc_read_error;	/* something else wrong */

    ts.fread(&dsize,4,1);
    if (EST_BIG_ENDIAN) dsize = SWAPINT(dsize);
    ts.fread(&shortdata,2,1);
    if (EST_BIG_ENDIAN) shortdata = SWAPSHORT(shortdata);

    switch (shortdata)
    {
	/* This is a non-proprietary format */
    case WAVE_FORMAT_PCM:
	actual_sample_type = st_short; break;
	/* The follow are registered proprietary WAVE formats  (?) */
    case WAVE_FORMAT_MULAW:
	actual_sample_type = st_mulaw; break;
    case WAVE_FORMAT_ADPCM:
	fprintf(stderr, "RIFF file: unsupported proprietary sample format ADPCM\n"); 
	actual_sample_type = st_short;
	break;
	/*	  actual_sample_type = st_adpcm; break; */ /* yes but which adpcm ! */
    case WAVE_FORMAT_ALAW:
    default:
	fprintf(stderr, "RIFF file: unknown sample format\n");
	actual_sample_type = st_short;
	/*	return misc_read_error; */
    }
    ts.fread(&shortdata,2,1);
    if (EST_BIG_ENDIAN) shortdata = SWAPSHORT(shortdata);
    *num_channels = shortdata;
    ts.fread(sample_rate,4,1);
    if (EST_BIG_ENDIAN) *sample_rate = SWAPINT(*sample_rate);
    ts.fread(&intdata,4,1);	/* average bytes per second -- ignored */
    if (EST_BIG_ENDIAN) intdata = SWAPINT(intdata);
    ts.fread(&shortdata,2,1);	/* block align ? */
    if (EST_BIG_ENDIAN) shortdata = SWAPSHORT(shortdata);
    ts.fread(&shortdata,2,1);
    if (EST_BIG_ENDIAN) shortdata = SWAPSHORT(shortdata);

    sample_width = (shortdata+7)/8;
    if ((sample_width == 1) && (actual_sample_type == st_short))
	actual_sample_type = st_uchar; /* oops I meant 8 bit */

    ts.seek((dsize-16)+ts.tell());     /* skip rest of header */
    while (1)
    {
	if (ts.fread(info,sizeof(char),4) != 4)
	{
	    fprintf(stderr,"RIFF file truncated\n");
	    return misc_read_error;	/* something else wrong */
	}
	if (strncmp(info,"data",4) == 0)
	{
	    ts.fread(&samps,4,1);
	    if (EST_BIG_ENDIAN) samps = SWAPINT(samps);
	    samps /= (sample_width*(*num_channels));
	    break;
	}
	else if (strncmp(info,"fact",4) == 0)
	{			/* some other type of chunk -- skip it */
	    ts.fread(&samps,4,1);
	    if (EST_BIG_ENDIAN) samps = SWAPINT(samps);
	    ts.seek(samps+ts.tell());	/* skip rest of header */
	    /* Hope this is the right amount */
	}
	else
	{
            //	    fprintf(stderr,"Ignoring unsupported chunk type \"%c%c%c%c\" in RIFF file\n",
            //    info[0],info[1],info[2],info[3]);
	    //return misc_read_error;
	    ts.fread(&dsize,4,1);
	    if (EST_BIG_ENDIAN) dsize = SWAPINT(dsize);
	    ts.seek(dsize+ts.tell());     /* skip this chunk */
	}
    }
    if (length == 0)
	data_length = (samps - offset)*(*num_channels);
    else
	data_length = length*(*num_channels);
    
    file_data = walloc(unsigned char,sample_width * data_length);
    
    ts.seek((sample_width*offset*(*num_channels))+ts.tell());
    if ((dsize=ts.fread(file_data,sample_width,data_length)) != data_length)
    {
	/*  It seems so many WAV files have their datasize wrong I'll */
	/*  let it through -- I think SOX is a major culprit          */
	if (length == 0) /* the file did the identification */
	    fprintf(stderr,"Unexpected end of file but continuing (apparently missing %d samples)\n",data_length-dsize);
	else
	{
	    fprintf(stderr,"Unexpected end of file: (missing %d samples)\n",data_length-dsize);
	    wfree(file_data);
	    return misc_read_error;
	}
    }
    
    *data = convert_raw_data(file_data,dsize,
			     actual_sample_type, bo_little);
    
    *num_samples = dsize / (*num_channels);
    *sample_type = st_short;
    *bo = EST_NATIVE_BO;
    *word_size = 2;
    
    return format_ok;
}

enum EST_write_status save_wave_riff(FILE *fp, const short *data, int offset,
				     int num_samples, int num_channels, 
				     int sample_rate,
				     enum EST_sample_type_t sample_type, int bo)   
{
    (void)bo;
    const char *info;
    int data_size, data_int;
    short data_short;

    if (sample_type == st_schar)
      {
	EST_warning("RIFF format: Signed 8-bit not allowed by this file format");
	sample_type=st_uchar; 
      }
    
    info = "RIFF"; fwrite(info,4,1,fp);
    data_size = num_channels*num_samples*get_word_size(sample_type)+ 8+16+12;
    /* WAV files are always LITTLE_ENDIAN (i.e. intel x86 format) */
    if (EST_BIG_ENDIAN) data_size = SWAPINT(data_size);
    fwrite(&data_size,1,4,fp);	/* total number of bytes in file */
    info = "WAVE"; fwrite(info,4,1,fp);
    info = "fmt "; fwrite(info,4,1,fp);
    data_size = 16;
    if (EST_BIG_ENDIAN) data_size = SWAPINT(data_size);
    fwrite(&data_size,1,4,fp);	/* size of header */
    switch (sample_type)
    {
    case st_short:  data_short = WAVE_FORMAT_PCM; break;
    case st_uchar: data_short = WAVE_FORMAT_PCM; break;
    case st_mulaw: data_short = WAVE_FORMAT_MULAW; break;
    case st_alaw: data_short = WAVE_FORMAT_ALAW; break;
    case st_adpcm: data_short = WAVE_FORMAT_ADPCM; break;
    default:
      fprintf(stderr,"RIFF format: unsupported data format %d\n",
	      sample_type);
      return misc_write_error;
	       }
    if (EST_BIG_ENDIAN) data_short = SWAPSHORT(data_short);
    fwrite(&data_short,1,2,fp);	/* sample type */
    data_short = num_channels;
    if (EST_BIG_ENDIAN) data_short = SWAPSHORT(data_short);
    fwrite(&data_short,1,2,fp);	/* number of channels */
    data_int = sample_rate;
    if (EST_BIG_ENDIAN) data_int = SWAPINT(data_int);
    fwrite(&data_int,1,4,fp);	/* sample rate */
    data_int = sample_rate * num_channels * get_word_size(sample_type);
    if (EST_BIG_ENDIAN) data_int = SWAPINT(data_int);
    fwrite(&data_int,1,4,fp);	/* Average bytes per second */
    data_short = num_channels * get_word_size(sample_type);
    if (EST_BIG_ENDIAN) data_short = SWAPSHORT(data_short);
    fwrite(&data_short,1,2,fp);	/* block align */
    data_short = get_word_size(sample_type) * 8;
    if (EST_BIG_ENDIAN) data_short = SWAPSHORT(data_short);
    fwrite(&data_short,1,2,fp);	/* bits per sample */
    info = "data"; fwrite(info,4,1,fp);
    data_size = num_channels*num_samples*get_word_size(sample_type);
    if (EST_BIG_ENDIAN) data_size = SWAPINT(data_size);
    fwrite(&data_size,1,4,fp);	/* total number of bytes in data */
    
    return save_raw_data(fp,data,offset,num_samples,num_channels,
			 sample_type,bo_little);
}

/*=======================================================================*/
/* Amiga/Apple AIFF waveform format                                      */
/* This was constructed using info in AudioIFF1.3.hqx found on the web   */
/* and also I did look at SOX's aiff.c written by Guido van Rossum       */
/* and Sundry Contributors.                                              */
/*=======================================================================*/

struct AIFFchunk {
    char id[4];
    int size;
};

struct AIFFssnd {		/* Sound Data Chunk */
    int offset;
    int blocksize;
};

enum EST_read_status load_wave_aiff(EST_TokenStream &ts, short **data, int
				    *num_samples, int *num_channels, int
				    *word_size, int *sample_rate, enum
				    EST_sample_type_t *sample_type, int *bo , int
				    offset, int length)
{
    char info[4];
    struct AIFFchunk chunk;
    short comm_channels;
    int comm_samples;
    short comm_bits;
    unsigned char ieee_ext_sample_rate[10];
    struct AIFFssnd ssndchunk;
    enum EST_sample_type_t actual_sample_type;
    int dsize,data_length,n;
    unsigned char *file_data;
    
    if (ts.fread(info,sizeof(char),4) != 4)
	return wrong_format;	/* but its almost definitely an error */
    if (strncmp(info,"FORM",4) != 0)
	return wrong_format;

    /* We've got an aiff file, I hope */
    ts.fread(&dsize,4,1);
    if (EST_LITTLE_ENDIAN)	/* file is in different byte order */
	dsize = SWAPINT(dsize);
    if ((ts.fread(info,sizeof(char),4) != 4) ||
	(strncmp(info,"AIFF",4) != 0))
    {
	fprintf(stderr, "AIFF file does not have AIFF chunk\n");
	return misc_read_error; 
    }
    
    for ( ; ts.fread(&chunk,1,sizeof(chunk)) == sizeof(chunk) ; )
    {				/* for each chunk in the file */
	if (EST_LITTLE_ENDIAN)	/* file is in different byte order */
	    chunk.size = SWAPINT(chunk.size);
	if (strncmp(chunk.id,"COMM",4) == 0)
	{
	    if (chunk.size != 18)
	    {
		fprintf(stderr,"AIFF chunk: bad size\n");
		return misc_read_error;
	    }
	    ts.fread(&comm_channels,1,sizeof(short));
	    ts.fread(&comm_samples,1,sizeof(int));
	    ts.fread(&comm_bits,1,sizeof(short));
	    if (ts.fread(ieee_ext_sample_rate,1,10) != 10)
	    {
		fprintf(stderr,"AIFF chunk: eof within COMM chunk\n");
		return misc_read_error;
	    }
	    if (EST_LITTLE_ENDIAN)
	    {
		comm_channels = SWAPSHORT(comm_channels);
		comm_samples = SWAPINT(comm_samples);
		comm_bits = SWAPSHORT(comm_bits);
	    }
	    *sample_rate = (int)ConvertFromIeeeExtended(ieee_ext_sample_rate);
	}
	else if (strncmp(chunk.id,"SSND",4) == 0)
	{
	    if (ts.fread(&ssndchunk,1,sizeof(ssndchunk)) != sizeof(ssndchunk))
	    {
		fprintf(stderr,"AIFF chunk: eof within SSND chunk\n");
		return misc_read_error;
	    }
	    if (EST_LITTLE_ENDIAN)
	    {
		ssndchunk.offset = SWAPINT(ssndchunk.offset);
		ssndchunk.blocksize = SWAPINT(ssndchunk.blocksize);
	    }
	    
	    *num_channels = comm_channels;
	    switch (comm_bits)
	    {
	    case 8: actual_sample_type = st_uchar; break;
		case 16: actual_sample_type = st_short; break;
		default:
		    fprintf(stderr,"AIFF: unsupported sample width %d bits\n",
			    comm_bits);
		    return misc_read_error;
		}
	    
	    ts.seek(ssndchunk.offset+(comm_channels*offset)+ts.tell());
	    if (length == 0)
 		data_length = (comm_samples-offset)*comm_channels;
  	    else
 		data_length = length*comm_channels;
	    file_data = walloc(unsigned char, 
			       data_length*comm_channels*
			       get_word_size(actual_sample_type));
	    if ((n=ts.fread(file_data,get_word_size(actual_sample_type),
			 data_length)) != data_length)
	    {
		fprintf(stderr,"AIFF read: short file %s\n",
			(const char *)ts.filename());
		fprintf(stderr,"AIFF read: at %d got %d instead of %d samples\n",
			offset,n,data_length);
		data_length = n;
	    }
	    
	    *data = convert_raw_data(file_data,data_length,
				     actual_sample_type,bo_big);
	    *num_samples = data_length/comm_channels;
	    *sample_type = st_short;
	    *word_size = 2;
	    *bo = EST_NATIVE_BO;
	    break;			/* only care about the first SSND chunk */
	}
	else 
	{			/* skip bytes in chunk */
	    ts.seek(ts.tell()+chunk.size);
	}
    }
    
    return format_ok;
}

enum EST_write_status save_wave_aiff(FILE *fp, const short *data, int offset,
				     int num_samples, int num_channels, 
				     int sample_rate,
				     enum EST_sample_type_t sample_type, int bo)   
{
    (void)bo;
    const char *info;
    int data_size, data_int;
    unsigned char ieee_ext_buf[10];
    short data_short;
    
    info = "FORM";
    fwrite(info,1,4,fp);
    /* This number seems to be derived different for each example */
    data_size = 54+(num_samples*num_channels*get_word_size(sample_type));
    if (EST_LITTLE_ENDIAN)
	data_size = SWAPINT(data_size);
    fwrite(&data_size,1,4,fp);
    info = "AIFF";
    fwrite(info,1,4,fp);
    info = "COMM";
    fwrite(info,1,4,fp);
    data_int = 18;
    if (EST_LITTLE_ENDIAN)
	data_int = SWAPINT(data_int);
    fwrite(&data_int,1,4,fp);
    data_short = num_channels;
    if (EST_LITTLE_ENDIAN)
	data_short = SWAPSHORT(data_short);
    fwrite(&data_short,1,2,fp);
    data_int = num_samples;
    if (EST_LITTLE_ENDIAN)
	data_int = SWAPINT(data_int);
    fwrite(&data_int,1,4,fp);
    data_short = 8*get_word_size(sample_type);
    if (EST_LITTLE_ENDIAN)
	data_short = SWAPSHORT(data_short);
    fwrite(&data_short,1,2,fp);
    ConvertToIeeeExtended((double)sample_rate,ieee_ext_buf);
    fwrite(ieee_ext_buf,1,10,fp);
    info = "SSND";
    fwrite(info,1,4,fp);
    data_int = 8 + (num_samples*num_channels*get_word_size(sample_type));
    if (EST_LITTLE_ENDIAN)
	data_int = SWAPINT(data_int);
    fwrite(&data_int,1,4,fp);
    data_int = 0;
    if (EST_LITTLE_ENDIAN)
	data_int = SWAPINT(data_int);
    fwrite(&data_int,1,4,fp);   /* offset */
    if (EST_LITTLE_ENDIAN)
	data_int = SWAPINT(data_int);
    fwrite(&data_int,1,4,fp);   /* blocksize */
    
    if ((sample_type == st_short) ||
	(sample_type == st_uchar))
	return save_raw_data(fp,data,offset,num_samples,num_channels,
			     sample_type,bo_big);
    else
    {
	fprintf(stderr,"AIFF: requested data type not uchar or short\n");
	return misc_write_error;
    }
    
}

/*=======================================================================*/
/* ulaw EST_filetype are just raw data with 8K ulaw contents                 */
/*=======================================================================*/

enum EST_read_status load_wave_ulaw(EST_TokenStream &ts, short **data, int
				    *num_samples, int *num_channels, int *word_size, int
				    *sample_rate, enum EST_sample_type_t *sample_type, int *bo,
				    int offset, int length)

{
    unsigned char *ulaw;
    int data_length,samps;
    
    ts.seek_end();
    samps = ts.tell();
    
    if (length == 0)
	data_length = samps - offset;
    else
	data_length = length;
    
    ulaw = walloc(unsigned char, data_length);
    ts.seek(offset);
    if (ts.fread(ulaw,1,data_length) != data_length)
    {
	wfree(ulaw); 
	return misc_read_error;
    }
    
    *data = walloc(short,data_length);
    ulaw_to_short(ulaw,*data,data_length);
    wfree(ulaw);
    
    *num_samples = data_length;
    *sample_rate = 8000;
    *num_channels = 1;
    *sample_type = st_short;
    *word_size = 2;
    *bo = EST_NATIVE_BO;
    
    return format_ok;
}

enum EST_write_status save_wave_ulaw(FILE *fp, const short *data, int offset,
				     int num_samples, int num_channels, 
				     int sample_rate, 
				     enum EST_sample_type_t sample_type, int bo)
{
    (void)sample_rate;
    (void)sample_type;
    return save_wave_raw(fp,data,offset,num_samples,num_channels,
			 8000,st_mulaw,bo);
    
    
}

/*=======================================================================*/
/* Sun and Next snd files                                                */
/*=======================================================================*/

typedef struct {
    unsigned int    magic;	/* magic number */
    unsigned int    hdr_size;	/* size of this header */
    int    data_size;	        /* length of data (optional) */
    unsigned int    encoding;	/* data encoding format */
    unsigned int    sample_rate; /* samples per second */
    unsigned int    channels;	 /* number of interleaved channels */
} Sun_au_header;

enum EST_read_status load_wave_snd(EST_TokenStream &ts, short **data, int
				   *num_samples, int *num_channels, int *word_size, int
				   *sample_rate,enum EST_sample_type_t *sample_type, int *bo ,
				   int offset, int length)
{
    /* Header structures */
    Sun_au_header header;
    enum EST_sample_type_t encoding_type;
    int data_length, sample_width, bytes, samps, n;
    unsigned char *file_data;
    int current_pos;
    
    current_pos = ts.tell();
    ts.fread(&header, sizeof(Sun_au_header), 1);
    
    /* test for magic number */
    if ((EST_LITTLE_ENDIAN) && 
	((unsigned int)0x2e736e64 == SWAPINT(header.magic)))
    {				/* wrong byte order, swap header */
	header.hdr_size = SWAPINT(header.hdr_size);
	header.data_size = SWAPINT(header.data_size);
	header.encoding = SWAPINT(header.encoding);
	header.sample_rate = SWAPINT(header.sample_rate);
	header.channels = SWAPINT(header.channels);
    }
    else if ((unsigned int)0x2e736e64 != header.magic)
	return wrong_format;
    
    switch (header.encoding) 
    {
    case 1:
	encoding_type = st_mulaw;	
	break;
    case 2:
	encoding_type = st_uchar;	
	break;
    case 3:
	encoding_type = st_short;	
	break;
    default:
	fprintf(stderr, "Unsupported data type in SND header\n");
	return misc_read_error;
    }
    
    *num_channels = header.channels;
    sample_width = get_word_size(encoding_type);
    *sample_rate = header.sample_rate;
    
    if ((header.data_size == 0) ||
	(header.data_size == -1))
    {
	ts.seek_end();
	bytes = ts.tell() - header.hdr_size;
    }
    else
	bytes = header.data_size;
    samps = bytes/sample_width;
    
    if (length == 0)
	data_length = (samps - offset)*(*num_channels);
    else
	data_length = length *(*num_channels);
    
    file_data = walloc(unsigned char, sample_width * data_length);
    ts.seek(current_pos+header.hdr_size+(sample_width*offset*(*num_channels)));
    if ((n=ts.fread(file_data,sample_width,data_length)) != data_length)
    {
	fprintf(stderr,"WAVE read: short file %s\n",
		(const char *)ts.filename());
	fprintf(stderr,"WAVE read: at %d got %d instead of %d samples\n",
		offset,n,data_length);
	data_length = n;
    }
    
    *data = convert_raw_data(file_data,data_length,encoding_type,bo_big);

    if (*data == NULL)
      return read_error;
    
    *num_samples = data_length/ (*num_channels);
    *sample_type = st_short;
    *bo = EST_NATIVE_BO;
    *word_size = 2;
    return read_ok;
}

enum EST_write_status save_wave_snd(FILE *fp, const short *data, int offset,
				    int num_samples, int num_channels, 
				    int sample_rate, 
				    enum EST_sample_type_t sample_type, int bo)
{
    (void)bo;
    /* Header structures */
    Sun_au_header header;
    
    /* Fill in header structure */
    header.magic = (unsigned int)0x2e736e64; /* should be a macro surely */
    header.hdr_size = sizeof(header);	     /* ! */
    header.data_size = get_word_size(sample_type) * num_channels * num_samples;
    
    switch (sample_type) {
    case st_mulaw:
	header.encoding = 1;
	break;
    case st_uchar:
	header.encoding = 2;
	break;
    case st_short:
	header.encoding = 3;
	break;
	
    default:
	fprintf(stderr, 
		"Unsupported sample type cannot be saved in SND format\n");
	return misc_write_error;
	
    }
    
    /* check consistency */
    
    header.sample_rate = sample_rate;
    
    header.channels = num_channels;
    
    if (EST_LITTLE_ENDIAN)
    {
	/* snd files all in big endian format */
	header.magic = SWAPINT(header.magic);
	header.hdr_size = SWAPINT(header.hdr_size);
	header.data_size = SWAPINT(header.data_size);
	header.encoding = SWAPINT(header.encoding);
	header.sample_rate = SWAPINT(header.sample_rate);
	header.channels = SWAPINT(header.channels);
	
    }
    /* write header */
    if (fwrite(&header, sizeof(header), 1, fp) != 1)
	return misc_write_error;

    /* snd files are always in BIG_ENDIAN (sun) byte order */
    return save_raw_data(fp,data,offset,num_samples,num_channels,
			 sample_type,bo_big);
}

/*=======================================================================*/
/* CSTR Audlab files (from the last century)                             */
/* They are always bigendian                                             */
/*=======================================================================*/
struct s1 {
    char   c[17];
    float  f1;
    float  f2;
};

struct s2 {
    float f1;
    float f2;   
    float f3;
    char c1;
    char c2;
    int i1;
    int i2;
};

struct audlabfh {
    struct s1 z; 
    char  file_type[8];
    char  c1[17];
    char  c2[17];
    char  c3[17];
    int   start;
    char  data_type;    
    char  c5[64];
};

struct audlabsh {
    int   channel_count;
    char  serial;
    int   sample_rate;
    char  c1[20];
    int   i1;
    char  c2;
    char  c3[121];
    char  c4[121];
    
};
struct audlabsd {
    char descr[17];
    int sample_count;
    int nbits;
    float f1;
    struct s2 z;
};

enum EST_read_status load_wave_audlab(EST_TokenStream &ts, short **data, int
				      *num_samples, int *num_channels, int *word_size, int
				      *sample_rate, enum EST_sample_type_t *sample_type, int *bo, int
				      offset, int length)
{
    /* Header structures */
    struct audlabfh fh;
    struct audlabsh sh;
    struct audlabsd sd;
    int data_length,sample_count;
    int hdr_length;
    int current_pos;
    
    /* Read header structures from char array */
    current_pos = ts.tell();
    ts.fread(&fh, sizeof(struct audlabfh), 1);
    if (strcmp(fh.file_type, "Sample") != 0) 
	return wrong_format;
    
    ts.fread(&sh, sizeof(struct audlabsh), 1);
    ts.fread(&sd, sizeof(struct audlabsd), 1);
    hdr_length = sizeof(struct audlabfh) +
	sizeof(struct audlabsh) +
	    sizeof(struct audlabsd);
    
    if (EST_BIG_ENDIAN)
    {
	*num_channels = sh.channel_count;
	*sample_rate = sh.sample_rate;
	sample_count = sd.sample_count;
    }
    else			// audlab files are bigendian
    {
	*num_channels = SWAPINT(sh.channel_count);
	*sample_rate = SWAPINT(sh.sample_rate);
	sample_count = SWAPINT(sd.sample_count);
    }
    if (length == 0)
	data_length = (sample_count - offset) * (*num_channels);
    else
	data_length = length *(*num_channels);
    
    *data = walloc(short,sizeof(short) * data_length);
    ts.seek(current_pos+hdr_length+(sizeof(short)*offset*(*num_channels)));
    
    if ((int)ts.fread(*data, sizeof(short), data_length) != data_length)
    {
	wfree(*data);
	return misc_read_error;
    }
    if (EST_LITTLE_ENDIAN)
	swap_bytes_short(*data,data_length);
    
    *num_samples = data_length / (*num_channels);
    *sample_type = st_short;	/* set internal type*/
    *word_size = sizeof(short);
    *bo = EST_NATIVE_BO;
    
    return format_ok;
}

enum EST_write_status save_wave_audlab(FILE *fp, const short *data, int offset,
				       int num_samples, int num_channels, 
				       int sample_rate, 
				       enum EST_sample_type_t sample_type, int bo)
{
    (void)bo;
    (void)sample_type;
    /* Header structures */
    struct audlabfh fh;
    struct audlabsh sh;
    struct audlabsd sd;
    
    fh.start = sizeof (struct audlabfh) +
	sizeof (struct audlabsh) + sizeof (struct audlabsd);
    fh.data_type = 2;
    strcpy(fh.file_type, "Sample");
    
    if (EST_LITTLE_ENDIAN) 
    {				// need to swap some of those numbers 
	sh.channel_count = SWAPINT(num_channels);
	sh.serial = 1;
	sh.sample_rate = SWAPINT(sample_rate);
	
	sd.sample_count = SWAPINT(num_samples);
	sd.nbits = SWAPINT(16);
    }
    else
    {
	sh.channel_count = num_channels;
	sh.serial = 1;
	sh.sample_rate = sample_rate;
	
	sd.sample_count = num_samples;
	sd.nbits = 16;
    }
    sprintf(sd.descr, "Filter 1");
    
    /* write headers */
    fwrite (&fh, sizeof(fh), 1, fp);
    fwrite (&sh, sizeof(sh), 1, fp);
    fwrite (&sd, sizeof(sd), 1, fp);
    
    /* write data*/
    return save_raw_data(fp,data,offset,num_samples,num_channels,
			 st_short,bo_big);
}

/*=======================================================================*/
/* Entropic ESPS SD files: portable (non-proprietary) method             */
/*=======================================================================*/

/* Deep thanks go to Peter Kabal from McGill University whose AF code */
/* showed me this was even possible.  I looked at his code to find    */
/* parts I couldn't work out myself.  Also to Rodney Johnson of       */
/* Entropic whose document "ESPS APPLICATION NOTE: Non-ESPS Programs  */
/* and the ESPS File System" gave details of how to access ESPS files */
/* without using the ESPS library code.                               */

#include "esps_utils.h"
enum EST_read_status load_wave_sd(EST_TokenStream &ts, short **data, int
				  *num_samples, int *num_channels, int
				  *word_size, int *sample_rate, enum
				  EST_sample_type_t *sample_type, int *bo , int
				  offset, int length)
{
    /* A license free version of an esps file reading program */
    FILE *fd;
    esps_hdr hdr;
    int actual_bo, sample_width, data_length;
    enum EST_read_status rv;
    int dl;
    enum EST_sample_type_t actual_sample_type;
    double d;
    unsigned char *file_data;

    if ((fd = ts.filedescriptor()) == NULL)
    {
	fprintf(stderr, "Can't open esps file %s for reading\n",
		(const char *)ts.filename());
	return misc_read_error;
    }
    
    if ((rv=read_esps_hdr(&hdr,fd)) != format_ok)
	return rv;
    
    if (hdr->file_type != ESPS_SD)
    {
	fprintf(stderr,"ESPS file: not an FEA_SD file\n");
	delete_esps_hdr(hdr);
	return misc_read_error;
    }
    
    if (fea_value_d("record_freq",0,hdr,&d) != 0)
    {
	fprintf(stderr,"ESPS file: can't find sample_rate in header assuming 16000\n");
	*sample_rate = 16000;
    }
    else
	*sample_rate = (int)d;
    actual_sample_type = st_short; /* you're not trying hard enough */
    sample_width = get_word_size(actual_sample_type);
    *num_channels = hdr->field_dimension[0];
    if (hdr->swapped)
	actual_bo = (EST_BIG_ENDIAN ? bo_little : bo_big);
    else
	actual_bo = (EST_BIG_ENDIAN ? bo_big : bo_little);
    
    if (length == 0)
	data_length = (hdr->num_records - offset)*(*num_channels);
    else
	data_length = length *(*num_channels);
    
    file_data = walloc(unsigned char, sample_width * data_length);
    fseek(fd,hdr->hdr_size+(sample_width*offset*(*num_channels)),
	  SEEK_SET);
    if ((dl=fread(file_data,sample_width,data_length,fd)) != data_length)
    {
	fprintf(stderr,"WAVE read: esps short file %s\n",
		(const char *)ts.filename());
	fprintf(stderr,"WAVE read: at %d got %d instead of %d samples\n",
		offset,dl,data_length);
	data_length = dl;
    }
    
    *data = convert_raw_data(file_data,data_length,
			     actual_sample_type,
			     actual_bo);
    
    *num_samples = data_length/ (*num_channels);
    *sample_type = st_short;
    *bo = EST_NATIVE_BO;
    *word_size = 2;
    delete_esps_hdr(hdr);
    return format_ok;
    
}

enum EST_write_status save_wave_sd(FILE *fp, const short *data, int offset,
				   int num_samples, int num_channels, 
				   int sample_rate, 
				   enum EST_sample_type_t sample_type, int bo)

{
    (void)bo;
    esps_hdr hdr = make_esps_sd_hdr();
    enum EST_write_status rv;
    short esps_type;
    
    hdr->num_records = num_samples;
    switch (sample_type)
    {
    case st_short:  esps_type = ESPS_SHORT; break;
		case st_schar:  esps_type = ESPS_CHAR; break; /* maybe should be BYTE */
		case st_int:    esps_type = ESPS_INT; break;
		case st_float:  esps_type = ESPS_FLOAT; break;
		case st_double: esps_type = ESPS_DOUBLE; break;
		default:
		    fprintf(stderr,"ESPS file: no support for sample_type %s\n",
			    sample_type_to_str(sample_type));
		    return misc_write_error;
		}
    /* I believe all of the following are necessary and in this order */
    add_field(hdr,"samples",esps_type,num_channels);
    add_fea_special(hdr,ESPS_FEA_DIRECTORY,"margo:/disk/disk10/home/awb/projects/speech_tools/main");
    add_fea_special(hdr,ESPS_FEA_COMMAND,
		    "EDST waveform written as ESPS FEA_SD.\n\
			");
    add_fea_d(hdr,"start_time",0,(double)0);
    add_fea_d(hdr,"record_freq",0,(double)sample_rate);
    add_fea_d(hdr,"max_value",0,(double)27355);
    
    if ((rv=write_esps_hdr(hdr,fp)) != write_ok)
    {
	delete_esps_hdr(hdr);
	return rv;
    }
    /* lets ignore desired bo and sample type for the time being */
    delete_esps_hdr(hdr);
    
    return save_raw_data(fp,data,offset,num_samples,num_channels,
			 sample_type,EST_NATIVE_BO);
}

/*=======================================================================*/
/* Raw data files -- unheadered                                          */
/* THESE FUNCTIONS ARE DIFFERENT FROM THE REST                           */
/* These function have more parameters than the standard file i/o        */
/* as information cannot be gleamed from the file                        */
/*=======================================================================*/

enum EST_read_status load_wave_raw(EST_TokenStream &ts, short **data, int
				   *num_samples, int *num_channels, 
				   int *word_size, int
				   *sample_rate,  
				   enum EST_sample_type_t *sample_type, 
				   int *bo, int offset, int length, 
				   int isample_rate, 
				   enum EST_sample_type_t isample_type,
				   int ibo, int inc)
{
    unsigned char *file_data;
    int data_length,samps,sample_width;
    int guess,i,samp;
    short *ndata;
    
    if (isample_type == st_ascii)
    {
	/* Guess the size */
	if ((offset != 0) || (length != 0))
	{
	    fprintf(stderr,"Load ascii wave: doesn't support offets and lengths\n");
	    return misc_read_error;
	}
	
	ts.seek_end();
	guess = (int)(1.2*ts.tell()/7)+10; /* rough guess of the num of samps */
	ts.seek(0);
	*data = walloc(short, guess);
	i=0;
	while (!ts.eof())
	{
	    samp = atoi(ts.get().string());
	    if (i == guess)
	    {
		ndata = walloc(short,(int)(guess*1.2));
		memmove(ndata,*data,guess*sizeof(short));
		wfree(*data);
		*data = ndata;
		guess = (int)(guess*1.2);
	    }
	    if (samp < -32768)
	    {
		fprintf(stderr,"Load ascii wave: sample %d underflow clipping\n",
			i);
		(*data)[i] = -32768;
	    }
	    else if (samp > 32767)
	    {
		fprintf(stderr,"Load ascii wave: sample %d overflow clipping\n",
			i);
		(*data)[i] = 32767;
	    }
	    else
		(*data)[i] = (short)samp;
	    i++;
	}
	data_length = i;
    }
    else
    {
	ts.seek_end();
	sample_width = get_word_size(isample_type);
	samps = ts.tell()/sample_width;
	
	if (length == 0)
	    data_length = samps - offset;
	else
	    data_length = length;
	
	file_data = walloc(unsigned char, data_length * sample_width *inc);
	ts.seek(offset*sample_width*inc);
	if ((int)ts.fread(file_data,sample_width,data_length) != data_length)
	    return misc_read_error;
	
	*data = convert_raw_data(file_data,data_length,isample_type,ibo);
    }
    
    *num_samples = data_length/inc;
    *sample_rate = isample_rate;
    *num_channels = inc;
    *sample_type = st_short;
    *word_size = 2;
    *bo = EST_NATIVE_BO;
    
    return format_ok;
}

enum EST_write_status save_wave_raw(FILE *fp, const short *data, 
				    int offset,
				    int num_samples, int num_channels, 
				    int sample_rate,
				    enum EST_sample_type_t sample_type, int bo)   
{
    (void)sample_rate;
    
    return save_raw_data(fp,data,offset,num_samples,num_channels,
			 sample_type,bo);
}

/***********************************************************************/
/*                                                                     */
/* end of file type specific functions                                 */
/*                                                                     */
/***********************************************************************/

