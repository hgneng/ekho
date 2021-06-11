#!/usr/bin/env python3

import numpy
from pydub import AudioSegment
from pydub.utils import get_array_type
import sys


if __name__ == '__main__':
    try:
        inputfile = sys.argv[1]
        outputfile = sys.argv[2]
    except IndexError:
        print('usage: get_raw_audio.py infile.ext outfile.txt', file=sys.stderr)
        sys.exit(1)

    sound = AudioSegment.from_file(
            file=inputfile,
            format=inputfile.split('.')[-1]).set_channels(1)

    sound_raw = numpy.frombuffer(sound._data,
            dtype=get_array_type(sound.sample_width * 8))

    print('sample rate: {0}'.format(sound.frame_rate), file=sys.stderr)
    
    with open(outputfile, 'w') as fwrite:
        for sample in sound_raw:
            fwrite.write('{0}\n'.format(sample))
