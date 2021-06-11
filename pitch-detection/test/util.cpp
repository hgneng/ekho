#include "util.h"
#include "pitch_detection.h"
#include <cmath>
#include <float.h>
#include <fstream>
#include <iostream>
#include <iterator>
#include <map>
#include <sstream>
#include <string>
#include <vector>

std::vector<double>
test_util::sinewave(size_t size, double frequency, int sample_rate)
{
	size_t lut_size = size / 4;

	std::vector<int> lut{};
	double *_tone_single_channel = (double *)malloc(sizeof(double) * size / 2);

	double doublef = (double)frequency;
	double delta_phi = doublef * lut_size * 1.0 / sample_rate;
	double phase = 0.0;

	for (int i = 0; i < signed(lut_size); ++i) {
		lut.push_back((int)roundf(0x7FFF * sinf(2.0 * M_PI * i / lut_size)));
	}

	double min = DBL_MAX;
	double max = -DBL_MAX;
	for (int i = 0; i < signed(size / 2); ++i) {
		int val = double(lut[(int)phase]);
		if (val > max) {
			max = val;
		}
		if (val < min) {
			min = val;
		}
		_tone_single_channel[i] = val;
		phase += delta_phi;
		if (phase >= lut_size)
			phase -= lut_size;
	}

	std::vector<double> tone_single_channel(
	    _tone_single_channel, _tone_single_channel + size / 2);

	return tone_single_channel;
}

std::vector<double>
test_util::vec_from_file(std::string path)
{
	std::vector<double> data;
	std::ifstream infile(path);

	if (infile.fail()) {
		std::cerr << "File '" << path << "' doesn't exist, exiting"
		          << std::endl;
		exit(1);
	}

	double val;
	while (infile >> val)
		data.push_back(val);

	return data;
}
