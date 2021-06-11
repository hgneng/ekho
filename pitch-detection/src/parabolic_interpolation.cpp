#include "pitch_detection.h"
#include <vector>

template <typename T>
std::pair<T, T>
util::parabolic_interpolation(const std::vector<T> &array, int x_)
{
	int x_adjusted;
	T x = (T)x_;

	if (x < 1) {
		x_adjusted = (array[x] <= array[x + 1]) ? x : x + 1;
	} else if (x > signed(array.size()) - 1) {
		x_adjusted = (array[x] <= array[x - 1]) ? x : x - 1;
	} else {
		T den = array[x + 1] + array[x - 1] - 2 * array[x];
		T delta = array[x - 1] - array[x + 1];
		return (!den) ? std::make_pair(x, array[x])
		              : std::make_pair(x + delta / (2 * den),
		                    array[x] - delta * delta / (8 * den));
	}
	return std::make_pair(x_adjusted, array[x_adjusted]);
}

template std::pair<double, double>
util::parabolic_interpolation<double>(const std::vector<double> &array, int x);
template std::pair<float, float>
util::parabolic_interpolation<float>(const std::vector<float> &array, int x);
