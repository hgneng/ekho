#ifndef UTIL_H
#define UTIL_H

#include <string>
#include <vector>

namespace test_util
{

std::vector<double>
sinewave(size_t, double, int);

std::vector<double> vec_from_file(std::string);

} // namespace test_util

#endif /* UTIL_H */
