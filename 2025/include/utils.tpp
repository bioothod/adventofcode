#pragma once

#include <algorithm>
#include <vector>

#include "utils.hpp" // needed for clangd

namespace aoc {


template <std::integral T>
inline T combine(const std::vector<T> &value) {
  T ret = 0;
  for (auto r = value.begin(); r != value.end(); ++r) {
    ret *= 10;
    ret += *r;
  }
  return ret;
}

template <std::integral T>
inline std::vector<T> split(T value) {
  std::vector<T> tmp;
  while (value > 0) {
    T r = value % 10;
    tmp.push_back(r);
    value /= 10;
  }
  std::reverse(tmp.begin(), tmp.end());
  return tmp;
}

} // namespace aoc
