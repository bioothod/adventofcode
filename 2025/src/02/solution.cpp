#include <format>
#include <sstream>
#include <iostream>
#include <stdexcept>
#include <utility>
#include <string>

#include "utils.hpp"

unsigned long long quick_pow10(int n)
{
  static std::vector<unsigned long long> pow10 = {
    1, 10, 100, 1000, 10000,
    100000, 1000000, 10000000, 100000000, 1000000000,
    10000000000, 100000000000, 1000000000000, 10000000000000,
    100000000000000, 1000000000000000, 10000000000000000, 100000000000000000,
  };

  if ((size_t)n >= pow10.size()) {
    throw std::invalid_argument(std::format("quick_pow10({}): n is too big, max is {}", n, pow10.size()));
  }

  return pow10[n];
}

template <std::integral T>
T combine(const std::vector<T> &value) {
  T ret = 0;
  for (auto r = value.begin(); r != value.end(); ++r) {
    ret *= 10;
    ret += *r;
  }
  return ret;
}
template <std::integral T>
std::vector<T> split(T value) {
  std::vector<T> tmp;
  while (value > 0) {
    T r = value % 10;
    tmp.push_back(r);
    value /= 10;
  }
  std::reverse(tmp.begin(), tmp.end());
  return tmp;
}

template <std::integral T>
std::tuple<T, T> split_and_pad(T value) {
  auto tmp = split(value);

  if (tmp.size() % 2 != 0) {
    tmp = split(quick_pow10(tmp.size()+1));
  }

  T middle = tmp.size() / 2;
  std::vector<T> v0(tmp.begin(), tmp.begin() + middle);
  std::vector<T> v1(tmp.begin()+middle, tmp.end());

  T f0 = combine(v0);
  T f1 = combine(v1);

  return {f0, f1};
}

unsigned long long solve_part1(const std::vector<std::pair<unsigned long long, unsigned long long>> &id_ranges) {
  unsigned long long sum = 0;
  for (auto ids: id_ranges) {
    auto [start, end] = ids;
    auto [f0, f1] = split_and_pad(start);
    unsigned long long mult = quick_pow10(split(f0).size());

    if (f0 != f1) {
      if (f0 > f1) {
        f1 = f0;
      } else {
        f0 = f0 + 1;
        f1 = f0;
      }
    }
    auto combined = f0 * mult + f1;

    while (combined <= end) {
      sum += combined;

      f0 += 1;
      f1 += 1;

      if (f0 % 10 == 0) {
        mult = quick_pow10(split(f0).size());
      }

      combined = f0 * mult + f1;
    }
  }
  return sum;
}

template <std::integral T>
bool check_n(std::vector<T> &value, size_t n) {
  if (value.size() % n != 0) {
    return false;
  }

  for (size_t i=0; i<n; ++i) {
    T first = value[i];

    for (auto idx=i+n; idx<value.size(); idx+=n) {
      T next = value[idx];
      if (first != next) {
        return false;
      }
    }
  }
  return true;
}

unsigned long long solve_part2(const std::vector<std::pair<unsigned long long, unsigned long long>> &id_ranges) {
  unsigned long long sum = 0;
  for (auto ids: id_ranges) {
    auto [start, end] = ids;
    for (auto index=start; index<=end; ++index) {
      auto spl = split(index);
      for (size_t n =1; n<spl.size()/2+1; ++n) {
        if (check_n(spl, n)) {
          sum += index;
          break;
        }
      }
    }
  }
  return sum;
}

int main(int argc, char *argv[]) {
  try {
    auto [part, filename] = aoc::validate_input(argc, argv);

    auto data = aoc::read_lines(filename);
    std::vector<std::pair<unsigned long long, unsigned long long>> id_ranges;
    std::stringstream ss(data[0]);
    std::string token;
    while (std::getline(ss, token, ',')) {
      std::stringstream sd(token);
      std::vector<unsigned long long> ids;
      std::string id;
      while (std::getline(sd, id, '-')) {
        ids.push_back(std::stoull(id));
      }
      if (ids.size() != 2) {
        throw std::invalid_argument(std::format("Invalid pair token: {}", token));
      }
      id_ranges.push_back({ids[0], ids[1]});
    }

    if (part == 1) {
      std::cout << "Part 1: " << solve_part1(id_ranges) << "\n";
    } else {
      std::cout << "Part 2: " << solve_part2(id_ranges) << "\n";
    }
  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << "\n";
    return -1;
  }

  return 0;
}
