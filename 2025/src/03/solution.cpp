#include <iostream>

#include "utils.hpp"

unsigned long long solve(const std::vector<std::vector<int>> &batteries, size_t num_to_find_base) {
  unsigned long long sum = 0;
  for (auto &bat: batteries) {
    size_t num_to_find = num_to_find_base;
    size_t top_boundary = bat.size() - num_to_find + 1;
    std::vector<int> nums;
    size_t bottom_boundary = 0;

    while (num_to_find != 0) {
      int f = 0;
      for (size_t i = bottom_boundary; i < top_boundary; ++i) {
        if (bat[i] > f) {
          f = bat[i];
          bottom_boundary = i;
        }
      }

      nums.push_back(f);
      num_to_find -= 1;
      top_boundary += 1;
      bottom_boundary += 1;
    }

    unsigned long long jolt = 0;
    for (auto n: nums) {
      jolt = jolt*10 + n;
    }

    sum += jolt;
  }

  return sum;
}

int main(int argc, char *argv[]) {
  try {
    auto [part, filename] = aoc::validate_input(argc, argv);

    auto data = aoc::read_lines(filename);
    std::vector<std::vector<int>> batteries;
    for (auto &line: data) {
      std::vector<int> battery;
      for (auto c: line) {
        int b = c - '0';
        battery.push_back(b);
      }
      batteries.push_back(battery);
    }

    if (part == 1) {
      std::cout << "Part 1: " << solve(batteries, 2) << "\n";
    } else {
      std::cout << "Part 2: " << solve(batteries, 12) << "\n";
    }
  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << "\n";
    return -1;
  }

  return 0;
}
