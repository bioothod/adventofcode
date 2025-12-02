#include <cmath>
#include <iostream>
#include <charconv>

#include "utils.hpp"

enum Dir {
  LEFT,
  RIGHT,
};

struct Rotation {
  Dir dir;
  int num;
};

const int DIAL_START = 50;
const int DIAL_NUM = 100;

int solve_part1(std::vector<Rotation> &rotations) {
  int sum = 0;

  int start = DIAL_START;
  for (const auto &r: rotations) {
    switch (r.dir) {
    case Dir::LEFT: {start -= r.num; break;}
    case Dir::RIGHT: {start += r.num; break;}
    }

    start = (start + DIAL_NUM) % DIAL_NUM;
    if (start == 0) {
      sum += 1;
    }
  }

  return sum;
}

int solve_part2(std::vector<Rotation> &rotations) {
  int sum = 0;

  int start = DIAL_START;
  for (const auto &r: rotations) {
    int prev_start = start;
    switch (r.dir) {
    case Dir::LEFT: {start -= r.num; break;}
    case Dir::RIGHT: {start += r.num; break;}
    }

    if (start == 0) {
      sum += 1;
    } else if (start > 0) {
      int div = floor(start / DIAL_NUM);
      sum += div;
      start %= DIAL_NUM;
    } else {
      int div = floor(abs(start) / DIAL_NUM);
      sum += div;
      if (prev_start != 0) {
        sum += 1;
      }
      start = DIAL_NUM - (abs(start) % DIAL_NUM);
      if (start == DIAL_NUM) {
        start = 0;
      }
    }

  }

  return sum;
}

int main(int argc, char *argv[]) {
  try {
    auto [part, filename] = aoc::validate_input(argc, argv);

    auto data = aoc::read_lines(filename);
    std::vector<Rotation> rots;
    rots.reserve(data.size());

    for (const auto &line: data) {
      auto [dir, num] = aoc::split_prefix_number(line);

      if (dir == "L") {
        rots.push_back({Dir::LEFT, num});
      } else if (dir == "R") {
        rots.push_back({Dir::RIGHT, num});
      } else {
        throw std::invalid_argument("Invalid direction in line " + line);
      }
    }

    if (part == 1) {
      std::cout << "Part 1: " << solve_part1(rots) << "\n";
    } else {
      std::cout << "Part 2: " << solve_part2(rots) << "\n";
    }
  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << "\n";
    return -1;
  }

  return 0;
}
