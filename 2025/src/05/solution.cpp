#include <algorithm>
#include <iostream>
#include <map>
#include <sstream>

#include "utils.hpp"

unsigned long long solve_part1(const std::vector<std::pair<unsigned long long, unsigned long long>> &ranges, const std::vector<unsigned long long> &all_ids) {
  unsigned long long sum = 0;
  for (auto id: all_ids) {
    for (auto [start, finish]: ranges) {
      if (id >= start && id <= finish) {
        sum += 1;
        break;
      }
    }
  }
  return sum;
}

unsigned long long solve_part2(const std::vector<std::pair<unsigned long long, unsigned long long>> &orig_ranges) {
  std::map<unsigned long long, unsigned long long> ranges;
  for (auto [start, finish]: orig_ranges) {
    auto iter = ranges.upper_bound(start);
    auto inserted = iter;

    if (iter == ranges.begin() or std::prev(iter)->second < start) {
      inserted = ranges.insert({start, finish}).first;
    } else {
      auto prev = std::prev(iter);

      if (prev->second >= finish) {
        continue;
      } else {
        prev->second = finish;
      }

      inserted = prev;
    }

    while (iter != ranges.end() and finish >= iter->first) {
      inserted->second = std::max(inserted->second, iter->second);
      iter = ranges.erase(iter);
    }
  }

  unsigned long long sum = 0;
  for (auto [start, finish]: ranges) {
    sum += finish - start + 1;
  }
  return sum;
}

int main(int argc, char *argv[]) {
  try {
    auto [part, filename] = aoc::validate_input(argc, argv);

    auto data = aoc::read_lines(filename);

    std::vector<std::pair<unsigned long long, unsigned long long>> ranges;
    std::vector<unsigned long long> all_ids;
    bool parsing_ids = false;

    for (auto &s: data) {
      if (!parsing_ids) {
        if (!s.empty()) {
          std::stringstream ss(s);
          std::string token;
          std::vector<unsigned long long> ids;
          while (std::getline(ss, token, '-')) {
            ids.push_back(std::stoull(token));
          }

          ranges.push_back({ids[0], ids[1]});
        } else {
          parsing_ids = true;
          continue;
        }
      }

      if (parsing_ids) {
        all_ids.push_back(std::stoull(s));
      }
    }

    if (part == 1) {
      std::cout << "Part 1: " << solve_part1(ranges, all_ids) << "\n";
    } else {
      std::cout << "Part 2: " << solve_part2(ranges) << "\n";
    }
  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << "\n";
    return -1;
  }

  return 0;
}
