#include <iostream>
#include <set>
#include <map>
#include <sys/types.h>

#include "utils.hpp"

struct Coord {
  ssize_t row;
  ssize_t col;

  bool operator<(const Coord& rhs) const
  {
    return row < rhs.row or (rhs.row == row and col < rhs.col);
  }

  bool operator==(const Coord& rhs) const
  {
    return row == rhs.row and col == rhs.col;
  }
};

unsigned long long solve_part1(const std::vector<std::string> &rows, const std::set<Coord> &splitters, const Coord &start) {
  unsigned long long sum = 0;
  std::set<ssize_t> beams;
  beams.insert(start.col);

  for (ssize_t row_idx=start.row+1; row_idx<(ssize_t)rows.size(); ++row_idx) {
    std::set<ssize_t> beam_cols;
    for (auto beam_col: beams) {
      Coord b = {row_idx, beam_col};
      if (splitters.contains(b)) {
        bool is_split = false;

        if (beam_col-1 >= 0 && rows[row_idx][beam_col-1] == '.') {
          beam_cols.insert(beam_col-1);
          is_split = true;
        }
        if (beam_col+1 < (ssize_t)rows[0].size() && rows[row_idx][beam_col+1] == '.') {
          beam_cols.insert(beam_col+1);
          is_split = true;
        }

        if (is_split) {
          sum += 1;
        }
      } else {
        beam_cols.insert(beam_col);
      }
    }

    beams = beam_cols;
  }

  return sum;
}

unsigned long long solve_part2(const std::vector<std::string> &rows,
                               const std::set<Coord> &splitters,
                               const Coord &start,
                               std::map<Coord, unsigned long long> &cache
                              ) {
  ssize_t row_idx = start.row + 1;
  ssize_t beam_col = start.col;
  Coord beam = {row_idx, beam_col};
  unsigned long long current_sum = 0;

  auto it = cache.find(beam);
  if (it != cache.end()) {
    return it->second;
  }

  if (start.row + 1 == (ssize_t)rows.size()) {
    current_sum = 1;
  } else {
    std::vector<ssize_t> beam_cols;

    if (splitters.contains(beam)) {
      if (beam_col - 1 >= 0 && rows[row_idx][beam_col - 1] == '.') {
        beam_cols.push_back(beam_col - 1);
      }
      if (beam_col + 1 < (ssize_t)rows[0].size() &&
          rows[row_idx][beam_col + 1] == '.') {
        beam_cols.push_back(beam_col + 1);
      }
    } else {
      beam_cols.push_back(beam_col);
    }

    for (auto col : beam_cols) {
      current_sum += solve_part2(rows, splitters, {row_idx, col}, cache);
    }
  }

  cache.insert({beam, current_sum});
  return current_sum;
}

int main(int argc, char *argv[]) {
  try {
    auto [part, filename] = aoc::validate_input(argc, argv);

    auto rows = aoc::read_lines(filename);
    Coord start(0, 0);
    std::set<Coord> splitters;
    for (ssize_t row_idx = 0; row_idx < (ssize_t)rows.size(); ++row_idx) {
      auto row = rows[row_idx];
      for (ssize_t col_idx = 0; col_idx < (ssize_t)row.size(); ++col_idx) {
        auto c = row[col_idx];
        if (c == '.') {
          continue;
        }
        if (c == 'S') {
          start = {row_idx, col_idx};
          continue;
        }
        if (c == '^') {
          splitters.insert({row_idx, col_idx});
          continue;
        }
      }
    }

    if (part == 1) {
      std::cout << "Part 1: " << solve_part1(rows, splitters, start) << "\n";
    } else {
      std::map<Coord, unsigned long long> cache;
      std::cout << "Part 2: " << solve_part2(rows, splitters, start, cache) << "\n";
    }
  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << "\n";
    return -1;
  }

  return 0;
}
