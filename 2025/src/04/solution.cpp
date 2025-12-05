#include <iostream>

#include "utils.hpp"

int solve(std::vector<std::string> &grid, size_t num_iters) {
  int sum = 0;
  ssize_t row_size = grid[0].size();

  while (num_iters > 0) {
    std::vector<std::pair<ssize_t, ssize_t>> pos2remove;

    for (ssize_t i=0; i < (ssize_t)grid.size(); ++i) {
      for (ssize_t j=0; j < row_size; ++j) {
        if (grid[i][j] == '@') {
          ssize_t begin_x = std::max(j-1, (ssize_t)0);
          ssize_t end_x = std::min(j+2, row_size);
          ssize_t begin_y = std::max(i-1, (ssize_t)0);
          ssize_t end_y = std::min(i+2, (ssize_t)grid.size());

          int rolls = 0;
          for (auto x=begin_x; x<end_x; ++x) {
            for (auto y=begin_y; y<end_y; ++y) {
              if (grid[y][x] == '@') {
                rolls += 1;
              }
            }
          }

          if (rolls <= 4) {
            pos2remove.push_back({i, j});
          }

        }
      }
    }

    if (pos2remove.empty()) {
      break;
    }

    sum += pos2remove.size();
    for (auto [i, j]: pos2remove) {
      grid[i][j] = '.';
    }
    num_iters -= 1;
  }
  return sum;
}

int main(int argc, char *argv[]) {
  try {
    auto [part, filename] = aoc::validate_input(argc, argv);

    auto data = aoc::read_lines(filename);

    if (part == 1) {
      std::cout << "Part 1: " << solve(data, 1) << "\n";
    } else {
      std::cout << "Part 2: " << solve(data, 999999999999999) << "\n";
    }
  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << "\n";
    return -1;
  }

  return 0;
}
