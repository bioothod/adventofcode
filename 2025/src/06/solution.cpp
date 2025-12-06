#include <iostream>
#include <format>

#include "utils.hpp"

template <std::integral T>
T run_numbers(const std::vector<T> &numbers, const char op) {
  if (op == '*') {
    T res = 1;
    for (auto v: numbers) {
      res *= v;
    }

    return res;
  } else if (op == '+') {
    T sum = 0;
    for (auto v: numbers) {
      sum += v;
    }
    return sum;
  }

  throw std::invalid_argument(std::format("Unsupported op {}", op));
}

template <std::integral T>
T solve_part1(const std::vector<std::vector<std::string>> &orig_rows,
              const std::vector<char> &ops) {
  T sum = 0;
  for (size_t i=0; i<ops.size(); ++i) {
    std::vector<T> numbers;
    for (auto &n: orig_rows[i]) {
      numbers.push_back(std::stoull(n));
    }

    sum += run_numbers(numbers, ops[i]);
  }
  return sum;
}

template <std::integral T>
T solve_part2(const std::vector<std::vector<std::string>> &orig_rows,
              const std::vector<char> &ops) {
  T sum = 0;
  for (size_t i=0; i<ops.size(); ++i) {
    std::vector<T> numbers;

    for (size_t j=0; j<orig_rows[i][0].size(); ++j) {
      std::string s;
      for (auto &ns: orig_rows[i]) {
        s.push_back(ns[j]);
      }

      numbers.push_back(std::stoull(s));
    }

    sum += run_numbers(numbers, ops[i]);
  }
  return sum;
}

int main(int argc, char *argv[]) {
  try {
    auto [part, filename] = aoc::validate_input(argc, argv);

    auto data = aoc::read_lines(filename);
    auto last = data.back();

    std::vector<size_t> sign_pos;
    std::vector<char> operations;
    for (size_t i=0; i<last.size(); ++i) {
      auto op = last[i];
      if (op != ' ') {
        sign_pos.push_back(i);
        operations.push_back(op);
      }
    }
    sign_pos.push_back(last.size() + 1);
    data.pop_back();

    std::vector<std::vector<std::string>> all_numbers;
    all_numbers.reserve(sign_pos.size()-1);
    for (size_t i=0; i<sign_pos.size()-1; ++i) {
      all_numbers.push_back({});
    }
    for (auto &row: data) {
      for (size_t i=0; i<sign_pos.size()-1; ++i) {
        size_t start = sign_pos[i];
        size_t len = sign_pos[i+1] - sign_pos[i] - 1;
        auto nstr = row.substr(start, len);

        all_numbers[i].push_back(nstr);
      }
    }

    if (part == 1) {
      std::cout << "Part 1: " << solve_part1<unsigned long long>(all_numbers, operations) << "\n";
    } else {
      std::cout << "Part 2: " << solve_part2<unsigned long long>(all_numbers, operations) << "\n";
    }
  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << "\n";
    return -1;
  }

  return 0;
}
