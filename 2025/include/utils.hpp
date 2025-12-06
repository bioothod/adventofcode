#pragma once

#include <vector>
#include <string>
#include <tuple>
#include <iostream>

namespace aoc {
  std::tuple<int, std::string> validate_input(int argc, char *argv[]);
  std::vector<std::string> read_lines(const std::string& filename);
  std::pair<std::string, int> split_prefix_number(const std::string& s);

  template <std::integral T> T combine(const std::vector<T> &value);
  template <std::integral T> std::vector<T> split(T value);

  unsigned long long quick_pow10(int n);
}

namespace stq
{
    template<typename T>
    void println(auto, const T& xz)
    {
        std::cout << '[';
        bool first{true};
        for (const auto& x : xz)
            std::cout << (first ? first = false, "" : ", ") << x;
        std::cout << "]\n";
    }
} // namespace stq


#include "utils.tpp"
