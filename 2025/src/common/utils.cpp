#include <format>
#include <tuple>
#include <iostream>
#include <fstream>
#include <vector>

namespace aoc {

std::tuple<int, std::string> validate_input(int argc, char *argv[]) {
  if (argc != 3) {
    throw std::invalid_argument(std::format("Usage: {} 1|2 <input file>", argv[0]));
  }

  int part;
  try {
    part = std::stoi(argv[1]);
  } catch (const std::exception &e) {
    throw std::invalid_argument(std::format("stoi({}) failed: {}", argv[1], e.what()));
  }
  if ((part != 1) && (part != 2)) {
    std::string s = std::format("Part number must be 1 or 2, but got {}", part);
    throw std::invalid_argument(s);
  }
  std::string filename = argv[2];

  return {part, filename};
}


std::vector<std::string> read_lines(const std::string& filename) {
  std::ifstream file(filename);
  if (!file) {
    throw std::runtime_error("Cannot open file: " + filename);
  }

  std::vector<std::string> lines;
  std::string line;
  while (std::getline(file, line)) {
    lines.push_back(line);
  }

  return lines;
}

std::pair<std::string, int> split_prefix_number(const std::string& s) {
    size_t i = 0;
    while (i < s.size() && std::isalpha(s[i])) {
        i++;
    }

    std::string prefix = s.substr(0, i);
    int number = (i < s.size()) ? std::stoi(s.substr(i)) : 0;

    return {prefix, number};
}


} // namespace aoc
