#include <vector>
#include <string>
#include <tuple>

namespace aoc {
  std::tuple<int, std::string> validate_input(int argc, char *argv[]);
  std::vector<std::string> read_lines(const std::string& filename);
  std::pair<std::string, int> split_prefix_number(const std::string& s);
}
