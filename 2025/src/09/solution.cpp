#include <algorithm>
#include <climits>
#include <iostream>
#include <map>
#include <set>

#include "utils.hpp"
#include "coords.hpp"


long long solve_part1(const std::vector<aoc::Coord> &coords) {
  long long max_area = 0;

  for (size_t i=0; i < coords.size(); ++i) {
    aoc::Coord a = coords[i];
    for (size_t j=i+1; j < coords.size(); ++j) {
      aoc::Coord b = coords[j];
      auto area = (std::abs(a.x() - b.x()) + 1) * (std::abs(a.y() - b.y()) + 1);

      if (area > max_area) {
        max_area = area;
      }
    }
  }

  return max_area;
}


struct Line {
  aoc::Coord a;
  aoc::Coord b;

  bool x_inside(const aoc::Coord &p) const {
    return x_inside(p.x());
  }
  bool x_inside(const long long x) const {
    auto min_x = std::min(a.x(), b.x());
    auto max_x = std::max(a.x(), b.x());
    return (x >= min_x and x <= max_x);
  }
  bool x_inside_ex(const long long x) const {
    auto min_x = std::min(a.x(), b.x());
    auto max_x = std::max(a.x(), b.x());
    return (x > min_x and x < max_x);
  }

  bool y_inside(const aoc::Coord &p) const {
    return y_inside(p.y());
  }
  bool y_inside(const long long y) const {
    auto min_y = std::min(a.y(), b.y());
    auto max_y = std::max(a.y(), b.y());
    return (y >= min_y and y <= max_y);
  }
  bool y_inside_ex(const long long y) const {
    auto min_y = std::min(a.y(), b.y());
    auto max_y = std::max(a.y(), b.y());
    return (y > min_y and y < max_y);
  }

  bool is_vertical() const {
    return a.x() == b.x();
  }
  bool is_horizontal() const {
    return a.y() == b.y();
  }
};

std::ostream &operator<< (std::ostream &outs, const Line &l) {
  outs << l.a << "->" << l.b;
  return outs;
}
std::ostream &operator<< (std::ostream &outs, const std::pair<long long, Line> &p) {
  auto [pos, l] = p;
  outs << pos << ": " << l.a << "->" << l.b;
  return outs;
}
std::ostream &operator<< (std::ostream &outs, const std::pair<aoc::Coord, aoc::Coord> &p) {
  auto [a, b] = p;
  outs << a << " - " << b;
  return outs;
}

bool inside(const std::vector<Line> &lines, const aoc::Coord &p) {
  bool inside = false;
  for (auto &l: lines) {
    if (l.is_vertical()) {
      if (p.x() == l.a.x() and l.y_inside(p)) {
        return true;
      }

      auto y_max = std::max(l.a.y(), l.b.y());
      auto y_min = std::min(l.a.y(), l.b.y());

      if (l.a.x() > p.x() and p.y() >= y_min and p.y() < y_max) {
        inside = !inside;
      }
    } else {
      if (p.y() == l.a.y() and l.x_inside(p)) {
        return true;
      }
    }
  }

  return inside;
}

bool lines_cross_ex(const Line &l0, const Line &l1) {
  if (l0.is_horizontal()) {
    return (l1.y_inside_ex(l0.a.y()) and l0.x_inside_ex(l1.a.x()));
  } else {
    return (l1.x_inside_ex(l0.a.x()) and l0.y_inside_ex(l1.a.y()));
  }
}

bool inside_polygon(const std::vector<Line> &lines, const aoc::Coord &a, const aoc::Coord &b) {
  auto x_min = std::min(a.x(), b.x());
  auto x_max = std::max(a.x(), b.x());
  auto y_min = std::min(a.y(), b.y());
  auto y_max = std::max(a.y(), b.y());

  auto rect = std::vector<Line>{
    Line({x_min, y_min}, {x_max, y_min}),
    Line({x_max, y_min}, {x_max, y_max}),
    Line({x_max, y_max}, {x_min, y_max}),
    Line({x_min, y_max}, {x_min, y_min}),
  };

  for (auto &rl: rect) {
    if (!inside(lines, rl.a)) {
      return false;
    }

    for (auto &l: lines) {
      if (lines_cross_ex(rl, l)) {
        return false;
      }
    }
  }

  return true;
}

long long solve_part2(const std::vector<aoc::Coord> &coords) {
  std::vector<Line> lines;

  for (size_t i=0; i<coords.size(); ++i) {
    aoc::Coord a = coords[i];
    aoc::Coord b = coords[(i+1) % coords.size()];

    lines.push_back(Line(a, b));
  }

  std::vector<std::tuple<long long, aoc::Coord, aoc::Coord>> areas;
  for (size_t i=0; i < coords.size(); ++i) {
    aoc::Coord a = coords[i];
    for (size_t j=i+1; j < coords.size(); ++j) {
      aoc::Coord b = coords[j];

      auto area = (std::abs(a.x() - b.x()) + 1) * (std::abs(a.y() - b.y()) + 1);
      areas.push_back({area, a, b});
    }
  }

  std::sort(areas.begin(), areas.end(), [](const std::tuple<long long, aoc::Coord, aoc::Coord> &a,
                                           const std::tuple<long long, aoc::Coord, aoc::Coord> &b) {
    return std::get<0>(a) > std::get<0>(b);
  });

  for (auto &[area, a, b]: areas) {
    if (inside_polygon(lines, a, b)) {
      return area;
    }
  }

  return 0;
}


int main(int argc, char *argv[]) {
  try {
    auto [part, filename] = aoc::validate_input(argc, argv);

    auto data = aoc::read_lines(filename);
    std::vector<aoc::Coord> coords;
    for (auto &line: data) {
      int pos = line.find_first_of(',');
      std::string f = line.substr(0, pos);
      std::string l = line.substr(pos+1);
      coords.push_back(aoc::Coord(std::stoll(f), std::stoll(l)));
    }

    if (part == 1) {
      std::cout << "Part 1: " << solve_part1(coords) << "\n";
    } else {
      std::cout << "Part 2: " << solve_part2(coords) << "\n";
    }
  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << "\n";
    return -1;
  }

  return 0;
}
