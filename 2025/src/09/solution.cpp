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

  std::pair<bool, aoc::Coord> cross_simple(const Line &other) const {
    // only cross is one is vertical and another one is horizontal
    if (a.x() == b.x() and other.a.y() == other.b.y()) {
      if (y_inside(other.a.y()) and other.x_inside(a.x())) {
        return {true, aoc::Coord{a.x(), other.a.y()}};
      }
    } else if (a.y() == b.y() and other.a.x() == other.b.x()) {
      if (x_inside(other.a.x()) and other.y_inside(a.y())) {
        return {true, aoc::Coord{other.a.x(), a.y()}};
      }
    }

    return {false, a};
  }

  std::pair<bool, int> is_inside_point(const aoc::Coord &p) const {
    if (a.x() == b.x()) {
      if (a.y() < b.y()) {
        return {(p.x() <= a.x() and y_inside(p)), 1};
      } else {
        return {(p.x() >= a.x() and y_inside(p)), 2};
      }
      return {false, 0};
    }

    if (a.y() == b.y()) {
      if (a.x() < b.x()) {
        return {(p.y() >= a.y() and x_inside(p)), 3};
      } else {
        return {(p.y() <= a.y() and x_inside(p)), 4};
      }
      return {false, 0};
    }

    return {false, 0};
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

long long cross(const aoc::Coord &s, const aoc::Coord &a, const aoc::Coord &b) {
  auto sa = a - s;
  auto sb = b - s;
  return sa.x()*sb.y() - sa.y()*sb.x();
}

bool on_line(const Line &l, const aoc::Coord &p) {
  if (cross(p, l.a, l.b) != 0) {
    return false;
  }

  if (!l.x_inside(p.x()) or !l.y_inside(p.y())) {
    return false;
  }

  return true;
}

bool inside(const std::vector<Line> &lines, const aoc::Coord &p) {
  for (auto &l: lines) {
    if (on_line(l, p)) {
      return true;
    }
  }

  bool inside = false;
  for (auto &l: lines) {
    if ((l.a.y() > p.y()) != (l.b.y() > p.y())) {
      auto x_cross = l.a.x() + (p.y() - l.a.y()) * (l.b.x() - l.a.x()) / (l.b.y() - l.a.y());
      if (p.x() <= x_cross) {
        inside = !inside;
      }
    }
  }

  return inside;
}

bool lines_cross_ex(const Line &l0, const Line &l1) {
  auto c0 = cross(l0.a, l0.b, l1.a);
  auto c1 = cross(l0.a, l0.b, l1.b);
  auto c2 = cross(l1.a, l1.b, l0.a);
  auto c3 = cross(l1.a, l1.b, l0.b);
  if (c0 == 0 or c1 == 0 or c2 == 0 or c3 == 0) {
    return false;
  }

  return (c0 > 0) != (c1 > 0) and (c2 > 0) != (c3 > 0);
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
  //std::reverse(areas.begin(), areas.end());

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
