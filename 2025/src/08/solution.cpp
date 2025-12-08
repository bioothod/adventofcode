#include <cmath>
#include <iostream>
#include <map>
#include <sstream>
#include <stdexcept>

#include "utils.hpp"

struct Coord {
  long long x;
  long long y;
  long long z;

  bool operator<(const Coord& o) const {
    if (x < o.x) {
      return true;
    }

    if (x == o.x and y < o.y) {
      return true;
    }

    if (x == o.x and y == o.y and z < o.z) {
      return true;
    }

    return false;
  }

  bool operator==(const Coord& o) const {
    return (x == o.x) and (y == o.y) and (z == o.z);
  }
};

std::ostream & operator << (std::ostream &outs, const Coord &c) {
    return outs << c.x << "." << c.y << "." << c.z;
}

static Coord parse_line(const std::string &line) {
  std::stringstream ss(line);
  std::string token;
  std::vector<ssize_t> coords;
  while (std::getline(ss, token, ',')) {
    coords.push_back(std::stoll(token));
  }

  if (coords.size() != 3) {
    throw std::invalid_argument("Could not parse 3 coords from " + line);
  }

  return {coords[0], coords[1], coords[2]};
}

unsigned long long solve(const std::vector<Coord> &coords, unsigned long max_steps, int part_num) {
  std::map<unsigned long long, std::vector<std::pair<Coord, Coord>>> all_dists;
  std::map<Coord, unsigned long> coord2circuits;
  std::map<unsigned long, std::vector<Coord>> circuits;
  unsigned long circuit_id = 0;

  for (; circuit_id<coords.size(); ++circuit_id) {
    Coord a = coords[circuit_id];
    coord2circuits.emplace(a, circuit_id);
    circuits.emplace(circuit_id, std::vector<Coord>{a});

    for (size_t i=circuit_id+1; i<coords.size(); ++i) {
      Coord b = coords[i];

      unsigned long long dist = std::powl(a.x-b.x, 2) + std::powl(a.y-b.y, 2) + std::powl(a.z-b.z, 2);

      auto it = all_dists.find(dist);
      if (it == all_dists.end()) {
        std::vector<std::pair<Coord, Coord>> p = {{a, b}};
        all_dists.emplace(dist, p);
      } else {
        it->second.push_back({a, b});
      }
    }
  }

  std::pair<Coord, Coord> last_pair;
  size_t connections_made = 0;
  while (connections_made++ != max_steps and circuits.size() != 1) {
    auto it = all_dists.begin();
    auto [a, b] = it->second.back();
    last_pair = {a, b};

    auto ca = coord2circuits.find(a);
    auto cb = coord2circuits.find(b);

    it->second.pop_back();
    if (it->second.empty()) {
      all_dists.erase(it);
    }

    if (ca->second == cb->second) {
      continue;
    }

    auto circa = circuits.find(ca->second);
    auto circb = circuits.find(cb->second);

    circa->second.insert(circa->second.end(), circb->second.begin(), circb->second.end());

    for (auto &c: circb->second) {
      coord2circuits[c] = ca->second;
    }
    circuits.erase(circb);
    cb->second = ca->second;
  }

  if (part_num == 1) {
    std::vector<unsigned long> sizes;
    for (auto &circ : circuits) {
      sizes.push_back(circ.second.size());
    }

    std::sort(sizes.begin(), sizes.end());
    size_t num = 3;
    unsigned long long ret = 1;
    for (auto it = sizes.rbegin(); it != sizes.rend(); ++it) {
      ret *= *it;
      if (--num == 0) {
        break;
      }
    }
    return ret;
  } else {
    auto [a, b] = last_pair;
    return a.x * b.x;
  }
}

int solve_part2() {
  return 2;
}

int main(int argc, char *argv[]) {
  try {
    auto [part, filename] = aoc::validate_input(argc, argv);

    auto data = aoc::read_lines(filename);
    std::vector<Coord> coords;
    for (auto &line: data) {
      coords.push_back(parse_line((line)));
    }

    if (part == 1) {
      std::cout << "Part 1: " << solve(coords, 1000, 1) << "\n";
    } else {
      std::cout << "Part 2: " << solve(coords, 999999999999999, 2) << "\n";
    }
  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << "\n";
    return -1;
  }

  return 0;
}
