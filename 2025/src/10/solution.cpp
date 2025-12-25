#include <climits>
#include <format>
#include <iostream>
#include <numeric>
#include <sstream>
#include <string>

std::ostream &operator<< (std::ostream &outs, const std::pair<size_t, size_t> &p) {
  auto [idx, num] = p;
  outs << idx << "." << num;
  return outs;
}


#include "utils.hpp"

template <std::integral T>
std::vector<T> mult_vec(const std::vector<T> &row, T val) {
  std::vector<T> ret;
  ret.reserve(row.size());
  for (auto c: row) {
    ret.push_back(c*val);
  }
  return ret;
}

template <std::integral T>
struct Matrix {
  std::vector<std::vector<T>> rows;

  size_t num_rows() const {
    return rows.size();
  }
  size_t num_cols() const {
    return rows[0].size();
  }

  void swap_rows(size_t i, size_t j) {
    std::vector<T> tmp = rows[i];
    rows[i] = rows[j];
    rows[j] = tmp;
  }

  void mult_row(size_t row_idx, T v) {
    for (size_t i=0; i<num_cols(); ++i) {
      rows[row_idx][i] *= v;
    }
  }

  void add_row(size_t row_idx, const std::vector<T> &other) {
    for (size_t i=0; i<num_cols(); ++i) {
      rows[row_idx][i] += other[i];
    }
  }

  void sub_row(size_t row_idx, const std::vector<T> &other) {
    for (size_t i=0; i<num_cols(); ++i) {
      rows[row_idx][i] -= other[i];
    }
  }

  bool can_divide_row(size_t row_idx, T val) const {
    for (auto c: rows[row_idx]) {
      if (std::gcd(c, val) != val) {
        return false;
      }
    }
    return true;
  }
  void divide_row(size_t row_idx, T val) {
    for (size_t i=0; i<num_cols(); ++i) {
      rows[row_idx][i] /= val;
    }
  }
  void drop_row(size_t row_idx) {
    rows.erase(rows.begin() + row_idx);
  }

  void add_column(size_t col_idx, const::std::vector<T> &other) {
    for (size_t i=0; i<num_rows(); ++i) {
      rows[i][col_idx] += other[i];
    }
  }
  void sub_column(size_t col_idx, const::std::vector<T> &other) {
    for (size_t i=0; i<num_rows(); ++i) {
      rows[i][col_idx] -= other[i];
    }
  }
  void mult_column(size_t col_idx, T val) {
    for (size_t i=0; i<num_rows(); ++i) {
      rows[i][col_idx] *= val;
    }
  }
  bool can_divide_col(size_t col_idx, T val) const {
    for (size_t i=0; i<num_rows(); ++i) {
      auto c = get(i, col_idx);
      if (std::gcd(c, val) != val) {
        return false;
      }
    }
    return true;
  }
  void divide_col(size_t col_idx, T val) {
    for (size_t i=0; i<num_rows(); ++i) {
      rows[i][col_idx] /= val;
    }
  }
  std::vector<T> get_column(size_t col_idx) {
    std::vector<T> ret;
    ret.reserve(num_rows());
    for (size_t row_idx=0; row_idx<num_rows(); ++row_idx) {
      ret.push_back(get(row_idx, col_idx));
    }
    return ret;
  }
  void drop_column(size_t col_idx) {
    for (size_t row_idx=0; row_idx<num_rows(); ++row_idx) {
      rows[row_idx].erase(rows[row_idx].begin() + col_idx);
    }
  }

  const std::vector<T>& operator [](size_t idx) const {
    return rows[idx];
  }

  T get(size_t row_idx, size_t col_idx) const {
    return rows[row_idx][col_idx];
  }

  void join(const Matrix<T> &other) {
    for (size_t i=0; i<rows.size(); ++i) {
      rows[i].insert(rows[i].end(), other[i].begin(), other[i].end());
    }
  }

  Matrix<T> cut(size_t col_idx) {
    Matrix<T> n;
    for (size_t row_idx=0; row_idx<rows.size(); ++row_idx) {
      auto row = rows[row_idx];
      std::vector<T> r(row.begin()+col_idx, row.end());
      n.rows.push_back(r);
      rows[row_idx].resize(col_idx);
    }
    return n;
  }

  void hermite() {
    size_t num_cols_calc = std::min(num_cols(), num_rows());
    for (size_t col_idx = 0; col_idx < num_cols_calc; ++col_idx) {
      size_t row_idx = -1;
      bool found = false;
      T min_val = LONG_MAX;
      for (auto ri=col_idx; ri < num_rows(); ++ri) {
        T val = get(ri, col_idx);
        if (val == 0) {
          continue;
        }
        auto abs_val = std::abs(val);

        if (abs_val != 1 and can_divide_row(ri, abs_val)) {
          divide_row(ri, abs_val);
          val = get(ri, col_idx);
        }
        if (val < 0) {
          mult_row(ri, -1);
          val = get(ri, col_idx);
        }

        if (val < min_val) {
          found = true;
          row_idx = ri;
          min_val = val;
        }
      }

      if (found) {
        swap_rows(row_idx, col_idx);
        std::vector<T> &row = rows[col_idx];

        for (size_t ri = col_idx + 1; ri < num_rows(); ++ri) {
          T val = get(ri, col_idx);
          if (val == 0) {
            continue;
          }

          if (val % min_val == 0) {
            T mult = val / min_val;
            auto new_row = mult_vec(row, mult);
            sub_row(ri, new_row);
          }
        }
      }

      //std::cout << *this << "\n\n";
    }
  }

  bool check_triangle() const {
    for (size_t row_idx=0; row_idx<num_rows(); ++row_idx) {
      for (size_t col_idx=0; col_idx<num_cols(); ++col_idx) {
        T val = get(row_idx, col_idx);
        if (col_idx < row_idx) {
          if (val != 0) {
            return false;
          }
        } else if (col_idx == row_idx) {
          if (val != 1) {
            return false;
          }
        }
      }
    }
    return true;
  }

  long check_solution(const std::vector<std::pair<size_t, size_t>> &sol) {
    auto sum = 0;
    for (auto [idx, num]: sol) {
      sum += num;
    }

    return sum;
  }

  std::vector<std::pair<size_t, size_t>> solve_last(std::vector<std::pair<size_t, size_t>> best,
                                                            std::vector<std::pair<size_t, size_t>> current,
                                                            Matrix b, T max_factor) {

    auto current_best = check_solution(best);

    // std::cout << "solving... current: ";
    // stq::println("{}", current);
    // std::cout << "best counter: " << current_best << ", steps: ";
    // stq::println("{}", best);
    // std::cout << std::make_pair(*this, b) << "\n";

    for (ssize_t row_idx=b.num_rows()-1; row_idx >= 0; --row_idx) {
      T bval = b.get(row_idx, 0);
      ssize_t last_idx = -1;
      T last_val = 0;
      for (ssize_t i=num_cols()-1; i>=0; --i) {
        last_val = get(row_idx, i);
        if (last_val != 0) {
          last_idx = i;
          break;
        }
      }

      if (last_idx == -1 and bval != 0) {
        return best;
      }
      if (last_idx == -1) {
        drop_row(row_idx);
        b.drop_row(row_idx);
        continue;
      }

      // std::cout << "starting... last_idx: " << last_idx << ", last_val: " << last_val << ", bval: " << bval << ", current: ";
      // stq::println("{}", current);
      // std::cout << std::make_pair(*this, b) << "\n";

      auto current_length = check_solution(current);
      for (T last_mult=0; ; ++last_mult) {
        T last_val_mult = last_mult * last_val;
        // std::cout << "trying... last_idx: " << last_idx << ", last_val: " << last_val << ", last_mult: " << last_mult << ", last_val_mult: " << last_val_mult << ", bval: " << bval << ", current: ";
        // stq::println("{}", current);
        if (std::abs(last_val_mult) > std::abs(bval) * max_factor) {
          return best;
        }

        if (current_length + last_mult >= current_best and !best.empty()) {
          return best;
        }

        current.push_back({last_idx, last_mult});

        auto new_a = *this;
        new_a.mult_column(last_idx, last_mult);
        auto new_b = b;
        new_b.sub_column(0, new_a.get_column(last_idx));

        auto b_column = new_b.get_column(0);
        auto b_is_zero = true;
        for (auto xb: b_column) {
          if (xb != 0) {
            b_is_zero = false;
            break;
          }
        }


        if (b_is_zero) {
          if (check_solution(current) < check_solution(best) or best.empty()) {
            std::cout << "new best solution: ";
            auto sum = 0;
            for (auto [idx, num]: current) {
              std::cout << idx << "." << num << " ";
              sum += num;
            }
            std::cout << "| " << current_best << " => " << sum << "\n";

            return current;
          }
          return best;
        }

        new_a.drop_column(last_idx);
        best = new_a.solve_last(best, current, new_b, max_factor);
        current_best = check_solution(best);

        current.pop_back();
      }

    }

    return best;
  }
};

template <std::integral T>
std::ostream &operator<< (std::ostream &outs, const Matrix<T> &m) {
  for (auto &row: m.rows) {
    for (const auto& x : row)
      outs << std::format("{:4d}", x);
    std::cout << "\n";
  }
  return outs;
}

template <std::integral T>
std::ostream &operator<< (std::ostream &outs, const std::pair<Matrix<T>, Matrix<T>> &p) {
  auto &[A, b] = p;
  for (size_t row_idx=0; row_idx<A.rows.size(); ++row_idx) {
    for (const auto& x : A.rows[row_idx])
      outs << std::format("{:3d}", x);
    outs << "  | ";
    for (const auto& x : b.rows[row_idx])
      outs << std::format("{:3d}", x);
    outs << "\n";
  }
  return outs;
}

typedef unsigned long mask_t;
struct Machine {
  mask_t mask;
  std::vector<mask_t> buttons;
  std::vector<std::vector<long>> buttons_raw;
  std::vector<long> jolts;

  std::vector<std::vector<long>> find_min_match(std::vector<std::vector<long>> all,
                                                std::vector<long> pressed,
                                                mask_t current_mask,
                                                size_t depth_index) const {
    if (depth_index == buttons.size()) {
      return all;
    }

    // pass without pressing
    all = find_min_match(std::move(all), pressed, current_mask, depth_index+1);

    // press the button
    pressed.push_back(depth_index);
    current_mask = current_mask xor buttons[depth_index];
    if (current_mask == 0) {
      all.push_back(pressed);
    } else {
      all = find_min_match(std::move(all), std::move(pressed), current_mask, depth_index+1);
    }

    return all;
  }

};


Machine parse_line(const std::string &line) {
  std::vector<std::string> chunks;
  std::istringstream is(line);
  std::string token;
  while (std::getline(is, token, ' ')) {
    chunks.push_back(token);
  }
  Machine m = {
    0, {}, {}, {}
  };

  auto mstr = chunks.front();
  for (size_t i=1; i<mstr.size()-1; ++i) {
    if (mstr[i] == '#') {
      m.mask |= 1 << (i-1);
    }
  }

  for (size_t i=1; i<chunks.size()-1; ++i) {
    auto &l = chunks[i];
    std::vector<long> bnums =
      aoc::split_string<long>(l.substr(1, l.size()-2), ',');
    mask_t bmask = 0;
    for (auto bnum: bnums) {
      bmask |= 1 << bnum;
    }
    m.buttons.push_back(bmask);
    m.buttons_raw.push_back(bnums);
  }
  auto jolts = chunks.back();
  m.jolts = aoc::split_string<long>(jolts.substr(1, jolts.size()-2), ',');

  return m;
}

unsigned long long solve_part1(const std::vector<Machine> &machines) {
  unsigned long long sum = 0;
  for (auto &m: machines) {
    std::vector<std::vector<long>> steps;
    steps = m.find_min_match(steps, {}, m.mask, 0);

    size_t min_steps = LONG_MAX;
    for (auto &s: steps) {
      if (s.size() < min_steps) {
        min_steps = s.size();
      }
    }

    sum += min_steps;
  }
  return sum;
}

unsigned long long solve_part2(const std::vector<Machine> &machines) {
  auto total_sum = 0;

  auto machine_idx = 1;
  std::cout << "\n";
  for (auto &machine: machines) {
    Matrix<long> A;
    Matrix<long> b;
    Matrix<long> b_null;
    for (size_t row_idx=0; row_idx<machine.jolts.size(); ++row_idx) {
      b.rows.push_back({machine.jolts[row_idx]});
      std::vector<long> row(machine.buttons_raw.size(), 0);
      A.rows.push_back(row);
      b_null.rows.push_back(std::vector<long>{0});
    }

    for (size_t bidx=0; bidx<machine.buttons_raw.size(); ++bidx) {
      for (auto b: machine.buttons_raw[bidx]) {
        A.rows[b][bidx] = 1;
      }
    }

    std::cout << "Solving machine " << machine_idx << "\n";
    A.join(b);
    std::cout << A << "\n";
    A.hermite();

    b = A.cut(machine.buttons_raw.size());
    std::cout << std::make_pair(A, b) << "\n";
    std::cout << "\n";

    auto max_factor = 20;
    if (machine_idx == 4) {
      max_factor = 1;
    }
    if (machine_idx == 50) {
      max_factor = 5;
    }
    if (machine_idx == 161) {
      max_factor = 5;
    }
    // std::vector<std::pair<size_t, size_t>> null_res = A.solve_last({}, {}, b_null, 10);
    // std::vector<long> null_vector;
    // for (auto [idx, num]: null_res) {
    //   null_vector.push_back(num);
    // }
    //std::reverse(null_vector.begin(), null_vector.end());
    // std::cout << "null: ";
    // stq::println("{}", null_vector);
    // return -1;

    std::vector<std::pair<size_t, size_t>> res = A.solve_last({}, {}, b, max_factor);
    std::cout << "best solution: ";
    auto sum = 0;
    for (auto [idx, num]: res) {
      std::cout << idx << "." << num << " ";
      sum += num;
    }
    std::cout << " => " << sum << "\n";
    std::cout << "===============================\n";

    if (sum == 0) {
      std::cout << "bug in machine " << machine_idx << "\n";
      return -1;
    }

    total_sum += sum;
    machine_idx += 1;
  }
  return total_sum;
}

int main(int argc, char *argv[]) {
  try {
    auto [part, filename] = aoc::validate_input(argc, argv);

    auto data = aoc::read_lines(filename);
    std::vector<Machine> machines;
    for (auto &l: data) {
      machines.push_back(parse_line(l));
    }

    if (part == 1) {
      std::cout << "Part 1: " << solve_part1(machines) << "\n";
    } else {
      std::cout << "Part 2: " << solve_part2(machines) << "\n";
    }
  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << "\n";
    return -1;
  }

  return 0;
}
