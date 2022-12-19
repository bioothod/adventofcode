from typing import List, Dict, Tuple

from dataclasses import dataclass
from time import perf_counter

import argparse
import datetime
import itertools

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

@dataclass
class Coord:
    x: int
    y: int

class Rock:
    pattern: List[List[int]]
    pos: Coord

    def __init__(self, coord: Coord):
        self.pattern = []
        self.pos = coord

    def __getitem__(self, pos: Coord):
        y = len(self.pattern) - pos.y - 1
        x = pos.x
        return self.pattern[y][x]


class Rock0(Rock):
    def __init__(self, coord: Coord):
        self.pattern = [[1, 1, 1, 1]]
        self.pos = coord

class Rock1(Rock):
    def __init__(self, coord: Coord):
        self.pattern = [
            [0, 1, 0],
            [1, 1, 1],
            [0, 1, 0],
        ]
        self.pos = coord

class Rock2(Rock):
    def __init__(self, coord: Coord):
        self.pattern = [
            [0, 0, 1],
            [0, 0, 1],
            [1, 1, 1],
        ]
        self.pos = coord

class Rock3(Rock):
    def __init__(self, coord: Coord):
        self.pattern = [
            [1],
            [1],
            [1],
            [1],
        ]
        self.pos = coord

class Rock4(Rock):
    def __init__(self, coord: Coord):
        self.pattern = [
            [1, 1],
            [1, 1],
        ]
        self.pos = coord

class Solution:
    def __init__(self, wind, num_steps):
        self.top_y = 0
        self.rocks = [Rock0, Rock1, Rock2, Rock3, Rock4]
        self.rock_index = 0

        self.wind = wind
        self.wind_index = 0

        self.num_steps = num_steps
        self.chamber_width = 7
        self.chamber_start_y = 0
        self.chamber = [[0] * self.chamber_width for _ in range(4)]

        self.truncate_pattern = []
        self.truncate_meta = []

    def get_rock(self):
        self.rock_index %= len(self.rocks)

        y_pos = self.top_y + 3
        pos = Coord(2, y_pos)
        rock = self.rocks[self.rock_index](pos)
        self.rock_index += 1
        return rock

    def get_wind(self):
        self.wind_index %= len(self.wind)
        wind = self.wind[self.wind_index]
        self.wind_index += 1
        return wind

    def position_allowed(self, rock: Rock) -> bool:
        if rock.pos.y == self.chamber_start_y-1 or rock.pos.x == -1:
            return False

        if rock.pos.x + len(rock.pattern[0]) - 1 == self.chamber_width:
            return False

        for y in range(len(rock.pattern)):
            line = rock.pattern[y]
            for x in range(len(line)):
                chamber_x = x + rock.pos.x
                chamber_y = y + rock.pos.y - self.chamber_start_y

                #print(f'rock: {rock.pattern}, pos: {rock.pos}, chamber: x: {chamber_x}/{self.chamber_width}, y: {chamber_y}/{len(self.chamber)}, chamber_start_y: {self.chamber_start_y}')
                if self.chamber[chamber_y][chamber_x] == 0:
                    continue

                pos = Coord(x, y)
                if rock[pos] == 0:
                    continue

                return False

        return True

    def copy_into_chamber(self, rock: Rock):
        for y in range(len(rock.pattern)):
            line = rock.pattern[y]
            for x in range(len(line)):
                chamber_x = x + rock.pos.x
                chamber_y = y + rock.pos.y - self.chamber_start_y

                pos = Coord(x, y)
                rock_pat = rock[pos]
                if rock_pat == 1:
                    self.chamber[chamber_y][chamber_x] = 1

    def draw(self):
        pattern = '.#'
        str_chamber = []
        for line in self.chamber:
            str_line = [pattern[c] for c in line]
            str_line = ''.join(str_line)
            str_chamber.append(str_line)

        str_chamber = '\n'.join(reversed(str_chamber))
        print(str_chamber)

    def truncate_chamber(self, step: int) -> bool:
        for pos, line in enumerate(reversed(self.chamber)):
            width = sum(line)
            if width == self.chamber_width:
                truncate_at = len(self.chamber) - pos
                self.chamber_start_y += truncate_at
                self.chamber = self.chamber[truncate_at:]

                self.truncate_pattern.append(truncate_at)
                self.truncate_meta.append((step, self.top_y, self.wind_index, self.rock_index, self.chamber_start_y))

                #print(f'truncated at: {truncate_at}, chamber_start_y: {self.chamber_start_y}')
                return True

        return False

    def detect_repetition(self, last_step, num=4) -> int:
        if len(self.truncate_pattern) < num * 3:
            return -1

        last = self.truncate_pattern[-num:]
        for index in range(len(self.truncate_pattern) - len(last) - 1, 0, -1):
            if self.truncate_pattern[index:index+len(last)] == last:
                #print(f'found pattern at {index}: last: {last}, {self.truncate_pattern}')

                tidx = index + len(last) - 1
                step, top_y, wind_index, rock_index, chamber_start_y = self.truncate_meta[tidx]
                step_diff = last_step - step

                steps_to_final = self.num_steps - last_step
                step_jumps = steps_to_final // step_diff

                new_step = last_step + step_jumps * (last_step - step)
                #print(f'last_step: {step}, step: {step}, step_jumps: {step_jumps}, new_step: {new_step}, pattern: {self.truncate_pattern[tidx]}, tidx: {tidx}')

                self.top_y += step_jumps * (self.top_y - top_y)
                self.wind_index += step_jumps * (self.wind_index - wind_index)
                self.rock_index += step_jumps * (self.rock_index - rock_index)

                chamber_diff = self.chamber_start_y - chamber_start_y
                self.chamber_start_y += step_jumps * chamber_diff

                return new_step

        return -1

    def run(self):
        start_time = perf_counter()
        step = 0
        while step < self.num_steps:
            rock = self.get_rock()

            if self.top_y + 3 + len(rock.pattern) >= len(self.chamber):
                for _ in range(3 + len(rock.pattern)):
                    self.chamber.append([0] * self.chamber_width)

            while True:
                wind = self.get_wind()

                prev_x = rock.pos.x
                if wind == '>':
                    rock.pos.x += 1
                elif wind == '<':
                    rock.pos.x -= 1
                else:
                    raise ValueError(f'invalid wind symbol: "{wind}"')

                if not self.position_allowed(rock):
                    rock.pos.x = prev_x

                rock.pos.y -= 1
                if not self.position_allowed(rock):
                    rock.pos.y += 1

                    self.copy_into_chamber(rock)

                    rock_top_y = rock.pos.y + len(rock.pattern)
                    if rock_top_y > self.top_y:
                        self.top_y = rock_top_y

                    step += 1

                    if self.truncate_chamber(step):
                        new_step = self.detect_repetition(step)
                        if new_step > 0:
                            step = new_step


                    break

            if step % 1000 == 0:
                time_per_step = (perf_counter() - start_time) / (step + 1)
                eta = time_per_step * (self.num_steps - step)
                eta = datetime.timedelta(seconds=eta)
                #print(f'steps: {step}, left: {self.num_steps - step}, eta: {eta}, chamber_size: {len(self.chamber)}, time_per_step: {time_per_step*1000:.1f} ms')
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    line_generator = load_input(FLAGS.input)

    wind = next(line_generator)
    solution = Solution(wind, num_steps=2022)
    solution.run()
    print(f'part1: top_y: {solution.top_y}')

    solution = Solution(wind, num_steps=1000000000000)
    solution.run()
    print(f'part2: top_y: {solution.top_y}')

if __name__ == '__main__':
    main()
