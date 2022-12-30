from typing import List, Dict, Tuple, Optional

from dataclasses import dataclass
from collections import defaultdict
from copy import deepcopy

import argparse
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

    def __repr__(self) -> str:
        return f'{self.x}.{self.y}'

    def __hash__(self) -> int:
        return hash((self.x, self.y))

    def __eq__(self, other) -> bool:
        return self.x == other.x and self.y == other.y

class Elf:
    def __init__(self, x: int, y: int):
        self.pos = Coord(x, y)

        self.check_positions = [
            self.check_north,
            self.check_south,
            self.check_west,
            self.check_east,
        ]
        self.first_direction_index = 0

    def __hash__(self):
        return self.pos.__hash__()
    def __eq__(self, other):
        return self.pos == other.pos

    def check_pos_in_coords(self, coords, elves) -> Tuple[bool, Coord]:
        for c in coords:
            e = Elf(c.x, c.y)
            if e in elves:
                return False, self.pos

        return True, coords[1]

    def check_north(self, elves) -> Tuple[bool, Coord]:
        coords = [
            Coord(self.pos.x-1, self.pos.y-1),
            Coord(self.pos.x  , self.pos.y-1),
            Coord(self.pos.x+1, self.pos.y-1),
        ]

        return self.check_pos_in_coords(coords, elves)

    def check_south(self, elves) -> Tuple[bool, Coord]:
        coords = [
            Coord(self.pos.x-1, self.pos.y+1),
            Coord(self.pos.x  , self.pos.y+1),
            Coord(self.pos.x+1, self.pos.y+1),
        ]

        return self.check_pos_in_coords(coords, elves)

    def check_west(self, elves) -> Tuple[bool, Coord]:
        coords = [
            Coord(self.pos.x-1, self.pos.y-1),
            Coord(self.pos.x-1, self.pos.y),
            Coord(self.pos.x-1, self.pos.y+1),
        ]

        return self.check_pos_in_coords(coords, elves)

    def check_east(self, elves) -> Tuple[bool, Coord]:
        coords = [
            Coord(self.pos.x+1, self.pos.y-1),
            Coord(self.pos.x+1, self.pos.y),
            Coord(self.pos.x+1, self.pos.y+1),
        ]

        return self.check_pos_in_coords(coords, elves)

    def has_neighbours(self, elves) -> bool:
        coords = [
            Coord(self.pos.x-1, self.pos.y-1),
            Coord(self.pos.x  , self.pos.y-1),
            Coord(self.pos.x+1, self.pos.y-1),
            Coord(self.pos.x-1, self.pos.y  ),
            Coord(self.pos.x+1, self.pos.y  ),
            Coord(self.pos.x-1, self.pos.y+1),
            Coord(self.pos.x  , self.pos.y+1),
            Coord(self.pos.x+1, self.pos.y+1),
        ]

        for c in coords:
            e = Elf(c.x, c.y)
            if e in elves:
                return True

        return False

    def move(self, elves) -> Tuple[bool, Coord]:
        pos = self.pos
        can_move = False
        have_to_move = False

        if self.has_neighbours(elves):
            have_to_move = True
            for check_position_index in range(self.first_direction_index, self.first_direction_index+len(self.check_positions)):
                check_position_index %= len(self.check_positions)

                can_move, pos = self.check_positions[check_position_index](elves)
                #print(f'{self.pos}: check_position_index: {check_position_index}, can_move: {can_move}, new_pos: {pos}')
                if can_move:
                    break

        self.first_direction_index += 1
        self.first_direction_index %= len(self.check_positions)

        if can_move:
            #print(f'{self.pos}: can move -> {pos}')
            return have_to_move, pos

        return have_to_move, self.pos

    def update_coord(self, new_pos: Coord):
        #print(f'elf: {self.pos} -> {new_pos}')
        self.pos = new_pos

class Solution:
    def __init__(self, line_generator):
        self.orig_elves = set()

        for y, line in enumerate(line_generator):
            for x, symbol in enumerate(line):
                if symbol == '#':
                    self.orig_elves.add(Elf(x, y))
        self.reset()

    def reset(self):
        self.elves = deepcopy(self.orig_elves)

    def print(self, print_all=False):
        min_x = min(self.elves, key=lambda elf: elf.pos.x).pos.x
        max_x = max(self.elves, key=lambda elf: elf.pos.x).pos.x
        min_y = min(self.elves, key=lambda elf: elf.pos.y).pos.y
        max_y = max(self.elves, key=lambda elf: elf.pos.y).pos.y

        if print_all:
            map = []
            for y in range(min_y, max_y+1):
                line = ['.'] * (max_x - min_x + 1)
                map.append(line)

            for elf in self.elves:
                x = elf.pos.x - min_x
                y = elf.pos.y - min_y

                map[y][x] = '#'

            map_str = [''.join(line) for line in map]
            map_str = '\n'.join(map_str)
            print(map_str)
            print()

        space = (max_y - min_y + 1) * (max_x - min_x + 1)
        space -= len(self.elves)
        return space

    def run(self, num_rounds) -> Tuple[int, int]:
        num_free_spaces = 0
        round = 1
        for round in range(1, num_rounds+1):
            next_moves = defaultdict(list)

            num_still = 0
            for elf in self.elves:
                have_to_move, new_pos = elf.move(self.elves)
                if not have_to_move:
                    num_still += 1

                next_moves[new_pos].append(elf)

            if num_still == len(self.elves):
                break

            new_elves = set()
            for next_coord, elves in next_moves.items():
                if len(elves) == 1:
                    elf = elves[0]
                    elf.update_coord(next_coord)
                    new_elves.add(elf)
                    continue

                for elf in elves:
                    new_elves.add(elf)

            self.elves = new_elves

        num_free_spaces = self.print(print_all=True)
        return num_free_spaces, round

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    line_generator = load_input(FLAGS.input)
    solution = Solution(line_generator)

    num_free_spaces, num_rounds = solution.run(10)
    print(f'part1: {num_free_spaces}')

    solution.reset()
    num_free_spaces, num_rounds = solution.run(10000000)
    print(f'part2: {num_rounds}')

if __name__ == '__main__':
    main()
