from typing import List, Dict

from copy import deepcopy
from time import perf_counter

import argparse

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

class Coord:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __repr__(self) -> str:
        return f'{self.x}.{self.y}'

    def __hash__(self) -> int:
        return hash((self.x, self.y))

    def __eq__(self, other: 'Coord') -> bool:
        if other == None:
            return False

        return self.x == other.x and self.y == other.y

class Map:
    def __init__(self, line_generator):
        self.start = Coord(500, 0)

        self.rocks = set()

        for line in line_generator:
            prev = None

            coords = line.split(' -> ')
            for coord in coords:
                x, y = map(int, coord.split(','))
                c = Coord(x, y)

                if prev == None:
                    prev = c
                    continue

                if prev.x != c.x:
                    min_x = min(prev.x, c.x)
                    max_x = max(prev.x, c.x)
                    for x in range(min_x, max_x+1):
                        self.rocks.add(Coord(x, prev.y))
                if prev.x != c.y:
                    min_y = min(prev.y, c.y)
                    max_y = max(prev.y, c.y)
                    for y in range(min_y, max_y+1):
                        self.rocks.add(Coord(prev.x, y))

                prev = c

        self.min_x = min(*self.rocks, key=lambda c: c.x).x
        self.max_x = max(*self.rocks, key=lambda c: c.x).x
        self.min_y = min(*self.rocks, key=lambda c: c.y).y
        self.max_y = max(*self.rocks, key=lambda c: c.y).y

        print(f'min_x: {self.min_x}, min_y: {self.min_y}, max_x: {self.max_x}, max_y: {self.max_y}, rocks: {len(self.rocks)}')

        self.orig_rocks = self.rocks.copy()

    def reset(self):
        self.rocks = self.orig_rocks.copy()

    def print(self):
        plane = ['.'] * (self.max_x - self.min_x + 2)
        map = []
        for y in range(self.min_y, self.max_y+1):
            map.append(deepcopy(plane))

        for r in self.rocks:
            map[r.y-self.min_y][r.x-self.min_x+1] = '#'

        str_map = []
        for line in map:
            str_map.append(''.join(line))
        str_map = '\n'.join(str_map)
        print(str_map)

    def add_floor_check(self, c: Coord):
        if c.y == self.max_y + 1:
            return True
        return False

    def check_blocked(self, c: Coord) -> bool:
        return c in self.rocks

    def check_abyss(self, c: Coord) -> bool:
        if c.x < self.min_x or c.x > self.max_x:
            return True
        if c.y > self.max_y:
            return True
        return False

    def pour_one_blocked(self, add_floor_check=False) -> bool:
        c = self.start
        while True:
            if not add_floor_check:
                if self.check_abyss(c):
                    break

            next_coords = [
                Coord(c.x, c.y+1),
                Coord(c.x-1, c.y+1),
                Coord(c.x+1, c.y+1),
            ]

            blocked = True
            for n in next_coords:
                if not self.check_blocked(n):
                    c = n
                    blocked = False
                    break

            if not blocked and add_floor_check:
                blocked = self.add_floor_check(c)

            if blocked:
                self.rocks.add(c)

                if c == self.start:
                    return False

                return True

        return False

    def pour(self, add_floor_check=False):
        num_blocked = 0
        while self.pour_one_blocked(add_floor_check):
            num_blocked += 1

        if add_floor_check:
            num_blocked += 1

        print(f'blocked: {num_blocked}')

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    line_generator = load_input(FLAGS.input)
    map = Map(line_generator)
    map.pour()

    map.reset()
    map.pour(add_floor_check=True)

if __name__ == '__main__':
    main()
