from typing import List

import argparse

from enum import Enum
from copy import deepcopy
from dataclasses import dataclass

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

@dataclass
class Step:
    x: int
    y: int

def parse_direction(direction: str) -> Step:
    if direction == 'U':
        return Step(0, 1)
    if direction == 'D':
        return Step(0, -1)
    if direction == 'L':
        return Step(-1, 0)
    if direction == 'R':
        return Step(1, 0)

    raise ValueError(f'unsupported direction {direction}')

class Position:
    def __init__(self):
        self.x = 0
        self.y = 0

    def step(self, step: Step):
        self.x += step.x
        self.y += step.y

    def __repr__(self):
        return f'{self.x}.{self.y}'

    def __eq__(self, other):
        if other.x == self.x and other.y == self.y:
            return True
        return False

    def __hash__(self):
        return hash((self.x, self.y))

def step(head, tail):
    if head > tail:
        return 1
    if head == tail:
        return 0
    return -1

def calculate_tail_direction(head: Position, tail: Position) -> Step:
    ax = abs(head.x - tail.x)
    ay = abs(head.y - tail.y)

    if ax <= 1 and ay <= 1:
        return Step(0, 0)

    return Step(step(head.x, tail.x), step(head.y, tail.y))

def print_map(coords_dict, xr: List[int], yr: List[int]):
    map = []
    for y in range(*yr):
        row = ['.' for _ in range(*xr)]
        map.append(row)

    for l, coords in coords_dict.items():
        for c in coords:
            x = c.x
            y = len(map) - c.y - 1
            map[y + yr[0]][x + xr[0]] = l

    for line in map:
        print(''.join(line))
    print()


class Grid:
    def __init__(self, line_generator, num_tails):
        self.head = Position()

        self.tails = []
        for _ in range(num_tails):
            self.tails.append(Position())

        self.unique_tail_positions = set()
        self.unique_tail_positions.add(self.tails[-1])

        self.process_line(line_generator)

    def process_line(self, line_generator):
        for line in line_generator:
            direction, num_steps = line.split()
            num_steps = int(num_steps)

            direction = parse_direction(direction)
            for _ in range(num_steps):
                self.head.step(direction)

                target = self.head
                for tail in self.tails:
                    tail_step = calculate_tail_direction(target, tail)
                    tail.step(tail_step)
                    target = tail

                state_dict = {
                    'H': [self.head],
                    '#': self.unique_tail_positions,
                }
                for i, tail in enumerate(self.tails):
                    state_dict[str(i)] = [tail]
                #print_map(state_dict, xr=[-15, 15], yr=[-15, 15])

                self.unique_tail_positions.add(deepcopy(self.tails[-1]))

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    grid = Grid(load_input(FLAGS.input), num_tails=1)
    print(f'part1: unique tail positions: {len(grid.unique_tail_positions)}')

    grid = Grid(load_input(FLAGS.input), num_tails=9)
    print(f'part2: unique tail positions: {len(grid.unique_tail_positions)}')

if __name__ == '__main__':
    main()
