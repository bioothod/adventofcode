from typing import List, Dict, Tuple

from dataclasses import dataclass

import argparse
import re

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

@dataclass
class Cube:
    x: int
    y: int
    z: int

    def __repr__(self) -> str:
        return f'{self.x}.{self.y}.{self.z}'

    def __eq__(self, other: 'Cube') -> bool:
        return self.x == other.x and self.y == other.y and self.z == other.z

    def __hash__(self) -> int:
        return hash((self.x, self.y, self.z))

    def neighbours(self) -> List['Cube']:
        x = self.x
        y = self.y
        z = self.z

        neighbours = [
            [x, y, z-1],
            [x, y, z+1],
            [x, y-1, z],
            [x, y+1, z],
            [x-1, y, z],
            [x+1, y, z],
        ]

        return [Cube(*coords) for coords in neighbours]

class Solution:
    def __init__(self, line_generator):
        self.cubes = set()
        self.area = 0

        for line in line_generator:
            x, y, z = map(int, line.split(','))
            cube = Cube(x, y, z)

            self.cubes.add(cube)
            self.area += 6

            for c in cube.neighbours():
                if c in self.cubes:
                    self.area -= 2

        print(f'part1: area: {self.area}')

    def expand_cover(self):
        min_x = min(*self.cubes, key=lambda c: c.x)
        max_x = max(*self.cubes, key=lambda c: c.x)
        min_y = min(*self.cubes, key=lambda c: c.y)
        max_y = max(*self.cubes, key=lambda c: c.y)
        min_z = min(*self.cubes, key=lambda c: c.z)
        max_z = max(*self.cubes, key=lambda c: c.z)

        cover_nodes = set()
        external_nodes = set()

        min_x = min_x.x - 1
        max_x = max_x.x + 1
        min_y = min_y.y - 1
        max_y = max_y.y + 1
        min_z = min_z.z - 1
        max_z = max_z.z + 1

        start = Cube(min_x, min_y, min_z)

        nodes = [start]
        while len(nodes) > 0:
            nodes_to_explore = set()
            for node in nodes:
                if node in external_nodes:
                    continue
                if node in self.cubes:
                    cover_nodes.add(node)
                    continue

                external_nodes.add(node)

                for nn in node.neighbours():
                    if nn.x < min_x or nn.y < min_y or nn.z < min_z:
                        continue
                    if nn.x > max_x or nn.y > max_y or nn.z > max_z:
                        continue
                    nodes_to_explore.add(nn)

            nodes = nodes_to_explore

        area = self.area
        for x in range(min_x, max_x+1):
            for y in range(min_y, max_y+1):
                for z in range(min_z, max_z+1):
                    c = Cube(x, y, z)
                    if c in self.cubes:
                        continue
                    if c in external_nodes:
                        continue

                    trapped = c
                    for n in trapped.neighbours():
                        if n in self.cubes:
                            area -= 1

        print(f'part2: cover_area: {area}')

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    line_generator = load_input(FLAGS.input)
    solution = Solution(line_generator)
    solution.expand_cover()

if __name__ == '__main__':
    main()
