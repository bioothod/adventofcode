from typing import List, Dict, Tuple, Optional

from copy import deepcopy
from dataclasses import dataclass
from enum import IntEnum

import argparse

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

class Direction(IntEnum):
    STAY = 0,
    UP = 1,
    DOWN = 2,
    LEFT = 3,
    RIGHT = 4,

    @staticmethod
    def debug(dir: 'Direction') -> str:
        return '*^v<>'[dir]

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


    def step(self, direction: Direction):
        if direction == Direction.UP:
            self.y -= 1
        elif direction == Direction.DOWN:
            self.y += 1
        elif direction == Direction.LEFT:
            self.x -= 1
        elif direction == Direction.RIGHT:
            self.x += 1

class Blizzard:
    def __init__(self, pos: Coord, direction: Direction):
        self.pos = pos
        self.direction = direction
        self.map_width = 0
        self.map_height = 0

    def update_map_dimensions(self, map_width: int, map_height: int):
        self.map_width = map_width
        self.map_height = map_height

    def step(self):
        self.pos.step(self.direction)

        if self.pos.y == self.map_height - 1:
            self.pos.y = 1
        if self.pos.y == 0:
            self.pos.y = self.map_height - 2

        if self.pos.x == self.map_width - 1:
            self.pos.x = 1
        if self.pos.x == 0:
            self.pos.x = self.map_width - 2

class Node:
    pos: Coord
    distance: int = 0
    neighbours: List['Node'] = []

    def __init__(self, pos: Coord, distance: int):
        self.pos = pos
        self.distance = distance
        self.neighbours = []

    def __repr__(self) -> str:
        return f'{self.pos}: {self.distance}'

    def __hash__(self) -> int:
        return self.pos.__hash__()

    def __eq__(self, other) -> bool:
        return self.pos == other.pos

class Solution:
    def __init__(self, line_generator):
        self.blizzards: List[Blizzard] = []

        self.map_height = 0
        self.map_width = 0
        for y, line in enumerate(line_generator):
            self.map_width = len(line)
            self.map_height += 1

            for x, s in enumerate(line):
                if s == '.':
                    if y == 0:
                        self.expedition = Coord(x, y)
                    else:
                        self.exit_point = Coord(x, y)
                    continue
                if s == '#':
                    continue

                if s == 'E':
                    self.expedition = Coord(x, y)
                    continue

                bpos = Coord(x, y)
                if s == '>':
                    b = Blizzard(bpos, Direction.RIGHT)
                elif s == '<':
                    b = Blizzard(bpos, Direction.LEFT)
                elif s == '^':
                    b = Blizzard(bpos, Direction.UP)
                elif s == 'v':
                    b = Blizzard(bpos, Direction.DOWN)
                else:
                    raise ValueError(f'unsupported blizzard direction "{s}" at {bpos}')

                self.blizzards.append(b)

        for b in self.blizzards:
            b.update_map_dimensions(self.map_width, self.map_height)

        print(f'expedition: {self.expedition} -> {self.exit_point}, blizzards: {len(self.blizzards)}')
        self.debug_print([])

    def debug_print(self, expedition_positions: List[Coord]):
        map = []
        map.append(['#'] * self.map_width)
        for y in range(self.map_height - 2):
            map.append(['#'] + ['.'] * (self.map_width - 2) + ['#'])
        map.append(['#'] * self.map_width)

        exp_pos = set(expedition_positions)
        for b in self.blizzards:
            if b in exp_pos:
                map[b.pos.y][b.pos.x] = 'x'
            else:
                map[b.pos.y][b.pos.x] = Direction.debug(b.direction)

        map[self.expedition.y][self.expedition.x] = 'S'
        map[self.exit_point.y][self.exit_point.x] = 'X'

        for e in expedition_positions:
            map[e.y][e.x] = 'E'

        map_str = [''.join(line) for line in map]
        map_str = '\n'.join(map_str)
        print(map_str)

    def blizzards_step(self):
        for b in self.blizzards:
            b.step()

    def discover_steps(self, current_pos: Coord) -> List[Coord]:
        blizzards = set([b.pos for b in self.blizzards])

        new_directions = []
        for direction in [Direction.UP, Direction.DOWN, Direction.LEFT, Direction.RIGHT, Direction.STAY]:
            pos = deepcopy(current_pos)
            pos.step(direction)
            if pos != self.exit_point and pos != self.expedition:
                if pos.x <= 0 or pos.x >= self.map_width - 1:
                    continue
                if pos.y <= 0 or pos.y >= self.map_height - 1:
                    continue

            #print(f'node: {current_pos} -> {pos}, in blizzards: {pos in blizzards}')
            if pos in blizzards:
                continue

            new_directions.append(pos)

        return new_directions

    def discover_path(self, nodes_to_visit: List[Coord]) -> int:
        start = Node(self.expedition, 0)
        nodes_to_visit_index = 0

        nodes_to_discover = [start]
        while True:
            self.blizzards_step()
            #self.debug_print([n.pos for n in nodes_to_discover])
            #print(f'nodes: {len(nodes_to_discover)}')

            reached = False
            new_nodes = []
            new_nodes_pos = set()
            for node in nodes_to_discover:
                new_directions = self.discover_steps(node.pos)
                #print(f'{node}: new_directions: {len(new_directions)}')
                if len(new_directions) == 0:
                    continue

                new_distance = node.distance + 1
                for new_pos in new_directions:
                    if new_pos in new_nodes_pos:
                        continue

                    #print(f'node: {node}, new_pos: {new_pos}, exit_point: {self.exit_point}')
                    end_node = nodes_to_visit[nodes_to_visit_index]
                    if new_pos == end_node:
                        nodes_to_visit_index += 1
                        #print(f'reached: {end_node}, distance: {new_distance}')
                        if nodes_to_visit_index == len(nodes_to_visit):
                            return new_distance

                        reached = True
                        start = Node(end_node, new_distance)
                        new_nodes = [start]
                        break

                    neigh = Node(new_pos, new_distance)
                    #node.neighbours.append(neigh)
                    new_nodes.append(neigh)
                    new_nodes_pos.add(new_pos)

                if reached:
                    break

            nodes_to_discover = new_nodes

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    parser.add_argument('--part2', action='store_true', help='Whether to run part2')
    FLAGS = parser.parse_args()

    line_generator = load_input(FLAGS.input)
    solution = Solution(line_generator)

    if not FLAGS.part2:
        ret1 = solution.discover_path([solution.exit_point])
        print(f'part1: {ret1}')
    else:
        ret2 = solution.discover_path([solution.exit_point, solution.expedition, solution.exit_point])
        print(f'part2: {ret2}')

if __name__ == '__main__':
    main()
