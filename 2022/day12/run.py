from typing import List, Dict, Tuple, Set

import argparse

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

class Coord:
    def __init__(self, x: int, y: int):
        self.x = x
        self.y = y

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __hash__(self):
        return hash((self.x, self.y))

    def __repr__(self):
        return f'{self.x}.{self.y}'

class Point:
    def __init__(self, pos: Coord, height: int):
        self.pos = pos
        self.height = height
        self.min_start_distance = 10**10
        self.is_visited = False

        self.can_reach: Dict[Coord, Point] = {}

    def clear(self):
        self.min_start_distance = 10**10
        self.is_visited = False

    def add_can_reach(self, point: 'Point'):
        if point not in self.can_reach:
            self.can_reach[point.pos] = point

class ExplorationMap:
    def __init__(self, line_generator):
        heights = 'abcdefghijklmnopqrstuvwxyz'

        self.map: List[List[Point]] = []

        for y, line in enumerate(line_generator):
            explored_line = []
            for x, l in enumerate(line):
                if l == 'S':
                    self.start_pos = Coord(x, y)
                    height = heights.index('a')
                elif l == 'E':
                    self.end_pos = Coord(x, y)
                    height = heights.index('z')
                else:
                    height = heights.index(l)

                explored_line.append(Point(Coord(x, y), height))

            self.map.append(explored_line)

        #print(f'start_pos: {self.start_pos}, end: {self.end_pos}, map_size: {self.sizes()}')

    def sizes(self) -> Tuple[int, int]:
        return (len(self.map), len(self.map[0]))

    def __getitem__(self, pos: Coord) -> Point:
        return self.map[pos.y][pos.x]

    def height(self, pos: Coord) -> int:
        return self[pos].height

    def contains(self, pos: Coord) -> bool:
        if pos.y < 0 or pos.y >= len(self.map):
            return False
        if pos.x < 0 or pos.x >= len(self.map[pos.y]):
            return False
        return True

    def clear_visited(self):
        y_map_size, x_map_size = self.sizes()

        for y in range(y_map_size):
            for x in range(x_map_size):
                self.map[y][x].clear()

class Map:
    def __init__(self, line_generator):
        self.map = ExplorationMap(line_generator)

        self.start_pos = self.map[self.map.start_pos].pos
        self.end_pos = self.map[self.map.end_pos].pos

        self.map[self.start_pos].min_start_distance = 0

        self.connect()

    def connect(self):
        y_map_size, x_map_size = self.map.sizes()

        for y in range(y_map_size):
            for x in range(x_map_size):
                coord = Coord(x, y)
                n = self.map[coord]

                steps = [
                    Coord(x, y+1),
                    Coord(x, y-1),
                    Coord(x+1, y),
                    Coord(x-1, y),
                ]

                for pos in steps:
                    if not self.map.contains(pos):
                        continue

                    dst_point = self.map[pos]

                    if dst_point.height <= n.height + 1:
                        n.add_can_reach(dst_point)

    def path(self, start_pos: Coord) -> int:
        start = self.map[start_pos]

        nodes_to_explore = [start]
        while len(nodes_to_explore) > 0:
            #print(f'nodes_to_explore: {len(nodes_to_explore)}')
            new_nodes = set()
            for n in nodes_to_explore:
                n.is_visited = True
                #print(f'n: {n.pos}, min_start_distance: {n.min_start_distance}, can_reach: {len(n.can_reach)}: {list(n.can_reach.keys())}')

                new_min_distance = n.min_start_distance + 1
                for neighbour in n.can_reach.values():
                    if neighbour.min_start_distance > new_min_distance:
                        neighbour.min_start_distance = new_min_distance

                    if not neighbour.is_visited:
                        new_nodes.add(neighbour)

            nodes_to_explore = new_nodes

        end_point = self.map[self.end_pos]
        return end_point.min_start_distance

    def path_from_a(self):
        y_map_size, x_map_size = self.map.sizes()
        shortest_path = -1

        for y in range(y_map_size):
            for x in range(x_map_size):
                coord = Coord(x, y)
                if self.map[coord].height == 0:
                    self.map.clear_visited()
                    self.map[coord].min_start_distance = 0

                    path_len = self.path(coord)
                    if shortest_path < 0 or path_len < shortest_path:
                        shortest_path = path_len

        return shortest_path

    
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    line_generator = load_input(FLAGS.input)
    map = Map(line_generator)
    path = map.path(map.start_pos)
    print(f'part1: path: {path}')

    shortest_path_from_a = map.path_from_a()
    print(f'part2: path: {shortest_path_from_a}')


if __name__ == '__main__':
    main()
