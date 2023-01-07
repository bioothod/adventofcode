from typing import List, Dict, Tuple, Optional

from dataclasses import dataclass
from enum import Enum, IntEnum
from copy import deepcopy

import argparse

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            if line[-1] == '\n':
                line = line[:-1]
            yield line

class PointType(Enum):
    EMPTY = 0,
    PATH = 1,
    WALL = 2,

    def __repr__(self) -> str:
        if self == PointType.EMPTY:
            return ' '
        if self == PointType.WALL:
            return '#'
        if self == PointType.PATH:
            return '.'

        raise ValueError('unsupported point type "{self}"')

    @staticmethod
    def parse(s) -> 'PointType':
        if s == '.':
            return PointType.PATH
        elif s == '#':
            return PointType.WALL
        elif s == ' ':
            return PointType.EMPTY
        else:
            raise ValueError(f'unsupported map symbol "{s}"')

@dataclass
class Point:
    y: int
    x: int
    t: PointType
    plane: int


class Turn(Enum):
    CLOCK = 0,
    COUNTER = 1,
    STILL = 2,

    @staticmethod
    def parse(symbol) -> 'Turn':
        if symbol == 'R':
            return Turn.CLOCK
        elif symbol == 'L':
            return Turn.COUNTER
        else:
            raise ValueError(f'unsupported turn symbol "{symbol}"')

@dataclass
class Move:
    num_steps: int
    turn: Turn

class Direction(IntEnum):
    RIGHT = 0,
    DOWN = 1,
    LEFT = 2,
    UP = 3,

    def __repr__(self) -> str:
        return '>v<^'[int(self)]

    def __str__(self) -> str:
        return self.__repr__()

class Facing:
    def __init__(self, initial: Direction):
        self.dir = initial

    def turn(self, turn: Turn):
        if turn == Turn.CLOCK:
            self.dir = Direction((self.dir + 1) % 4)
        elif turn == Turn.COUNTER:
            self.dir = Direction((self.dir - 1) % 4)
        elif turn == Turn.STILL:
            pass
        else:
            raise ValueError(f'unsupported turn "{turn}"')

class FlatPlane:
    def __init__(self, size: int, start_y: int, start_x: int):
        self.size = size
        self.start_x = start_x
        self.start_y = start_y

    def __repr__(self) -> str:
        return f'[flat_y: [{self.start_y}, {self.start_y+self.size}], flat_x: [{self.start_x}, {self.start_x+self.size}]]'

    def point_in(self, pos: Point) -> bool:
        if pos.x < self.start_x or pos.y < self.start_y:
            return False
        if pos.x >= self.start_x + self.size or pos.y >= self.start_y + self.size:
            return False

        return True

    def step(self, pos: Point, direction: Direction) -> Point:
        if not self.point_in(pos):
            raise ValueError(f'initial position {pos} does not belong to this plane {self}')

        pos = deepcopy(pos)

        if direction == Direction.UP:
            pos.y -= 1
        elif direction == Direction.DOWN:
            pos.y += 1
        elif direction == Direction.RIGHT:
            pos.x += 1
        elif direction == Direction.LEFT:
            pos.x -= 1
        else:
            raise ValueError(f'invalid direction "{direction}"')

        return pos

class CubeFold1:
    def __init__(self, cube_size: int):
        self.cube_size = cube_size
        self.planes = {
            1: FlatPlane(size=self.cube_size, start_y=1, start_x=9),
            2: FlatPlane(size=self.cube_size, start_y=5, start_x=1),
            3: FlatPlane(size=self.cube_size, start_y=5, start_x=5),
            4: FlatPlane(size=self.cube_size, start_y=5, start_x=9),
            5: FlatPlane(size=self.cube_size, start_y=9, start_x=9),
            6: FlatPlane(size=self.cube_size, start_y=9, start_x=13),
        }
        self.transitions = {
            1: {
                Direction.UP:    (2, 'MX-MY', Direction.DOWN),
                Direction.LEFT:  (3, 'MX-C', Direction.DOWN),
                Direction.DOWN:  (4, '', Direction.DOWN),
                Direction.RIGHT: (6, 'MX-MY', Direction.LEFT),
            },
            2: {
                Direction.UP:    (1, 'MX-MY', Direction.DOWN),
                Direction.LEFT:  (6, 'MY-C', Direction.UP),
                Direction.DOWN:  (5, 'MX-MY', Direction.UP),
                Direction.RIGHT: (3, '', Direction.RIGHT),
            },
            3: {
                Direction.UP:    (1, 'MY-C', Direction.RIGHT),
                Direction.LEFT:  (2, '', Direction.LEFT),
                Direction.DOWN:  (5, 'MX-C', Direction.RIGHT),
                Direction.RIGHT: (4, '', Direction.RIGHT),
            },
            4: {
                Direction.UP:    (1, '', Direction.UP),
                Direction.LEFT:  (3, '', Direction.LEFT),
                Direction.DOWN:  (5, '', Direction.DOWN),
                Direction.RIGHT: (6, 'MY-C', Direction.DOWN),
            },
            5: {
                Direction.UP:    (4, '', Direction.UP),
                Direction.LEFT:  (3, 'MY-C', Direction.UP),
                Direction.DOWN:  (2, 'MY-MX', Direction.UP),
                Direction.RIGHT: (6, '', Direction.RIGHT),
            },
            6: {
                Direction.UP:    (4, 'MX-C', Direction.LEFT),
                Direction.LEFT:  (5, '', Direction.LEFT),
                Direction.DOWN:  (2, 'MX-C', Direction.RIGHT),
                Direction.RIGHT: (1, 'MY-MX', Direction.LEFT),
            },
        }

    def relative_transform(self, p: Point, transform: str) -> Point:
        x = p.x
        y = p.y
        for op in transform.split('-'):
            if op == 'MX':
                x = self.cube_size - x + 1
            elif op == 'MY':
                y = self.cube_size - y + 1
            elif op == 'C':
                tmp = x
                x = y
                y = tmp

        return Point(y=y, x=x, t=PointType.WALL, plane=0)


    def flat_point_to_plane(self, point: Point):
        for plane_id, plane in self.planes.items():
            if plane.point_in(point):
                point.plane = plane_id
                return

class CubeFold2:
    def __init__(self, cube_size: int):
        self.cube_size = cube_size
        self.planes = {
            1: FlatPlane(size=self.cube_size, start_y=1, start_x=51),
            2: FlatPlane(size=self.cube_size, start_y=1, start_x=101),
            3: FlatPlane(size=self.cube_size, start_y=51, start_x=51),
            4: FlatPlane(size=self.cube_size, start_y=101, start_x=1),
            5: FlatPlane(size=self.cube_size, start_y=101, start_x=51),
            6: FlatPlane(size=self.cube_size, start_y=151, start_x=1),
        }
        self.transitions = {
            1: {
                Direction.UP:    (6, 'MY-C', Direction.RIGHT),
                Direction.LEFT:  (4, 'MX-MY', Direction.RIGHT),
                Direction.DOWN:  (3, '', Direction.DOWN),
                Direction.RIGHT: (2, '', Direction.RIGHT),
            },
            2: {
                Direction.UP:    (6, '', Direction.UP),
                Direction.LEFT:  (1, '', Direction.LEFT),
                Direction.DOWN:  (3, 'MY-C', Direction.LEFT),
                Direction.RIGHT: (5, 'MY-MX', Direction.LEFT),
            },
            3: {
                Direction.UP:    (1, '', Direction.UP),
                Direction.LEFT:  (4, 'MX-C', Direction.DOWN),
                Direction.DOWN:  (5, '', Direction.DOWN),
                Direction.RIGHT: (2, 'MX-C', Direction.UP),
            },
            4: {
                Direction.UP:    (3, 'MY-C', Direction.RIGHT),
                Direction.LEFT:  (1, 'MX-MY', Direction.RIGHT),
                Direction.DOWN:  (6, '', Direction.DOWN),
                Direction.RIGHT: (5, '', Direction.RIGHT),
            },
            5: {
                Direction.UP:    (3, '', Direction.UP),
                Direction.LEFT:  (4, '', Direction.LEFT),
                Direction.DOWN:  (6, 'MY-C', Direction.LEFT),
                Direction.RIGHT: (2, 'MX-MY', Direction.LEFT),
            },
            6: {
                Direction.UP:    (4, '', Direction.UP),
                Direction.LEFT:  (1, 'MX-C', Direction.DOWN),
                Direction.DOWN:  (2, '', Direction.DOWN),
                Direction.RIGHT: (5, 'MX-C', Direction.UP),
            },
        }

    def relative_transform(self, p: Point, transform: str) -> Point:
        x = p.x
        y = p.y
        for op in transform.split('-'):
            if op == 'MX':
                x = self.cube_size - x + 1
            elif op == 'MY':
                y = self.cube_size - y + 1
            elif op == 'C':
                tmp = x
                x = y
                y = tmp

        return Point(y=y, x=x, t=PointType.WALL, plane=0)


    def flat_point_to_plane(self, point: Point):
        for plane_id, plane in self.planes.items():
            if plane.point_in(point):
                point.plane = plane_id
                return

class Map:
    def __init__(self, is_small=True):
        self.map = []
        self.width = 0
        self.height = 0

        if is_small:
            self.cube_size = 4
            self.fold = CubeFold1(self.cube_size)
        else:
            self.cube_size = 50
            self.fold = CubeFold2(self.cube_size)


    def add_point(self, y, x, s):
        point = Point(y, x, PointType.parse(s), 0)
        self.fold.flat_point_to_plane(point)
        if point.x > self.width:
            self.width = point.x
        if point.y > self.height:
            self.height = point.y

        if len(self.map) == 0:
            self.map.append([point])
            return

        last_row = self.map[-1]
        last_y = last_row[0].y
        if last_y == point.y:
            last_row.append(point)
        else:
            self.map.append([point])

    def wrap(self):
        for y, line in enumerate(self.map):
            for x in range(len(line), self.width):
                line.append(Point(y+1, x, PointType.EMPTY, 0))

    def visualize(self, steps: List[Tuple[Point, Direction]], facing: Direction):
        map = []
        for y, line in enumerate(self.map):
            line_str = [p.t.__repr__() for p in line]
            map.append(line_str)

        for point, direction in steps:
            map[point.y-1][point.x-1] = direction.__repr__()

        map_str = []
        for line in map:
            line_str = ''.join(line)
            map_str.append(line_str)
        map_str = '\n'.join(map_str)
        print(map_str)

    def flat_step(self, pos: Point, direction: Direction, num_steps: int) -> Point:
        for _ in range(num_steps):
            y = pos.y
            x = pos.x

            while True:
                if direction == Direction.UP:
                    y -= 1
                elif direction == Direction.DOWN:
                    y += 1
                elif direction == Direction.RIGHT:
                    x += 1
                elif direction == Direction.LEFT:
                    x -= 1
                else:
                    raise ValueError(f'invalid direction "{direction}"')

                if y == self.height+1:
                    y = 1
                if y == 0:
                    y = self.height

                if x == self.width+1:
                    x = 1
                if x == 0:
                    x = self.width

                point = self.map[y-1][x-1]
                if point.t == PointType.PATH:
                    pos.y = y
                    pos.x = x
                    break

                if point.t == PointType.WALL:
                    return pos

                # continue wrapping around over empty space

        return pos

    def wrap_coords(self, pos):
        pos = deepcopy(pos)

        if pos.y == self.cube_size + 1:
            pos.y = 1
        if pos.y == 0:
            pos.y = self.cube_size

        if pos.x == self.cube_size + 1:
            pos.x = 1
        if pos.x == 0:
            pos.x = self.cube_size

        return pos

    def single_cube_step(self, pos: Point, direction: Direction) -> Tuple[Point, Direction]:
        current_plane = self.fold.planes[pos.plane]
        next_pos = current_plane.step(pos, direction)
        if current_plane.point_in(next_pos):
            return next_pos, direction

        next_plane_id, relative_coord_transform, next_direction = self.fold.transitions[pos.plane][direction]

        relative_next_pos = Point(y=next_pos.y-current_plane.start_y+1,
                                  x=next_pos.x-current_plane.start_x+1,
                                  t=next_pos.t,
                                  plane=next_pos.plane)
        relative_next_pos = self.wrap_coords(relative_next_pos)
        #print(f'transition1: {pos} {next_direction} -> {next_pos} {next_direction}, current_plane: {current_plane}, relative_next_pos: {relative_next_pos}')

        relative_next_pos = self.fold.relative_transform(relative_next_pos, relative_coord_transform)

        next_plane = self.fold.planes[next_plane_id]
        next_y = relative_next_pos.y + next_plane.start_y - 1
        next_x = relative_next_pos.x + next_plane.start_x - 1
        #print(f'transition2: {pos} {direction} -> plane: {next_plane_id} {next_plane}, y: {next_y}, x: {next_x}, relative_next_pos: {relative_next_pos}')
        next_point_type = self.map[next_y-1][next_x-1].t
        next_pos = Point(next_y, next_x, next_point_type, next_plane_id)

        #print(f'transition3: {pos} {direction} -> {next_pos} {next_direction}')
        return next_pos, next_direction

    def cube_step(self, pos: Point, direction: Direction, num_steps: int) -> List[Tuple[Point, Direction]]:
        steps = []
        for _ in range(num_steps):
            while True:
                next_pos, next_direction = self.single_cube_step(pos, direction)

                point = self.map[next_pos.y-1][next_pos.x-1]
                #print(f'cube: {pos} {next_direction} -> {next_pos} {next_direction}, map: {point}')
                if point.t == PointType.PATH:
                    pos = next_pos
                    direction = next_direction
                    steps.append((pos, direction))
                    break

                if point.t == PointType.WALL:
                    #print(f'cube: ret: {pos}')
                    steps.append((pos, direction))
                    return steps

                # continue wrapping around over empty space

        #print(f'cube: ret: {pos}')
        steps.append((pos, direction))
        return steps

class Solution:
    def __init__(self, line_generator, is_small=False):
        self.map = Map(is_small)

        for y, line in enumerate(line_generator):
            if len(line) == 0:
                break

            for x, s in enumerate(line):
                self.map.add_point(y+1, x+1, s)

        self.map.wrap()

        self.route = self.parse_route(next(line_generator))
        self.steps_made = []

        for pos in self.map.map[0]:
            if pos.t == PointType.PATH:
                self.orig_pos = pos
                break

        self.orig_facing = Facing(Direction.RIGHT)
        self.reset()
        print(f'map: w: {self.map.width}/{len(self.map.map[0])}, h: {self.map.height}/{len(self.map.map)}, route: {len(self.route)} turns, pos: {self.pos}')

    def reset(self):
        self.pos = deepcopy(self.orig_pos)
        self.facing = deepcopy(self.orig_facing)

    def parse_route(self, line) -> List[Move]:
        steps = []

        start_index = 0
        for end_index, s in enumerate(line):
            if not s.isdigit():
                num_steps = int(line[start_index:end_index])
                turn = Turn.parse(s)

                steps.append(Move(num_steps, turn))
                start_index = end_index + 1

        num_steps = int(line[start_index:])
        steps.append(Move(num_steps, Turn.STILL))
        return steps

    def part1(self):
        self.reset()

        for step in self.route:
            self.pos = self.map.flat_step(self.pos, self.facing.dir, step.num_steps)
            self.facing.turn(step.turn)

        return self.pos.y * 1000 + self.pos.x * 4 + int(self.facing.dir)

    def part2(self):
        self.reset()

        for step in self.route:
            #print(f'pos: {self.pos}, dir: {self.facing.dir}, turn: {step.turn}, num_steps: {step.num_steps}')
            steps = self.map.cube_step(self.pos, self.facing.dir, step.num_steps)
            self.pos, self.facing.dir = steps[-1]
            self.facing.turn(step.turn)

            self.steps_made += steps
            #self.map.visualize(self.steps_made, self.facing.dir)

        return self.pos.y * 1000 + self.pos.x * 4 + int(self.facing.dir)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    parser.add_argument('--is_small', action='store_true', help='Whether this is a small example with a particular folding')
    FLAGS = parser.parse_args()

    line_generator = load_input(FLAGS.input)
    solution = Solution(line_generator, FLAGS.is_small)

    part1 = solution.part1()
    print(f'part1: {part1}')

    part2 = solution.part2()
    print(f'part2: {part2}')

if __name__ == '__main__':
    main()
