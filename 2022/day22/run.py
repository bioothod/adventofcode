from typing import Callable, List, Dict, Tuple, Optional

from dataclasses import dataclass
from enum import Enum, IntEnum
from copy import deepcopy

import argparse
import itertools

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

    def __repr__(self) -> str:
        #return f'({self.x}.{self.y}), t{self.t.__repr__()}, plane: {self.plane}'
        return f'{self.x}.{self.y}'

    def __add__(self, other: 'Point') -> 'Point':
        self.x += other.x
        self.y += other.y
        return self

    def __sub__(self, other: 'Point') -> 'Point':
        self.x -= other.x
        self.y -= other.y
        return self

    def __floordiv__(self, n: int) -> 'Point':
        self.x = self.x // n
        self.y = self.y // n
        return self

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

    def rotate(self, degree) -> 'Direction':
        if degree == 0:
            res = self
        elif degree == 90:
            res = (self + 1) % 4
        elif degree == 270:
            res = (self - 1) % 4
        elif degree == 180:
            res = (self + 2) % 4
        else:
            raise ValueError(f'invalid rotation degree {degree}')
        return Direction(res)

class Facing:
    def __init__(self, initial: Direction):
        self.dir = initial

    def turn(self, turn: Turn):
        if turn == Turn.CLOCK:
            self.dir = self.dir.rotate(90)
        elif turn == Turn.COUNTER:
            self.dir = self.dir.rotate(270)
        elif turn == Turn.STILL:
            pass
        else:
            raise ValueError(f'unsupported turn "{turn}"')

class FlatPlane:
    def __init__(self, id: int, size: int, start_y: int, start_x: int):
        self.id = id
        self.size = size
        self.start_x = start_x
        self.start_y = start_y
        self.transitions: Dict[Direction, Tuple['FlatPlane', Direction, Callable]] = {}

    def __repr__(self) -> str:
        return f'[{self.id}: {self.start_x}.{self.start_y}-{self.start_x+self.size}.{self.start_y+self.size}]'

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

class Map:
    def __init__(self):
        self.map = []
        self.width = 0
        self.height = 0
        self.cube_size = 0
        self.planes = {}

    def divide_map_to_planes(self, width_div, height_div) -> Tuple[int, bool]:
        width_rem = self.width % width_div
        width = self.width // width_div
        height_rem = self.height % height_div
        height = self.height // height_div
        if width_rem == 0 and height_rem == 0 and width == height:
            return width, True

        return 0, False

    def scan_planes(self):
        plane_size, found = self.divide_map_to_planes(3, 4)
        if not found:
            plane_size, found = self.divide_map_to_planes(4, 3)
            if not found:
                raise ValueError(f'could not determine plane size from the map height: {self.height} and width: {self.width}')

        print(f'map: height: {self.height}, width: {self.width}, plane_size: {plane_size}')
        self.cube_size = plane_size

        plane_idx = 1
        print(f'planes:')
        for height_idx in range(0, self.height // self.cube_size):
            plane_str = '  '
            for width_idx in range(0, self.width // self.cube_size):
                y = height_idx * self.cube_size + 1
                x = width_idx * self.cube_size + 1
                p = self.map[y][x]
                if p.t == PointType.EMPTY:
                    plane_str += '.'
                    continue

                plane = FlatPlane(plane_idx, self.cube_size, y, x)
                self.planes[plane_idx] = plane
                plane_str += str(plane_idx)
                plane_idx += 1

            print(plane_str)

        for plane in self.planes.values():
            self.find_transitions(plane)

    def find_plane(self, point: Point) -> Optional[FlatPlane]:
        for plane in self.planes.values():
            if plane.point_in(point):
                return plane

        return None

    def rotate_around(self, centre: Point, point: Point, R: List[List[int]]) -> Point:
        point = deepcopy(point)
        point -= centre

        x = R[0][0] * point.x + R[0][1] * point.y
        y = R[1][0] * point.x + R[1][1] * point.y

        point.x = x
        point.y = y

        point += centre
        return point

    def find_transitions_around(self, plane: FlatPlane, centres: List[Point], point: Point, direction: Direction):
        rotation_matrixes = {
            90: [
                [0, -1],
                [1, 0],
            ],
            270: [
                [0, 1],
                [-1, 0],
            ]
        }

        for centre in centres:
            for rot_degree in [90, 270]:
                rot_matrix = rotation_matrixes[rot_degree]
                new_point = self.rotate_around(centre, point, rot_matrix)
                new_direction = direction.rotate(rot_degree)

                other_plane = self.find_plane(new_point)
                if other_plane is not None and other_plane.id != plane.id:
                    plane.transitions[direction] = (other_plane, new_direction, lambda pos: self.rotate_around(centre, pos, rot_matrix))
                    print(f'transition: {plane.id} {direction} -> {other_plane.id} {new_direction}')


    def find_transitions(self, plane: FlatPlane):

        directions = {
            Direction.UP: [
                [plane.start_x, plane.start_y],
                [plane.start_x+plane.size-1, plane.start_y],
            ],
            Direction.LEFT: [
                [plane.start_x, plane.start_y],
                [plane.start_x, plane.start_y+plane.size-1],
            ],
            Direction.RIGHT: [
                [plane.start_x+plane.size-1, plane.start_y],
                [plane.start_x+plane.size-1, plane.start_y+plane.size-1],
            ],
            Direction.DOWN: [
                [plane.start_x, plane.start_y+plane.size-1],
                [plane.start_x+plane.size-1, plane.start_y+plane.size-1],
            ],
        }

        for direction, edge in directions.items():
            if direction in plane.transitions:
                continue

            p0, p1 = edge

            p0x, p0y = p0
            p0 = deepcopy(self.map[p0y-1][p0x-1])
            p1x, p1y = p1
            p1 = deepcopy(self.map[p1y-1][p1x-1])


            mid = deepcopy(p0)
            mid += p1
            mid //= 2

            mid = plane.step(mid, direction)
            other_plane = self.find_plane(mid)
            if other_plane is not None:
                plane.transitions[direction] = (other_plane, direction, lambda pos: pos)
                other_plane.transitions[direction.rotate(180)] = (plane, direction, lambda pos: pos)
                #print(f'transition: {plane.id} {direction} -> {other_plane.id}')
                continue

            mid = self.wrap_coords(mid)

            possible_rotations  = [
                [p0, p1, mid, 90],
                [p0, p1, mid, 270],
                [p1, p0, mid, 90],
                [p1, p0, mid, 270],
            ]

            for centre, second_centre, point, rot_degree in possible_rotations:

                second_direction =
                rot_degree = 360 - rot_degree
                rot_matrix = rotation_matrixes[rot_degree]
                new_point = self.rotate_around(second_centre, new_point, rot_matrix)
                new_direction = direction.rotate(rot_degree)

                other_plane = self.find_plane(new_point)
                print(f'plane: {plane.id}, centre: {centre}, point: {point} {direction} -> {new_point} {new_direction}, new_plane: {other_plane}')
                if other_plane is not None and other_plane.id != plane.id:
                    plane.transitions[direction] = (other_plane, new_direction, lambda pos: self.rotate_around(centre, pos, rot_matrix))
                    print(f'transition: {plane.id} {direction} -> {other_plane.id} {new_direction}')





    def add_point(self, y, x, s):
        point = Point(y, x, PointType.parse(s), 0)
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

    def wrap_coords(self, pos: Point) -> Point:
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
        current_plane = self.find_plane(pos)
        if current_plane is None:
            raise ValueError(f'could not locate plane for the point {pos}')

        next_pos = current_plane.step(pos, direction)
        if current_plane.point_in(next_pos):
            return next_pos, direction

        next_plane, next_direction, rot_callable = current_plane.transitions[direction]
        next_pos = rot_callable(pos)

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
    def __init__(self, line_generator):
        self.map = Map()

        for y, line in enumerate(line_generator):
            if len(line) == 0:
                break

            for x, s in enumerate(line):
                self.map.add_point(y+1, x+1, s)

        self.map.wrap()
        self.map.scan_planes()

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
    FLAGS = parser.parse_args()

    line_generator = load_input(FLAGS.input)
    solution = Solution(line_generator)

    part1 = solution.part1()
    print(f'part1: {part1}')

    part2 = solution.part2()
    print(f'part2: {part2}')

if __name__ == '__main__':
    main()
