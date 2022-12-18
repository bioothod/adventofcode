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
class Coord:
    x: int
    y: int

    def __repr__(self) -> str:
        return f'{self.x}.{self.y}'

    def mh_dist(self, p: 'Coord') -> int:
        return abs(self.x - p.x) + abs(self.y - p.y)

class Sensor:
    def __init__(self, line):
        number_pattern = '(-?\\d+)'
        m = re.match(f'Sensor at x={number_pattern}, y={number_pattern}: closest beacon is at x={number_pattern}, y={number_pattern}', line)
        if m is None:
            raise ValueError(f'invalid line: {line}')

        sx = int(m.group(1))
        sy = int(m.group(2))
        self.pos = Coord(sx, sy)

        bx = int(m.group(3))
        by = int(m.group(4))
        self.beacon = Coord(bx, by)

        self.mh_dist = self.pos.mh_dist(self.beacon)

        #print(f'sensor: {self.pos}, beacon: {self.beacon}, mh_dist: {self.mh_dist}')

def split_range(c: Coord, t: Tuple[int, int]) -> List[Tuple[int, int]]:
    x0, x1 = t

    if c.x == x0:
        x0 += 1
    if c.x == x1:
        x1 -= 1

    if c.x >= x0 and c.x <= x1:
        r0 = (x0, c.x-1)
        r1 = (c.x+1, x1)
        return [r0, r1]

    if x1 >= x0:
        t = (x0, x1)
        return [t]

    return []

def split_all_ranges(c: Coord, ranges: List[Tuple[int, int]]) -> List[Tuple[int, int]]:
    rev_ranges = []
    for r in ranges:
        rev_ranges += split_range(c, r)
    return rev_ranges

def merge_ranges(orig_range: Tuple[int, int], ranges: List[Tuple[int, int]]) -> List[Tuple[int, int]]:
    new_ranges = []
    rmin, rmax = orig_range

    for r in ranges:
        ni_rmin, ni_rmax = r

        if rmax < ni_rmin or rmin > ni_rmax:
            new_ranges.append(r)
            continue

        rmin = min(rmin, ni_rmin)
        rmax = max(rmax, ni_rmax)

    new_ranges.append((rmin, rmax))

    return new_ranges

class Map:
    def __init__(self, line_generator):
        self.sensors = [Sensor(line) for line in line_generator]

    def build_sensor_map(self, line, xlimit=None):
        ranges_in_line = []
        for s in self.sensors:
            yrange = (s.pos.y-s.mh_dist, s.pos.y+s.mh_dist+1)
            if line < yrange[0] or line >= yrange[1]:
                continue

            y_sens_dist = abs(s.pos.y - line)
            xdist = s.mh_dist - y_sens_dist

            # range in inclusive
            xmin = s.pos.x - xdist
            xmax = s.pos.x + xdist

            if xlimit is not None:
                xmin = max(xmin, xlimit[0])
                xmax = min(xmax, xlimit[1])

            xrange = (xmin, xmax)

            ranges_in_line.append(xrange)

        return ranges_in_line

    def exclude_sensors(self, line, ranges_in_line):
        for s in self.sensors:
            if s.pos.y == line:
                ranges_in_line = split_all_ranges(s.pos, ranges_in_line)
            if s.beacon.y == line:
                ranges_in_line = split_all_ranges(s.beacon, ranges_in_line)

        return ranges_in_line

    def merge_ranges(self, ranges_in_line):
        new_ranges = []
        for r in ranges_in_line:
            new_ranges = merge_ranges(r, new_ranges)
        return new_ranges

    def occupied(self, ranges_in_line):
        occupied_in_pos = 0
        for r in ranges_in_line:
            occupied_in_pos += r[1] - r[0] + 1

        return occupied_in_pos

    def part1(self, line):
        ranges_in_line = self.build_sensor_map(line=line)
        ranges_in_line = self.exclude_sensors(line, ranges_in_line)
        ranges_in_line = self.merge_ranges(ranges_in_line)
        return self.occupied(ranges_in_line)

    def part2(self, xlimit, ylimit):
        for y in range(ylimit[0], ylimit[1]):
            ranges_in_line = self.build_sensor_map(line=y, xlimit=xlimit)
            ranges_in_line = self.merge_ranges(ranges_in_line)
            occupied = self.occupied(ranges_in_line)

            if occupied != ylimit[1] - ylimit[0] + 1:
                sorted_ranges = sorted(ranges_in_line, key=lambda x: x[0])
                x = sorted_ranges[0][1] + 1
                return x * 4000000 + y

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    line_generator = load_input(FLAGS.input)
    map = Map(line_generator)

    line = 20
    line = 2000000
    print(f'part1: line: {line}, occupied: {map.part1(line)}')

    xlimit = (0, 20)
    ylimit = (0, 20)

    xlimit = (0, 4000000)
    ylimit = (0, 4000000)
    result = map.part2(xlimit, ylimit)
    print(f'part2: result: {result}')

if __name__ == '__main__':
    main()
