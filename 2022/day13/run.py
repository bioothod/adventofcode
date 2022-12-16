from typing import List, Dict, Tuple
from enum import Enum

import argparse

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

class Cmp(Enum):
    RIGHT = 0
    NOT_RIGHT = 1
    CONTINUE = 2

def in_right_order(left, right) -> Cmp:
    if isinstance(left, int) and isinstance(right, int):
        if left < right:
            return Cmp.RIGHT
        if right < left:
            return Cmp.NOT_RIGHT
        return Cmp.CONTINUE

    if isinstance(left, list) and isinstance(right, list):
        for l, r in zip(left, right):
            cmp = in_right_order(l, r)
            if cmp == Cmp.CONTINUE:
                continue
            return cmp
        if len(left) < len(right):
            return Cmp.RIGHT
        if len(right) < len(left):
            return Cmp.NOT_RIGHT
        return Cmp.CONTINUE

    if isinstance(left, int) and isinstance(right, list):
        left = [left]
    if isinstance(right, int) and isinstance(left, list):
        right = [right]

    return in_right_order(left, right)

class Item:
    def __init__(self, line):
        self.value = eval(line)

    def __lt__(self, other: 'Item') -> bool:
        cmp = in_right_order(self.value, other.value)
        if cmp == Cmp.RIGHT or cmp == cmp.CONTINUE:
            return True
        return False

class Signal:
    def __init__(self, line_generator):
        self.all_packets = []

        self.pair_index = 0
        self.in_right_order_indexes = 0
        
        pair = []
        for line in line_generator:
            if len(line) == 0:
                self.check_pair(pair)
                pair = []
                continue

            item = Item(line)
            pair.append(item)
            self.all_packets.append(item)

        self.del0 = Item('[[2]]')
        self.all_packets.append(self.del0)
        self.del1 = Item('[[6]]')
        self.all_packets.append(self.del1)

        if len(pair) == 2:
            self.check_pair(pair)
        print(f'in_right_order_indexes: {self.in_right_order_indexes}')

    def check_pair(self, pair: List[Item]):
        self.pair_index += 1

        cmp = in_right_order(pair[0].value, pair[1].value)
        if cmp == Cmp.RIGHT:
            self.in_right_order_indexes += self.pair_index

    def sort_packets(self):
        packets = sorted(self.all_packets)
        index_mult = 1
        for idx, p in enumerate(packets):
            if p == self.del0 or p == self.del1:
                index_mult *= idx+1
            #print(p.value)

        print(f'delimiter index multiplcation: {index_mult}')

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    line_generator = load_input(FLAGS.input)
    signal = Signal(line_generator)
    signal.sort_packets()

if __name__ == '__main__':
    main()
