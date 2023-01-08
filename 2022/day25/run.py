from typing import List, Dict, Tuple, Optional

from dataclasses import dataclass

import argparse
import itertools

DIGIT2SNAFU = {
    0: '0',
    1: '1',
    2: '2',
    -1: '-',
    -2: '='
}
SYMBOL2DIGIT = {
    '1': 1,
    '2': 2,
    '0': 0,
    '-': -1,
    '=': -2,
}



def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

class Solution:
    def __init__(self, line_generator):
        self.lines = list(line_generator)

    def snafu2decimal(self, line: str) -> int:
        value = 0
        for idx, s in enumerate(reversed(line)):
            m = 5 ** idx
            value += m * SYMBOL2DIGIT[s]

        return value

    def decimal2snafu(self, decimal: int) -> str:
        snafu = []
        for idx in itertools.count(0):
            rem = decimal % 5
            order = 0
            if rem > 2:
                rem -= 5
                order = 1

            decimal = decimal // 5 + order

            snafu.append(DIGIT2SNAFU[rem])

            if decimal == 0:
                break

        snafu = reversed(snafu)
        return ''.join(snafu)

    def part1(self):
        decimal_sum = sum([self.snafu2decimal(line) for line in self.lines])
        snafu = self.decimal2snafu(decimal_sum)
        return snafu

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    line_generator = load_input(FLAGS.input)
    solution = Solution(line_generator)

    part1 = solution.part1()
<<<<<<< Updated upstream
    print(f'parth1: {part1}')
=======
    print(f'part1: {part1}')
>>>>>>> Stashed changes

if __name__ == '__main__':
    main()
