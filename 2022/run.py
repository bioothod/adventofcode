from typing import List, Dict, Tuple, Optional

from dataclasses import dataclass

import argparse
import re

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

class Solution:
    def __init__(self, line_generator):
        for line in line_generator:
            pass

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    line_generator = load_input(FLAGS.input)
    solution = Solution(line_generator)

if __name__ == '__main__':
    main()
