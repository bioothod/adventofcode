import argparse

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

class Range:
    def __init__(self, descr):
        range_str = descr.split('-')
        range_int = [int(x) for x in range_str]

        self.start = range_int[0]
        self.end = range_int[1]

    def fully_contains(self, other: 'Range'):
        if self.start <= other.start and self.end >= other.end:
            return True
        return False

    def overlap(self, other: 'Range'):
        if self.end < other.start:
            return False
        if self.start > other.end:
            return False

        return True

def parse_ranges(line):
    ranges = []
    for per_elf_str in line.split(','):
        r = Range(per_elf_str)
        ranges.append(r)

    return ranges

def ranges_fully_overlap(ranges):
    if ranges[0].fully_contains(ranges[1]):
        return True
    if ranges[1].fully_contains(ranges[0]):
        return True
    return False

def ranges_partially_overlap(ranges):
    if ranges[0].overlap(ranges[1]):
        return True
    if ranges[1].overlap(ranges[0]):
        return True
    return False

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    fully_overlap = 0
    partially_overlap = 0
    for line in load_input(FLAGS.input):
        ranges = parse_ranges(line)

        if ranges_fully_overlap(ranges):
            fully_overlap += 1

        if ranges_partially_overlap(ranges):
            partially_overlap += 1

    print(f'fully_overlap: {fully_overlap}, partially_overlap: {partially_overlap}')

if __name__ == '__main__':
    main()
