from typing import List

import argparse

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

class Forest:
    def __init__(self, line_generator):
        self.rows = []
        for line in line_generator:
            row = [int(l) for l in line]
            self.rows.append(row)

    def distance_equal(self, tree: int, array: List[int]) -> int:
        dist = 0
        for t in array:
            dist += 1
            if tree <= t:
                break
        return dist

    def distance_unobstructed(self, tree: int, array: List[int]) -> int:
        dist = 0
        for t in array:
            if tree <= t:
                break
            dist += 1
        return dist

    def is_visible(self, col: int, row: int) -> bool:
        if col == 0 or row == 0:
            return True
        if col + 1 == len(self.rows[0]):
            return True
        if row + 1 == len(self.rows):
            return True

        tree = self.rows[row][col]

        array = self.rows[row][:col][::-1]
        if self.distance_unobstructed(tree, array) == len(array):
            return True

        array = self.rows[row][col+1:]
        if self.distance_unobstructed(tree, array) == len(array):
            return True


        column = [self.rows[i][col] for i in range(len(self.rows))]

        array = column[:row][::-1]
        if self.distance_unobstructed(tree, array) == len(array):
            return True

        array = column[row+1:]
        if self.distance_unobstructed(tree, array) == len(array):
            return True

        return False

    def count_visible(self):
        visible = 0
        for row in range(len(self.rows)):
            for col in range(len(self.rows[0])):
                if self.is_visible(col, row):
                    visible += 1
        return visible

    def scenic_score(self, col: int, row: int) -> int:
        if col == 0 or row == 0:
            return 0
        if col == len(self.rows[0]) - 1:
            return 0
        if row == len(self.rows) - 1:
            return 0

        tree = self.rows[row][col]
        score = 1

        score *= self.distance_equal(tree, self.rows[row][:col][::-1])
        score *= self.distance_equal(tree, self.rows[row][col+1:])

        column = [self.rows[i][col] for i in range(len(self.rows))]
        score *= self.distance_equal(tree, column[:row][::-1])
        score *= self.distance_equal(tree, column[row+1:])

        return score

    def max_scenic_score(self):
        max_score = 0
        for row in range(len(self.rows)):
            for col in range(len(self.rows[0])):
                score = self.scenic_score(col, row)
                if score > max_score:
                    max_score = score

        return max_score

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    forest = Forest(load_input(FLAGS.input))
    visible = forest.count_visible()

    print(f'part1: total_visible: {visible}')

    max_scenic_score = forest.max_scenic_score()
    print(f'part2: max_scenic_score: {max_scenic_score}')

if __name__ == '__main__':
    main()
