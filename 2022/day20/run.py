from typing import List, Dict, Tuple

import argparse

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

class Solution:
    def __init__(self, line_generator):
        self.orig_nums = list(map(int, line_generator))
        self.num_index = list(range(len(self.orig_nums)))
        self.reset()

    def reset(self, mult=1):
        self.index2num = {idx:num*mult for idx, num in zip(self.num_index, self.orig_nums)}
        self.index2pos = {idx:pos for pos, idx in enumerate(self.num_index)}

    def mix_one(self):
        for num_idx in self.num_index:
            num = self.index2num[num_idx]
            pos = self.index2pos[num_idx]

            index_list = self.build_index_seq()

            if num > 0:
                step = +1
            elif num < 0:
                step = -1
            else:
                step = 0

            if step != 0:
                #print(f'num: {num}, pos: {pos}->{new_pos}, step: {step}')
                #print(f'  orig_nums_list: {self.build_num_seq()}')

                num_steps = abs(num) % (len(self.orig_nums) - 1)
                for _ in range(num_steps):
                    orig_pos = pos
                    pos = (pos + step) % len(self.orig_nums)

                    item_index = index_list[pos]
                    item = self.index2num[item_index]

                    self.index2pos[item_index] = orig_pos
                    self.index2pos[num_idx] = pos

                    index_list[pos] = num_idx
                    index_list[orig_pos] = item_index


                    #print(f'  i: {i}, item_index: {item_index}, item: {item}, item_pos: {item_pos} -> {new_item_pos}')
                    #print(f'  nums_list: {self.build_num_seq()}')


                #self.index2pos[num_idx] = new_pos
                #print(f'  nums_list: {self.build_num_seq()}')

    def mix(self, n):
        for _ in range(n):
            self.mix_one()

        nums_list = self.build_num_seq()
        #print(nums_list)

        zero_index = nums_list.index(0)
        coords = [1000, 2000, 3000]
        res_sum = 0
        for x in coords:
            idx = (x + zero_index) % len(nums_list)
            res_sum += nums_list[idx]

        return res_sum

    def build_index_seq(self):
        index_seq = [0] * len(self.num_index)
        for num_idx in self.num_index:
            pos = self.index2pos[num_idx]
            index_seq[pos] = num_idx

        return index_seq

    def build_num_seq(self):
        nums = [0] * len(self.num_index)
        for num_idx in self.num_index:
            pos = self.index2pos[num_idx]
            num = self.index2num[num_idx]
            nums[pos] = num

        return nums

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    line_generator = load_input(FLAGS.input)
    solution = Solution(line_generator)

    part1_res = solution.mix(n=1)
    print(f'part1: {part1_res}')

    solution.reset(mult=811589153)
    part2_res = solution.mix(n=10)
    print(f'part2: {part2_res}')

if __name__ == '__main__':
    main()
