from typing import List

import argparse
import itertools
import math
import re

from copy import deepcopy

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

def parse_arg_str(arg_str, item):
    if arg_str.isnumeric():
        return int(arg_str)

    if arg_str == 'old':
        return item

class Item:
    def __init__(self, worry_level, src_id):
        self.orig_value = worry_level
        self.src_id = src_id

        self.worry_level = 0

        self.set_new_level(worry_level)

    def __repr__(self):
        return f'{self.orig_value}.{self.src_id}/{self.worry_level}'

    def value(self):
        return self.worry_level

    def set_new_level(self, new_worry_level):
        self.worry_level = new_worry_level

class Monkey:
    def __init__(self):
        self.id = 0
        self.had_items = 0
        self.items: List[Item] = []

    def feed_line(self, line):
        if line.startswith('Monkey '):
            self.id = int(line[:-1].split()[-1])
            return

        if line.startswith('Starting items'):
            items = line.split(':')[1]
            items = [int(i) for i in items.split(',')]
            for item in items:
                i = Item(item, self.id)
                self.items.append(i)
            return

        if line.startswith('Operation'):
            whole_op_str = line.split(':')[1]
            m = re.search('new = (\\w+) (.) ([\\w\\d]+)', whole_op_str)
            if not m:
                raise ValueError(f'monkey{self.id}: unsupported operation line: {line}')

            self.arg0_str = m.group(1)
            op_str = m.group(2)
            self.arg1_str = m.group(3)

            if op_str == '+':
                self.op = lambda x, y: x + y
            elif op_str == '-':
                self.op = lambda x, y: x - y
            elif op_str == '*':
                self.op = lambda x, y: x * y
            elif op_str == '/':
                self.op = lambda x, y: x / y
            else:
                raise ValueError(f'{self.id}: unsupported op {op_str} in operation: {whole_op_str}')

            self.x_is_old = self.arg0_str == 'old'
            if not self.x_is_old:
                self.x = int(self.arg0_str)

            self.y_is_old = self.arg1_str == 'old'
            if not self.y_is_old:
                self.y = int(self.arg1_str)
            return

        if line.startswith('Test: '):
            op_str = line.split(':')[1]
            div = op_str.split('by ')[1]
            self.test_divisible_by = int(div)
            return

        if line.startswith('If true:'):
            other_id = line.split()[-1]
            self.div_test_true_dst_id = int(other_id)
            return

        if line.startswith('If false:'):
            other_id = line.split()[-1]
            self.div_test_false_dst_id = int(other_id)
            return

    def add_item(self, item: Item):
        self.items.append(item)

    def turn(self, change_worry_level_in_the_middle, lcm=None):
        ret = []
        for item in self.items:
            self.had_items += 1

            #x = parse_arg_str(self.arg0_str, item)
            #y = parse_arg_str(self.arg1_str, item)
            if self.x_is_old:
                x = item.value()
            else:
                x = self.x

            if self.y_is_old:
                y = item.value()
            else:
                y = self.y

            new_item = self.op(x, y)

            if change_worry_level_in_the_middle:
                new_item = int(math.floor(new_item / 3))
            else:
                new_item %= lcm

            if new_item % self.test_divisible_by == 0:
                dst_id = self.div_test_true_dst_id
            else:
                dst_id = self.div_test_false_dst_id

            item.set_new_level(new_item)
            ret.append((item, dst_id))

        self.items = []
        return ret

def crt(value_pairs):
    value_pairs = sorted(value_pairs, key=lambda x: x[1], reverse=True)

    div_mult = 1
    start = value_pairs[0][0]
    for idx in range(0, len(value_pairs)-1):
        rem, div = value_pairs[idx]
        next_rem, next_div = value_pairs[idx+1]
        step = div_mult * div
        for number in itertools.count(start=start, step=step):
            if number % next_div == next_rem:
                div_mult *= div
                start = number
                break

    return number


def run_monkey_business(monkeys: List[Monkey], num_rounds: int, change_worry_level_in_the_middle: bool):
    all_items = []
    all_div = []
    for m in monkeys:
        all_div.append(m.test_divisible_by)
        for item in m.items:
            all_items.append(item)

    lcm = math.lcm(*all_div)
    for round in range(num_rounds):
        for monkey in monkeys:
            dst_items = monkey.turn(change_worry_level_in_the_middle, lcm=lcm)
            for item, dst_id in dst_items:
                monkeys[dst_id].add_item(item)

        if False and not change_worry_level_in_the_middle:
            for item in all_items:
                value_pairs = []
                for m in monkeys:
                    orig_rem = item.value() % m.test_divisible_by
                    value_pairs.append((orig_rem, m.test_divisible_by))

                new_item = crt(value_pairs)
                item.set_new_level(new_item)

    all_item_counts = []
    for monkey in monkeys:
        print(f'{monkey.id}: items: {monkey.had_items}')
        all_item_counts.append(monkey.had_items)

    all_item_counts = sorted(all_item_counts)[-2:]
    monkey_business = math.prod(all_item_counts)
    return monkey_business

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    monkeys = []

    line_generator = load_input(FLAGS.input)
    current_monkey = None
    for line in line_generator:
        if current_monkey is None:
            current_monkey = Monkey()

        if len(line) == 0:
            monkeys.append(current_monkey)
            current_monkey = Monkey()
            continue

        current_monkey.feed_line(line)
    monkeys.append(current_monkey)

    monkey_business1 = run_monkey_business(deepcopy(monkeys), 20, True)
    print(f'part1: monkey business: {monkey_business1}')

    monkey_business2 = run_monkey_business(monkeys, 10000, False)
    print(f'part2: monkey business: {monkey_business2}')

if __name__ == '__main__':
    main()
