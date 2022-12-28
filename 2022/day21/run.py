from typing import List, Dict, Tuple, Optional

from dataclasses import dataclass
import operator

import argparse
import re

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

OPS = {
    '+': operator.add,
    '-': operator.sub,
    '*': operator.mul,
    '/': operator.floordiv,
}

class Node:
    def __init__(self, line, part2=False):
        name, action = line.split(': ')
        self.name = name.strip()

        action = action.strip()
        items = action.split()

        self.reset()

        if len(items) == 1:
            self.value = int(items[0])
        else:
            self.operation = items[1]
            self.op = OPS[self.operation]

            self.left_name = items[0]
            self.right_name = items[2]

    def reset(self):
        self.parent: Optional['Node'] = None
        self.left_item: Optional['Node'] = None
        self.right_item: Optional['Node'] = None
        self.left_name = None
        self.right_name = None
        self.value = None

    def __eq__(self, other):
        return self.name == other.name

    def __hash__(self):
        return hash(self.name)

    def __repr__(self):
        ret = f'{self.name}: '
        if self.parent is None:
            ret += 'no-parent, '
        else:
            ret += f'parent: {self.parent.name}, '

        if self.value is not None:
            ret += f'value: {self.value}'
        else:
            ret += f'({self.left_name}, {self.right_name})'

        ret = f'[{ret}]'
        return ret

    def eval(self):
        if self.value is not None:
            return self.value

        self.value = self.op(self.left_item.eval(), self.right_item.eval())
        return self.value

class Solution:
    def __init__(self, lines, part2=False):
        nodes = [Node(line) for line in lines]
        self.nodes = {node.name:node for node in nodes}

        if part2:
            self.connect_nodes_part2()
        else:
            self.connect_nodes_part1()


    def connect_nodes_part1(self):
        for node in self.nodes.values():
            if node.value is not None:
                continue

            node.left_item = self.nodes[node.left_name]
            node.right_item = self.nodes[node.right_name]

            node.left_item.parent = node
            node.right_item.parent = node

    def derive_node(self, node: Node, parent: Node):
        if node.value is not None:
            return

        if parent.operation == '+' or parent.operation == '*':
            if parent.operation == '+':
                node.operation = '-'
            else:
                node.operation = '/'

            if parent.left_name == node.name:
                node.left_name, node.right_name = parent.name, parent.right_name
            else:
                node.left_name, node.right_name = parent.name, parent.left_name

        elif parent.operation == '-' or parent.operation == '/':
            if parent.left_name == node.name:
                if parent.operation == '-':
                    node.operation = '+'
                else:
                    node.operation = '*'

                node.left_name, node.right_name = parent.name, parent.right_name
            else:
                node.operation = parent.operation
                node.left_name, node.right_name = parent.left_name, parent.name

        node.op = OPS[node.operation]
        node.left_item = self.nodes[node.left_name]
        node.right_item = self.nodes[node.right_name]

        node.left_item.parent = node
        node.right_item.parent = node

    def connect_nodes_part2(self):
        self.connect_nodes_part1()

        humn_node = self.nodes['humn']
        if humn_node.parent is None:
            raise ValueError(f'humn node must have a parent node')

        humn_parent = humn_node.parent
        humn_node.reset()

        root = self.nodes['root']

        nodes_to_check = [(humn_node, humn_parent)]

        while len(nodes_to_check) > 0:
            new_nodes = []
            for node, parent in nodes_to_check:
                if parent.name == 'root':
                    if parent.left_item == node:
                        node.value = parent.right_item.eval()
                    else:
                        node.value = parent.left_item.eval()
                    continue

                orig_parent_parent = parent.parent
                self.derive_node(node, parent)

                new_nodes.append((parent, orig_parent_parent))
                parent.reset()

            nodes_to_check = new_nodes


        res = humn_node.eval()
        return res

    def part1(self):
        return self.nodes['root'].eval()
    def part2(self):
        return self.nodes['humn'].eval()

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    line_generator = load_input(FLAGS.input)
    lines = [line for line in line_generator]

    solution = Solution(lines)
    part1_res = solution.part1()
    print(f'part1: {part1_res}')

    solution = Solution(lines, part2=True)
    part2_res = solution.part2()
    print(f'part2: {part2_res}')

if __name__ == '__main__':
    main()
