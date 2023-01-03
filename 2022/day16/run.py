from typing import List, Dict, Tuple, Optional

from dataclasses import dataclass
from enum import Enum
from copy import deepcopy
from collections import defaultdict

import argparse
import re

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

class Node:
    def __init__(self, id: str):
        self.id = id

        self.rate = 0
        self.reachable = {}

    def __repr__(self):
        return f'[{self.id}, rate: {self.rate}, children: {list(self.reachable.keys())}]'

class ActionType(Enum):
    STAY = 0,
    MOVE = 1,
    OPEN = 2,

class ActionState:
    def __init__(self, actions: List[Tuple[ActionType, Node]]):
        self.action_tuples: List[Tuple[ActionType, Node]] = []
        self.opened_nodes = set()
        self.score = 0
        self.accumulated_score = 0
        self.moves_after_open = defaultdict(list)

        self.update(actions)

    def node_is_opened(self, node):
        return node.id in self.opened_nodes

    def update(self, actions: List[Tuple[ActionType, Node]]) -> 'ActionState':
        self.action_tuples = actions
        return self

    def do(self):
        self.accumulated_score += self.score

        for worker_id, (action, node) in enumerate(self.action_tuples):
            if action == ActionType.OPEN:
                if node.id not in self.opened_nodes:
                    self.score += node.rate
                    self.opened_nodes.add(node.id)
                self.moves_after_open[worker_id] = []
            if action == ActionType.MOVE:
                self.moves_after_open[worker_id].append(node.id)
            if action == ActionType.STAY:
                pass

    def clone(self, actions: List[Tuple[ActionType, Node]]) -> 'ActionState':
        return deepcopy(self).update(actions)

class Solution:
    def __init__(self, line_generator):
        self.graph = {}

        for line in line_generator:
            m = re.match('Valve (\\w+) has flow rate=(\\d+); tunnels? leads? to valves? ([\\w, ]*)', line)
            if not m:
                raise ValueError(f'could not match input string: {line}')

            valve = m.group(1)
            rate = int(m.group(2))
            dst_valves = m.group(3)
            dst_valves = [v.strip() for v in dst_valves.split(',')]

            node = self.graph.get(valve, Node(valve))
            node.rate = rate
            for dst_valve in dst_valves:
                child = self.graph.get(dst_valve, Node(dst_valve))
                self.graph[dst_valve] = child

                node.reachable[dst_valve] = child
                child.reachable[valve] = node

            self.graph[valve] = node

        self.max_rate = sum([node.rate for node in self.graph.values() if node.rate > 0])

    def new_actions(self, current_state: ActionState, num_workers: int) -> List[ActionState]:
        actions = []

        all_new_actions = []
        if current_state.score < self.max_rate:
            for worker_id, (action, node) in enumerate(current_state.action_tuples):
                new_actions = []
                if node.rate > 0 and not current_state.node_is_opened(node):
                    new_actions.append((ActionType.OPEN, node))

                for dst in node.reachable.values():
                    if dst.id not in current_state.moves_after_open[worker_id]:
                        new_actions.append((ActionType.MOVE, dst))

                all_new_actions.append(new_actions)

            if num_workers == 2:
                for t0 in all_new_actions[0]:
                    for t1 in all_new_actions[1]:
                        actions.append(current_state.clone([t0, t1]))
            else:
                for t in all_new_actions[0]:
                    actions.append(current_state.clone([t]))
        else:
            actions = [current_state]

        return actions

    def open_max(self, budget: int, num_workers: int) -> int:
        start = self.graph['AA']
        stay = ActionState([(ActionType.STAY, start)] * num_workers)

        actions_to_explore = self.new_actions(stay, num_workers)
        for step in range(budget):
            new_actions = []
            for action in actions_to_explore:
                action.do()

            actions_to_explore = sorted(actions_to_explore, key=lambda a: a.accumulated_score, reverse=True)
            num_actions = len(actions_to_explore)
            max_actions = 1000
            actions_to_explore = actions_to_explore[:max_actions]
            max_score_action = actions_to_explore[0]
            for action in actions_to_explore:
                new_actions += self.new_actions(action, num_workers)

            print(f'{step}: actions: {num_actions}/{len(actions_to_explore)} -> {len(new_actions)}, '
                  f'max_score: {max_score_action.accumulated_score}, step_score: {max_score_action.score}/{self.max_rate}, '
                  f'num_opened: {len(max_score_action.opened_nodes)}/{len(self.graph)}')
            actions_to_explore = new_actions

        max_score_action = max(actions_to_explore, key=lambda a: a.accumulated_score)

        return max_score_action.accumulated_score

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    line_generator = load_input(FLAGS.input)
    solution = Solution(line_generator)

    part1 = solution.open_max(budget=30, num_workers=1)
    print(f'part1: {part1}')

    part2 = solution.open_max(budget=26, num_workers=2)
    print(f'part2: {part2}')


if __name__ == '__main__':
    main()
