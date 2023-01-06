from typing import List, Dict, Tuple, Optional

from dataclasses import dataclass
from collections import defaultdict
from copy import deepcopy
from time import perf_counter

import argparse
import itertools
import joblib
import math
import re

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

class Robot:
    def __init__(self, name: str, costs: Dict[str, int]):
        self.name = name
        self.costs = costs

    def can_build(self, materials: Dict[str, int], max_build: int) -> Tuple[Dict[str, int], int]:
        materials = deepcopy(materials)

        if max_build == 0:
            return materials, max_build

        can_build = 0
        exhausted = False
        while not exhausted:
            not_exhausted_materials = deepcopy(materials)

            for material_name, material_amount in self.costs.items():
                if materials[material_name] < material_amount:
                    exhausted = True
                    materials = not_exhausted_materials
                    break

                materials[material_name] -= material_amount

            if not exhausted:
                can_build += 1

                if can_build == max_build:
                    break

        return materials, can_build

class Blueprint:
    def __init__(self, bid: int, line_generator):
        self.keys = ['ore', 'clay', 'obsidian', 'geode']

        self.robot_match_one = re.compile('Each (\\w+) robot costs (\\d+) (\\w+)\\.')
        self.robot_match_two = re.compile('Each (\\w+) robot costs (\\d+) (\\w+) and (\\d+) (\\w+)\\.')

        self.bid = bid
        self.robots = {}

        for line in line_generator:
            line = line.strip()

            costs = {}
            m = self.robot_match_one.match(line)
            if m:
                name = m.group(1)
                mineral_amount = int(m.group(2))
                mineral_name = m.group(3)
                costs[mineral_name] = mineral_amount
            else:
                m = self.robot_match_two.match(line)
                if m:
                    name = m.group(1)

                    mineral_amount = int(m.group(2))
                    mineral_name = m.group(3)
                    costs[mineral_name] = mineral_amount

                    mineral_amount = int(m.group(4))
                    mineral_name = m.group(5)
                    costs[mineral_name] = mineral_amount
                else:
                    break

            self.robots[name] = Robot(name, costs)

    def what_can_be_built_one(self, materials_provided: Dict[str, int], step: int) -> List[Dict[str, int]]:
        max_iterators = []
        max_builds = {}
        for robot_name in self.keys:
            if step == 2 and robot_name in ['ore', 'clay']:
                max_can_build = 0
            else:
                robot = self.robots[robot_name]

                _, max_can_build = robot.can_build(materials_provided, 2)
                if max_can_build == 2:
                    max_can_build = 0

            max_iterators.append(range(max_can_build + 1))
            max_builds[robot_name] = max_can_build

        builds = []
        for combination in itertools.product(*max_iterators, repeat=1):
            if sum(combination) != 1:
                continue

            materials = deepcopy(materials_provided)
            new_build = {}
            for robot_name, quantity in zip(self.keys, combination):
                materials, can_build = self.robots[robot_name].can_build(materials, quantity)
                if can_build == quantity:
                    new_build[robot_name] = quantity
                else:
                    break

            if len(new_build) == len(combination):
                builds.append(new_build)

        #print(f'materials: {dict(materials_provided)}, builds: {len(builds)}: {builds}')
        return builds

class BuildingState:
    def __init__(self):
        self.materials = defaultdict(int)
        self.robots = defaultdict(int)
        self.robots['ore'] = 1

        self.enable_operations = False
        self.operations = []

    def do_work(self):
        for material_name, num_robots in self.robots.items():
            self.materials[material_name] += num_robots

        if self.enable_operations:
            self.operations.append([
                deepcopy(self.materials),
                deepcopy(self.robots),
            ])

    def build_new(self, build: Dict[str, int], blueprint: Blueprint) -> 'BuildingState':
        state = deepcopy(self)

        for robot_name, num_robots in build.items():
            state.robots[robot_name] += num_robots

            robot = blueprint.robots[robot_name]
            for material_name, material_quantity in robot.costs.items():
                state.materials[material_name] -= material_quantity * num_robots

        if state.enable_operations:
            state.operations[-1] = [
                deepcopy(state.materials),
                deepcopy(state.robots),
            ]

        return state

    def __hash__(self) -> int:
        keys = ['ore', 'clay', 'obsidian', 'geode']
        materials = [self.materials[key] for key in keys]
        robots = [self.robots[key] for key in keys]
        all = tuple(materials + robots)
        return hash(all)

    def __eq__(self, o: 'BuildingState') -> bool:
        return self.materials == o.materials and self.robots == o.robots

    def __repr__(self) -> str:
        return f'materials: {dict(self.materials)}, robots: {dict(self.robots)}'

class Solution:
    def __init__(self, line_generator):
        blueprint_id_match = re.compile('Blueprint (\\d+):(.*)')

        self.blueprints = []

        for line in line_generator:
            m = blueprint_id_match.match(line)
            if not m:
                break

            current_bid = int(m.group(1))

            if len(m.group(2)) > 0:
                lines = m.group(2).split('.')
                lines = [line + '.' for line in lines]
                blueprint = Blueprint(current_bid, lines)
            else:
                blueprint = Blueprint(current_bid, line_generator)

            self.blueprints.append(blueprint)

    def maximize_blueprint(self, blueprint: Blueprint, num_steps: int) -> int:
        state = BuildingState()

        max_state = None
        states_to_analyze = set([state])
        major_step = 1
        for step in range(1, num_steps+1):
            start_time = perf_counter()
            new_states = set()
            num_builds = 0
            for state in states_to_analyze:
                builds = blueprint.what_can_be_built_one(state.materials, major_step)
                num_builds += len(builds)

                state.do_work()

                for build in builds:
                    new_state = state.build_new(build, blueprint)

                    if major_step == 1 and new_state.robots['geode'] > 0:
                        major_step = 2

                    new_states.add(new_state)

                new_states.add(state)

            max_state = max(new_states, key=lambda s: s.materials['geode'])

            max_geode_materials = max(new_states, key=lambda s: s.materials['geode']).materials['geode']

            step_time = perf_counter() - start_time

            print(f'{blueprint.bid}: {step}: time: {step_time:.1f}, states: {len(states_to_analyze)} -> {len(new_states)}, builds: {num_builds}, '
                  f'max_state: robots: {dict(max_state.robots)}, materials: {dict(max_state.materials)}, '
                  f'max_geode_materials: {max_geode_materials}')
            states_to_analyze = new_states

        max_state = max(states_to_analyze, key=lambda s: s.materials['geode'])
        # for step, (materials, robots) in enumerate(max_state.operations):
        #     print(f'  {step+1:2d}: materials: {dict(materials)}, robots: {dict(robots)}')
        return max_state.materials['geode']


    def max_schedules(self, num_steps: int, num_blueprints: int = -1, part2: bool = False) -> int:
        if num_blueprints < 0:
            num_blueprints = len(self.blueprints)
        blueprints = self.blueprints[:num_blueprints]

        #num_jobs = 1
        num_jobs = min(len(blueprints), 16)

        geodes = joblib.Parallel(n_jobs=num_jobs, verbose=2)([
            joblib.delayed(self.maximize_blueprint)(blueprint, num_steps) for blueprint in blueprints
        ])

        if not part2:
            metrics = [g*b.bid for g, b in zip(geodes, blueprints)]
            return sum(metrics)
        else:
            return math.prod(geodes)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    parser.add_argument('--part2', action='store_true', help='Part2')
    FLAGS = parser.parse_args()

    line_generator = load_input(FLAGS.input)
    solution = Solution(line_generator)

    if not FLAGS.part2:
        part1 = solution.max_schedules(num_steps=24)
        print(f'part1: {part1}')
    else:
        part2 = solution.max_schedules(num_steps=32, num_blueprints=3, part2=True)
        print(f'part2: {part2}')

if __name__ == '__main__':
    main()
