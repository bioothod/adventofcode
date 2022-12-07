from typing import Dict, List, Optional

import argparse
from enum import Enum

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

class Entry(Enum):
    DIR = 1
    FILE = 2

class DirEntry:
    def __init__(self, name: str, type: Entry, total_size: int, parent: 'DirEntry'):
        self.name = name
        self.type = type
        self.total_size = total_size

        self.parent: DirEntry = parent
        self.children: Dict[str, DirEntry] = {}

    def size(self):
        return self.total_size

    def add_entry(self, entry: 'DirEntry'):
        if entry.name not in self.children:
            self.children[entry.name] = entry
            self.total_size += entry.size()

            root = self.parent
            while root is not None:
                root.total_size += entry.size()
                root = root.parent

            #print(f'dir: {self.name}, entry: {entry.name}, entry_size: {entry.size()}, total_size: {self.total_size}')

class Env:
    def __init__(self):
        self.root = DirEntry('/', Entry.DIR, 0, None)
        self.current_dir: DirEntry = self.root

    def add_new_entry(self, name: str, type: Entry, size: int):
        entry = DirEntry(name, type, size, self.current_dir)
        self.current_dir.add_entry(entry)

    def handle_cd(self, elms: List[str]):
        dir = elms[2]
        if dir == '/':
            self.current_dir = self.root
            return
        if dir == '..':
            self.current_dir = self.current_dir.parent
            return

        entry = self.current_dir.children.get(dir)
        if entry is not None:
            if entry.type == Entry.FILE:
                raise ValueError(f'trying to enter file {dir} instead of dir: {elms}')

            self.current_dir = entry
            return

        entry = DirEntry(dir, Entry.DIR, 0, self.current_dir)
        self.current_dir.add_entry(entry)
        self.current_dir = entry
        return

    def parse(self, line_generator):
        handle_ls = False

        for line in line_generator:
            elms = line.split()

            if elms[0] == '$':
                handle_ls = False

                cmd = elms[1]
                if cmd == 'cd':
                    self.handle_cd(elms)
                    continue
                if cmd == 'ls':
                    handle_ls = True
                    continue

            if handle_ls:
                if elms[0] == 'dir':
                    name = elms[1]
                    self.add_new_entry(name, Entry.DIR, 0)
                    continue

                size = int(elms[0])
                name = elms[1]
                self.add_new_entry(name, Entry.FILE, size)
                continue


def scan_dirs_with_size_limit(root: DirEntry, size_limit: int) -> int:
    #print(f'{root.name}: size: {root.size()}')

    total_size = 0
    if root.size() <= size_limit:
        total_size += root.size()

    for child in root.children.values():
        if child.type == Entry.FILE:
            continue

        total_size += scan_dirs_with_size_limit(child, size_limit)

    return total_size


def find_suitable_dirs(root: DirEntry, size_limit: int) -> List[DirEntry]:
    return_list = []

    if root.size() >= size_limit:
        return_list.append(root)

    for child in root.children.values():
        if child.type == Entry.FILE:
            continue

        return_list += find_suitable_dirs(child, size_limit)

    return return_list

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    env = Env()
    env.parse(load_input(FLAGS.input))

    size_limit = 100000
    total_size = scan_dirs_with_size_limit(env.root, size_limit)
    print(f'part1: total_size: {total_size}')

    fs_size = 70000000
    required_for_update_size = 30000000
    used_space = env.root.size()
    free_space = fs_size - used_space
    need_to_free_size = required_for_update_size - free_space
    print(f'used_space: {used_space}, free_space: {free_space}, need_to_free: {need_to_free_size}')
    suitable_dirs = find_suitable_dirs(env.root, need_to_free_size)
    smallest = min(suitable_dirs, key=lambda x: x.size())

    print(f'part2: smallest: {smallest.size()}')

if __name__ == '__main__':
    main()
