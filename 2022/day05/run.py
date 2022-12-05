import argparse
import re

from collections import defaultdict

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            # do not strip line, we need spaces at the beginning
            if line[-1] == '\n':
                line = line[:-1]
            yield line

def load_crates(lines_generator):
    crate_lines = []
    for line in lines_generator:
        if len(line) == 0:
            break

        crate_lines.append(line)

    keys = crate_lines[-1].split()
    keys = [int(key) for key in keys]

    # remove crate names
    crate_lines = crate_lines[:-1]

    crates = defaultdict(list)
    for line in crate_lines:
        for i, key in enumerate(keys):
            start_idx = i * 4
            end_idx = start_idx + 3

            if end_idx > len(line):
                continue

            crate = line[start_idx:end_idx]
            if crate.isspace():
                continue

            crate = crate[1:-1]
            crates[key].append(crate)

    return crates

def run_instructions(lines_generator, crates, version):
    instruction_template = re.compile('move ([\\d]+) from ([\\d]+) to ([\\d]+)')
    for instruction in lines_generator:
        res = instruction_template.match(instruction)
        if not res:
            break

        quantity = int(res.group(1))
        src = int(res.group(2))
        dst = int(res.group(3))

        crates_to_move = crates[src][:quantity]

        if version == 1:
            # crates are appended in front one by one, so it is like reversed list from src appended in front of dst
            crates_to_move = list(reversed(crates_to_move))
        elif version == 2:
            pass

        crates[dst] = crates_to_move + crates[dst]
        crates[src] = crates[src][quantity:]

    top = []
    for key in range(1, len(crates.keys())+1):
        top.append(str(crates[key][0]))

    top = ''.join(top)
    print(top)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    lines_generator = load_input(FLAGS.input)
    crates = load_crates(lines_generator)
    run_instructions(lines_generator, crates, version=2)

if __name__ == '__main__':
    main()
