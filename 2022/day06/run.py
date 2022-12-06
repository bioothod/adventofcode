import argparse

from collections import defaultdict

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

class SetCount:
    def __init__(self):
        self.counts = defaultdict(int)

    def __len__(self):
        return len(self.counts)

    def insert(self, letter):
        self.counts[letter] += 1

    def remove(self, letter):
        self.counts[letter] -= 1
        if self.counts[letter] == 0:
            del self.counts[letter]

def process_line(line, n):
    unique = SetCount()
    for idx, l in enumerate(line):
        unique.insert(l)
        index_to_remove = idx - n
        if index_to_remove >= 0:
            unique.remove(line[index_to_remove])

        if len(unique) == n:
            return idx+1

    raise ValueError(f'there is no unique sequence of length {n} in {line}')

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    start_of_a_packet_n = 4
    start_of_a_message_n = 14
    for line in load_input(FLAGS.input):
        packet_idx = process_line(line, start_of_a_packet_n)
        message_idx = process_line(line, start_of_a_message_n)

        print(f'start: packet: {packet_idx}, message: {message_idx}')

if __name__ == '__main__':
    main()
