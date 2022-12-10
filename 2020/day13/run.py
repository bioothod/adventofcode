import argparse
import itertools

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

class TimeTable:
    def __init__(self, line_generator):
        start_time = next(line_generator)
        self.start_time = int(start_time)

        bus_ids = next(line_generator)
        self.bus_ids = []
        for bus_id in bus_ids.split(','):
            if bus_id != 'x':
                bus_id = int(bus_id)
                self.bus_ids.append(bus_id)

        print(f'time: {self.start_time}, bus_ids: {self.bus_ids}')

    def find_bus(self):
        earliest_bus_id = 0
        earliest_time_diff = self.start_time

        for bus_id in self.bus_ids:
            idx = self.start_time // bus_id
            bus_start_time = idx * bus_id
            if bus_start_time == self.start_time:
                return 0

            time_diff = bus_start_time + bus_id - self.start_time
            if time_diff < earliest_time_diff:
                earliest_time_diff = time_diff
                earliest_bus_id = bus_id

        return earliest_bus_id * earliest_time_diff

class Timestamp:
    def __init__(self, line):
        self.bus_ids = []
        for off, bus_id in enumerate(line.split(',')):
            if bus_id != 'x':
                bus_id = int(bus_id)
                if off == 0:
                    self.bus_ids.append((off, bus_id))
                else:
                    off = bus_id - off
                    while off < 0:
                        off += bus_id
                    self.bus_ids.append((off, bus_id))
                #self.bus_ids.append((off, bus_id))

        self.bus_ids = sorted(self.bus_ids, key=lambda x: x[1], reverse=True)

        bus_id_mult = 1
        start = self.bus_ids[0][0]
        for bus_idx in range(0, len(self.bus_ids)-1):
            off, bus_id = self.bus_ids[bus_idx]
            next_off, next_bus_id = self.bus_ids[bus_idx+1]
            step = bus_id_mult * bus_id
            for number in itertools.count(start=start, step=step):
                if number % next_bus_id == next_off:
                    bus_id_mult *= bus_id
                    start = number
                    break

        print(f'part2: {number}')

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    parser.add_argument('--bus_ids', type=str, help='Bus ids line')
    FLAGS = parser.parse_args()

    time_table = TimeTable(load_input(FLAGS.input))
    res = time_table.find_bus()
    print(f'part1: {res}')

    if FLAGS.bus_ids:
        bus_ids = FLAGS.bus_ids
    else:
        line_generator = load_input(FLAGS.input)
        _ = next(line_generator)
        bus_ids = next(line_generator)

    ts = Timestamp(bus_ids)

if __name__ == '__main__':
    main()
