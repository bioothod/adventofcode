import argparse

def load_input(fn):
    with open(fn, 'r') as fin:
        for line in fin:
            line = line.strip()
            yield line

class CPU:
    def __init__(self):
        self.reg = 1

        self.cycle = 1
        self.program = []

    def push_line(self, line):
        if line == 'noop':
            self.program.append((self.cycle, self.reg))
            self.cycle += 1
            return
        if line.startswith('addx '):
            self.program.append((self.cycle, self.reg))

            cmd, val = line.split()
            self.cycle += 2
            self.reg += int(val)
            return

        raise ValueError(f'unsupported commdn {line}')

    def calc_signal_strength(self, cycles):
        signal = 0
        for idx, (cycle, reg) in enumerate(self.program):
            if cycle == cycles[0]:
                strength = cycles[0] * reg
                signal += strength

                cycles.pop(0)

            elif idx >= 1:
                prev_cycle, prev_reg = self.program[idx - 1]

                if (prev_cycle < cycles[0] and cycle > cycles[0]):
                    strength = cycles[0] * prev_reg
                    signal += strength
                    cycles.pop(0)

            if len(cycles) == 0:
                break

        return signal

class CRT:
    def __init__(self):
        self.max_x = 40
        self.max_y = 6

        self.display = []
        for _ in range(self.max_y):
            self.display.append(['_'] * self.max_x)

        self.sprite_pos = 1
        self.cycle = 1

        self.program = []

    def draw(self):
        cycle = 1
        program_gen = iter(self.program)
        start_cycle, duration, sprite_pos = next(program_gen)

        try:
            for y_pos in range(self.max_y):
                for x_pos in range(self.max_x):
                    if x_pos >= sprite_pos-1 and x_pos <= sprite_pos+1:
                        self.display[y_pos][x_pos] = '#'
                    else:
                        self.display[y_pos][x_pos] = '.'
                    #print(f'{x_pos}.{y_pos}: cycle: {cycle}, inst: cycle: {start_cycle}, duration: {duration}, sprite_pos: {sprite_pos}, {"".join(self.display[y_pos])}')

                    cycle += 1
                    if cycle >= start_cycle + duration:
                        start_cycle, duration, sprite_pos = next(program_gen)

        except StopIteration:
            pass

        display = [''.join(row) for row in self.display]
        display = '\n'.join(display)
        return display

    def push_line(self, line):

        if line == 'noop':
            self.program.append((self.cycle, 1, self.sprite_pos))
            self.cycle += 1
            return
        if line.startswith('addx '):
            self.program.append((self.cycle, 2, self.sprite_pos))
            cmd, val = line.split()
            self.cycle += 2
            self.sprite_pos += int(val)
            return

        raise ValueError(f'unsupported commdn {line}')

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', required=True, type=str, help='Input file')
    FLAGS = parser.parse_args()

    cpu = CPU()
    for line in load_input(FLAGS.input):
        cpu.push_line(line)

    cycles = [20, 60, 100, 140, 180, 220]
    signal = cpu.calc_signal_strength(cycles)
    print(f'part1: signal strength: {signal}')

    crt = CRT()
    for line in load_input(FLAGS.input):
        crt.push_line(line)

    display = crt.draw()
    print(display)

if __name__ == '__main__':
    main()
