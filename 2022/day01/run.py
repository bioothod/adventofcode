def main():
    input_fn = 'input.txt'


    elf_calories = []
    current_sum = 0
    elf_id = 1

    with open(input_fn, 'r') as fin:
        for line in fin:
            is_max = ''

            if line[-1] == '\n':
                line = line[:-1]

            if len(line) == 0:
                elf_calories.append((elf_id, current_sum))

                current_sum = 0
                elf_id += 1
                continue

            current_sum += int(line)

    calories = sorted(elf_calories, key=lambda t: t[1])

    n = 3
    top = calories[-n:]
    top_sum = 0
    for elf_id, cal in top:
        top_sum += cal
    print(f'top{n}: {top_sum}')


if __name__ == '__main__':
    main()
