SCORE_STR = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'

def main1():
    input_fn = 'input.txt'
    messed_items = []
    with open(input_fn, 'r') as fin:
        for line in fin:
            if line[-1] == '\n':
                line = line[:-1]

            num_items = len(line)
            num_items_part = num_items // 2
            p0 = line[:num_items_part]
            p1 = line[num_items_part:]

            for item in p0:
                if item in p1:
                    messed_items.append(item)
                    break

    score = 0
    for item in messed_items:
        score += SCORE_STR.index(item) + 1

    print(f'score: {score}')

def main2():
    input_fn = 'input.txt'
    badges = []
    group_ransacs = []
    with open(input_fn, 'r') as fin:
        for line in fin:
            if line[-1] == '\n':
                line = line[:-1]


            found = False
            group_ransacs.append(line)
            if len(group_ransacs) == 3:
                for l0 in group_ransacs[0]:
                    for l1 in group_ransacs[1]:
                        if l1 != l0:
                            continue

                        for l2 in group_ransacs[2]:
                            if l2 == l0:
                                badges.append(l2)
                                group_ransacs = []
                                found = True
                                break

                        if found:
                            break

                    if found:
                        break

    score = 0
    for item in badges:
        score += SCORE_STR.index(item) + 1

    print(f'badges score: {score}')

if __name__ == '__main__':
    main2()
