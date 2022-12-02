from enum import Enum, IntEnum
from copy import deepcopy

SCORES = {
    'A': 1, # Rock
    'B': 2, # Paper
    'C': 3, # Scissors

    'X': 1, # Rock
    'Y': 2, # Paper
    'Z': 3, # Scissors
}

class PID(Enum):
    ROCK = 1
    PAPER = 2
    SCISSORS = 3

class SCORE(IntEnum):
    WIN = 6
    DRAW = 3
    LOST = 0

class Player:
    id: PID

    @staticmethod
    def create(letter):
        if letter in 'AX':
            return Rock()
        elif letter in 'BY':
            return Paper()
        else:
            return Scissors()

    def shape_score(self):
        SCORES = {
            PID.ROCK: 1,
            PID.PAPER: 2,
            PID.SCISSORS: 3,
        }

        return SCORES[self.id]


class Rock(Player):
    def __init__(self):
        self.id = PID.ROCK

    def winning_score(self, other: Player) -> SCORE:
        if other.id == self.id:
            return SCORE.DRAW
        if other.id == PID.SCISSORS:
            return SCORE.WIN
        return SCORE.LOST

    def create_opponent_which(self, outcome: SCORE) -> Player:
        if outcome == SCORE.DRAW:
            return deepcopy(self)
        if outcome == SCORE.WIN:
            return Paper()
        return Scissors()

class Scissors(Player):
    def __init__(self):
        self.id = PID.SCISSORS

    def winning_score(self, other: Player) -> SCORE:
        if other.id == self.id:
            return SCORE.DRAW
        if other.id == PID.PAPER:
            return SCORE.WIN
        return SCORE.LOST

    def create_opponent_which(self, outcome: SCORE) -> Player:
        if outcome == SCORE.DRAW:
            return deepcopy(self)
        if outcome == SCORE.WIN:
            return Rock()
        return Paper()

class Paper(Player):
    def __init__(self):
        self.id = PID.PAPER

    def winning_score(self, other: Player) -> SCORE:
        if other.id == self.id:
            return SCORE.DRAW
        if other.id == PID.ROCK:
            return SCORE.WIN
        return SCORE.LOST

    def create_opponent_which(self, outcome: SCORE) -> Player:
        if outcome == SCORE.DRAW:
            return deepcopy(self)
        if outcome == SCORE.WIN:
            return Scissors()
        return Rock()

def main1():
    input_fn = 'input.txt'

    total_score = 0
    with open(input_fn, 'r') as fin:
        for line in fin:
            if line[-1] == '\n':
                line = line[:-1]

            players = line.split()
            p0 = Player.create(players[0])
            p1 = Player.create(players[1])

            shape_score = p1.shape_score()
            win_score = p1.winning_score(p0)
            score = shape_score + win_score
            total_score += score

    print(total_score)

def letter_to_outcome(letter) -> SCORE:
    if letter == 'X':
        return SCORE.LOST
    if letter == 'Y':
        return SCORE.DRAW
    return SCORE.WIN

def main2():
    input_fn = 'input.txt'

    total_score = 0
    with open(input_fn, 'r') as fin:
        for line in fin:
            if line[-1] == '\n':
                line = line[:-1]

            players = line.split()
            p0 = Player.create(players[0])

            outcome = letter_to_outcome(players[1])
            p1 = p0.create_opponent_which(outcome)
            shape_score = p1.shape_score()
            score = shape_score + outcome
            total_score += score

    print(total_score)


if __name__ == '__main__':
    main2()
