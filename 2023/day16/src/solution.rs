use std::collections::HashSet;

use crate::config::Config;
use crate::config::Stage;

#[derive(Eq, Debug, Clone, PartialEq, Hash)]
enum Direction {
    LEFT,
    RIGHT,
    UP,
    DOWN,
}
use Direction::*;

#[derive(Eq, Debug, Clone, PartialEq, Hash)]
enum TileType {
    Empty,
    SplitV,
    SplitH,
    MirrorRU,
    MirrorRD,
}
use TileType::*;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct Position {
    y: isize,
    x: isize,
}

impl Position {
    fn up(&self) -> Position {
        Position {
            y: self.y - 1,
            x: self.x,
        }
    }

    fn down(&self) -> Position {
        Position {
            y: self.y + 1,
            x: self.x,
        }
    }

    fn left(&self) -> Position {
        Position {
            y: self.y,
            x: self.x - 1,
        }
    }

    fn right(&self) -> Position {
        Position {
            y: self.y,
            x: self.x + 1,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct Move {
    pos: Position,
    dir: Direction,
}

impl Move {}

struct Tile {
    tp: TileType,
    energy: usize,
    pos: Position,
}

impl Tile {
    fn empty(&self, dir: Direction) -> Vec<Move> {
        vec![Move {
            pos: match dir {
                LEFT => self.pos.left(),
                RIGHT => self.pos.right(),
                UP => self.pos.up(),
                DOWN => self.pos.down(),
            },
            dir: dir.clone(),
        }]
    }

    fn split_v(&self, dir: Direction) -> Vec<Move> {
        match dir {
            LEFT | RIGHT => vec![
                Move {
                    pos: self.pos.up(),
                    dir: UP,
                },
                Move {
                    pos: self.pos.down(),
                    dir: DOWN,
                },
            ],
            UP | DOWN => self.empty(dir),
        }
    }
    fn split_h(&self, dir: Direction) -> Vec<Move> {
        match dir {
            LEFT | RIGHT => self.empty(dir),
            UP | DOWN => vec![
                Move {
                    pos: self.pos.left(),
                    dir: LEFT,
                },
                Move {
                    pos: self.pos.right(),
                    dir: RIGHT,
                },
            ],
        }
    }
    fn mirror_ru(&self, dir: Direction) -> Vec<Move> {
        match dir {
            LEFT => vec![Move {
                pos: self.pos.down(),
                dir: DOWN,
            }],
            RIGHT => vec![Move {
                pos: self.pos.up(),
                dir: UP,
            }],
            UP => vec![Move {
                pos: self.pos.right(),
                dir: RIGHT,
            }],
            DOWN => vec![Move {
                pos: self.pos.left(),
                dir: LEFT,
            }],
        }
    }
    fn mirror_rd(&self, dir: Direction) -> Vec<Move> {
        match dir {
            LEFT => vec![Move {
                pos: self.pos.up(),
                dir: UP,
            }],
            RIGHT => vec![Move {
                pos: self.pos.down(),
                dir: DOWN,
            }],
            UP => vec![Move {
                pos: self.pos.left(),
                dir: LEFT,
            }],
            DOWN => vec![Move {
                pos: self.pos.right(),
                dir: RIGHT,
            }],
        }
    }

    fn process(&self, dir: Direction, ymax: usize, xmax: usize) -> Vec<Move> {
        match self.tp {
            Empty => self.empty(dir),
            SplitH => self.split_h(dir),
            SplitV => self.split_v(dir),
            MirrorRU => self.mirror_ru(dir),
            MirrorRD => self.mirror_rd(dir),
        }
        .into_iter()
        .filter(|m| {
            m.pos.x >= 0 && (m.pos.x as usize) < xmax && m.pos.y >= 0 && (m.pos.y as usize) < ymax
        })
        .collect()
    }
}

pub struct Solution {
    map: Vec<Vec<Tile>>,
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let sol = Solution {
            map: config
                .content
                .lines()
                .enumerate()
                .map(|(y, line)| {
                    line.chars()
                        .enumerate()
                        .map(|(x, c)| {
                            let tp = match c {
                                '.' => Empty,
                                '|' => SplitV,
                                '-' => SplitH,
                                '\\' => MirrorRD,
                                '/' => MirrorRU,
                                _ => panic!("unsupported char {} in line {}", c, line),
                            };

                            Tile {
                                tp,
                                energy: 0,
                                pos: Position {
                                    y: y as isize,
                                    x: x as isize,
                                },
                            }
                        })
                        .collect::<Vec<Tile>>()
                })
                .collect(),
        };

        sol
    }

    fn get(&self, pos: &Position) -> &Tile {
        self.map
            .get(pos.y as usize)
            .unwrap()
            .get(pos.x as usize)
            .unwrap()
    }
    fn get_mut(&mut self, pos: &Position) -> &mut Tile {
        self.map
            .get_mut(pos.y as usize)
            .unwrap()
            .get_mut(pos.x as usize)
            .unwrap()
    }

    fn step(&mut self, mv: &Move) -> Vec<Move> {
        self.get(&mv.pos)
            .process(mv.dir.clone(), self.map.len(), self.map[0].len())
            .iter()
            .map(|mv| {
                self.get_mut(&mv.pos).energy += 1;
                mv.clone()
            })
            .collect()
    }

    fn clear(&mut self) {
        for y in 0..self.map.len() {
            for x in 0..self.map[0].len() {
                self.map[y][x].energy = 0;
            }
        }
    }

    fn energize(&mut self, start: &Move) -> usize {
        let mut cache: HashSet<Move> = HashSet::new();
        self.clear();

        let mut start: Vec<Move> = vec![start.clone()];
        let _ = start
            .iter()
            .map(|mv| self.get_mut(&mv.pos).energy += 1)
            .collect::<Vec<_>>();

        while start.len() > 0 {
            let moves = start
                .iter()
                .fold(vec![], |acc, mv| {
                    let mut new_moves: Vec<Move> = self
                        .step(mv)
                        .iter()
                        .filter(|&mv| !cache.contains(mv))
                        .map(|mv| mv.clone())
                        .collect::<Vec<Move>>();
                    new_moves.extend(acc);
                    new_moves
                })
                .into_iter()
                .collect::<Vec<Move>>();

            let _ = moves
                .iter()
                .map(|m| cache.insert(m.clone()))
                .collect::<Vec<_>>();

            start = moves;
        }

        let mut energized = 0;
        for y in 0..self.map.len() {
            for x in 0..self.map[0].len() {
                if self.map[y][x].energy > 0 {
                    energized += 1;
                }
            }
        }
        energized
    }

    pub fn stage1(&mut self) -> usize {
        let start = Move {
            pos: Position { y: 0, x: 0 },
            dir: RIGHT,
        };
        self.energize(&start)
    }

    pub fn stage2(&mut self) -> usize {
        let mut max_energy = 0;

        max_energy = std::cmp::max(
            max_energy,
            (0..self.map.len())
                .map(|y| {
                    let start = Move {
                        pos: Position {
                            y: y as isize,
                            x: 0,
                        },
                        dir: RIGHT,
                    };
                    self.energize(&start)
                })
                .max()
                .unwrap(),
        );

        max_energy = std::cmp::max(
            max_energy,
            (0..self.map.len())
                .map(|y| {
                    let start = Move {
                        pos: Position {
                            y: y as isize,
                            x: (self.map[0].len() - 1) as isize,
                        },
                        dir: LEFT,
                    };
                    self.energize(&start)
                })
                .max()
                .unwrap(),
        );

        max_energy = std::cmp::max(
            max_energy,
            (0..self.map[0].len())
                .map(|x| {
                    let start = Move {
                        pos: Position {
                            y: 0,
                            x: x as isize,
                        },
                        dir: DOWN,
                    };
                    self.energize(&start)
                })
                .max()
                .unwrap(),
        );

        max_energy = std::cmp::max(
            max_energy,
            (0..self.map[0].len())
                .map(|x| {
                    let start = Move {
                        pos: Position {
                            y: (self.map.len() - 1) as isize,
                            x: x as isize,
                        },
                        dir: UP,
                    };
                    self.energize(&start)
                })
                .max()
                .unwrap(),
        );

        max_energy
    }
}
