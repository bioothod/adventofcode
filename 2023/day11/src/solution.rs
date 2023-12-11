use crate::config::Config;
use crate::config::Stage;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct Location {
    y: usize,
    x: usize,
}

fn type_distance(p0: usize, p1: usize) -> usize {
    if p0 > p1 {
        p0 - p1
    } else {
        p1 - p0
    }
}

impl Location {
    fn distance(&self, other: &Location) -> usize {
        type_distance(self.x, other.x) + type_distance(self.y, other.y)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Galaxy {
    id: usize,
    pos: Location,
}

impl Galaxy {
    fn distance(&self, other: &Galaxy) -> usize {
        let dist = self.pos.distance(&other.pos);
        dist
    }
}

struct Universe {
    galaxies: Vec<Galaxy>,
}

impl Universe {
    fn build(map: &Vec<Vec<usize>>) -> Universe {
        let mut u = Universe { galaxies: vec![] };

        for ridx in 0..map.len() {
            for cidx in 0..map[0].len() {
                if map[ridx][cidx] != 0 {
                    u.galaxies.push(Galaxy {
                        id: u.galaxies.len() + 1,
                        pos: Location { y: ridx, x: cidx },
                    });
                }
            }
        }

        u
    }
    fn expand_rows(&mut self, empty_row_ids: &Vec<usize>, space: usize) {
        for &empty_row_id in empty_row_ids.iter().rev() {
            for g in self.galaxies.iter_mut() {
                if g.pos.y > empty_row_id {
                    g.pos.y += space;
                }
            }
        }
    }

    fn expand_columns(&mut self, empty_column_ids: &Vec<usize>, space: usize) {
        for &empty_col_id in empty_column_ids.iter().rev() {
            for g in self.galaxies.iter_mut() {
                if g.pos.x > empty_col_id {
                    g.pos.x += space;
                }
            }
        }
    }

    fn all_pairs(&self) -> Vec<(usize, usize)> {
        let mut ret: Vec<(usize, usize)> = vec![];
        for id0 in 0..self.galaxies.len() {
            for id1 in (id0 + 1)..self.galaxies.len() {
                ret.push((id0, id1));
            }
        }

        ret
    }

    fn distances(&self) -> usize {
        let pairs = self.all_pairs();
        let mut dists = 0;
        for p in pairs {
            let src = self.galaxies.get(p.0).unwrap();
            let dst = self.galaxies.get(p.1).unwrap();

            let dist = src.distance(dst);
            dists += dist;
        }

        dists
    }
}

pub struct Solution {
    u: Universe,
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let map = config
            .content
            .lines()
            .map(|line| {
                line.chars()
                    .map(|c| match c {
                        '.' => 0,
                        '#' => 1,
                        _ => panic!("unsupported char {}", c),
                    })
                    .collect::<Vec<usize>>()
            })
            .collect::<Vec<_>>();

        let empty_row_ids = map
            .iter()
            .enumerate()
            .filter_map(|(idx, line)| {
                if line.iter().all(|&c| c == 0) {
                    Some(idx)
                } else {
                    None
                }
            })
            .collect::<Vec<usize>>();
        let empty_column_ids = (0..map[0].len())
            .filter_map(|cidx| {
                let column = map.iter().map(|row| row[cidx]).collect::<Vec<usize>>();
                if column.iter().all(|&c| c == 0) {
                    Some(cidx)
                } else {
                    None
                }
            })
            .collect::<Vec<usize>>();

        let mut u = Universe::build(&map);

        let space = if config.stage == Stage::ONE {
            1
        } else {
            1000000 - 1
        };

        u.expand_rows(&empty_row_ids, space);
        u.expand_columns(&empty_column_ids, space);

        Solution { u }
    }

    pub fn stage1(&self) -> usize {
        self.u.distances()
    }

    pub fn stage2(&self) -> usize {
        self.u.distances()
    }
}
