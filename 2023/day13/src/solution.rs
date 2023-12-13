use crate::config::Config;
use crate::config::Stage;

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
enum Stone {
    ASH,
    ROCK,
}
use Stone::*;

#[derive(Clone)]
struct Mirror {
    id: usize,
    data: Vec<Vec<Stone>>,
}

impl Mirror {
    fn new(id: usize) -> Mirror {
        Mirror { id, data: vec![] }
    }

    fn feed(&mut self, line: &str) {
        let l = line
            .chars()
            .map(|c| match c {
                '.' => ASH,
                '#' => ROCK,
                _ => panic!("unsupported char {}", c),
            })
            .collect::<Vec<Stone>>();
        self.data.push(l);
    }

    fn chunks_equal_by_rows(&self, c0: &[Vec<Stone>], c1: &[Vec<Stone>]) -> bool {
        let position = c0
            .iter()
            .rev()
            .zip(c1.iter())
            .position(|(c0l, c1l)| c0l != c1l);
        return position.is_none();
    }

    fn chunks_equal_by_columns(&self, c0: &[Vec<Stone>], c1: &[Vec<Stone>]) -> bool {
        c0.iter()
            .zip(c1.iter())
            .position(|(c0l, c1l)| {
                c0l.iter()
                    .rev()
                    .zip(c1l.iter())
                    .position(|(c0e, c1e)| c0e != c1e)
                    .is_some()
            })
            .is_none()
    }

    fn try_row_reflection_by_idx(&self, ridx: usize) -> Option<usize> {
        let c0 = &self.data[0..ridx];
        let c1 = &self.data[ridx..self.data.len()];
        if self.chunks_equal_by_rows(c0, c1) {
            return Some(ridx);
        }

        None
    }

    fn try_row_reflection(&self, skip: Option<usize>) -> Option<usize> {
        for ridx in 1..self.data.len() {
            if let Some(s) = skip {
                if ridx == s {
                    continue;
                }
            }
            if let Some(ridx) = self.try_row_reflection_by_idx(ridx) {
                return Some(ridx);
            }
        }

        None
    }

    fn try_column_reflection_by_idx(&self, cidx: usize) -> Option<usize> {
        let c0 = self
            .data
            .iter()
            .map(|line| line[0..cidx].to_vec())
            .collect::<Vec<Vec<Stone>>>();
        let c1 = self
            .data
            .iter()
            .map(|line| line[cidx..line.len()].to_vec())
            .collect::<Vec<Vec<Stone>>>();

        if self.chunks_equal_by_columns(&c0, &c1) {
            return Some(cidx);
        }

        None
    }

    fn try_column_reflection(&self, skip: Option<usize>) -> Option<usize> {
        for cidx in 1..self.data[0].len() {
            if let Some(c) = skip {
                if c == cidx {
                    continue;
                }
            }

            if let Some(cidx) = self.try_column_reflection_by_idx(cidx) {
                return Some(cidx);
            }
        }

        None
    }

    fn find_smudge(&mut self) -> usize {
        let orig_column = self.try_column_reflection(None);
        let orig_row = self.try_row_reflection(None);

        for row in 0..self.data.len() {
            for col in 0..self.data[0].len() {
                let old = self.data[row][col];
                self.data[row][col] = match old {
                    ASH => ROCK,
                    ROCK => ASH,
                };

                if let Some(cidx) = self.try_column_reflection(orig_column) {
                    return cidx;
                }

                if let Some(ridx) = self.try_row_reflection(orig_row) {
                    return 100 * ridx;
                }

                self.data[row][col] = old;
            }
        }

        println!("id: {}, could not find smudge", self.id);
        0
    }
}

pub struct Solution {
    mirrors: Vec<Mirror>,
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let mut sol = Solution {
            mirrors: vec![Mirror::new(0)],
        };
        for line in config.content.lines() {
            if line.len() == 0 {
                sol.mirrors.push(Mirror::new(sol.mirrors.len()));
                continue;
            }

            sol.mirrors.last_mut().unwrap().feed(line);
        }

        sol
    }

    pub fn stage1(&self) -> usize {
        let mut ret = 0;
        for m in self.mirrors.iter() {
            if let Some(cidx) = m.try_column_reflection(None) {
                ret += cidx;
                continue;
            }

            if let Some(ridx) = m.try_row_reflection(None) {
                ret += 100 * ridx;
                continue;
            }
        }
        ret
    }

    pub fn stage2(&mut self) -> usize {
        let mut ret = 0;
        for m in self.mirrors.iter_mut() {
            ret += m.find_smudge();
        }
        ret
    }
}
