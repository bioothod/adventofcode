use std::collections::HashMap;
use std::collections::HashSet;

use ndarray::prelude::*;
use ndarray::Array;

use crate::config::Config;
use crate::config::Stage;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum RType {
    EMPTY = 0,
    SQUARE = 1,
    ROUND = 2,
}
use RType::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Rock {
    y: usize,
    x: usize,
}

pub struct Solution {
    round: Vec<Rock>,
    square: Vec<Rock>,
    map: Array<u8, Ix2>,
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let lines = config
            .content
            .lines()
            .map(|line| line.to_owned())
            .collect::<Vec<String>>();

        let mut sol = Solution {
            round: vec![],
            square: vec![],
            map: Array::<u8, Ix2>::zeros((lines.len(), lines[0].len())),
        };

        for (y, line) in config.content.lines().enumerate() {
            for (x, c) in line.chars().enumerate() {
                let rock = Rock { y, x };
                if c == 'O' {
                    sol.map[[y, x]] = ROUND as u8;
                    sol.round.push(rock);
                } else if c == '#' {
                    sol.map[[y, x]] = SQUARE as u8;
                    sol.square.push(rock);
                }
            }
        }

        sol
    }

    fn tilt(&mut self, rocks: &Vec<Rock>, ydelta: isize, xdelta: isize) -> Vec<Rock> {
        let mut moved_rocks: Vec<Rock> = vec![];

        let ymax = self.map.shape()[0] as isize;
        let xmax = self.map.shape()[1] as isize;

        for r in rocks.iter() {
            let mut y = r.y as isize;
            let mut x = r.x as isize;

            self.map[[r.y, r.x]] = EMPTY as u8;

            while y >= 0 && y < ymax && x >= 0 && x < xmax {
                if self.map[[y as usize, x as usize]] != EMPTY as u8 {
                    break;
                }

                y += ydelta;
                x += xdelta;
            }

            y -= ydelta;
            x -= xdelta;

            let mut nr = r.clone();
            nr.y = y as usize;
            nr.x = x as usize;
            self.map[[nr.y, nr.x]] = ROUND as u8;

            moved_rocks.push(nr);
        }

        moved_rocks
    }

    pub fn stage1(&mut self) -> usize {
        let mut rocks = self.round.clone();
        rocks.sort_by(|r, o| r.y.cmp(&o.y));
        println!("rocks: {:?}", rocks);

        rocks = self.tilt(&rocks, -1, 0);
        let ymax: usize = self.map.shape()[0];
        rocks.iter().fold(0, |acc, r| acc + ymax - r.y)
    }

    fn make_step(&mut self, rocks: &mut Vec<Rock>) {
        rocks.sort_by(|r, o| r.y.cmp(&o.y));
        *rocks = self.tilt(rocks, -1, 0);

        rocks.sort_by(|r, o| r.x.cmp(&o.x));
        *rocks = self.tilt(rocks, 0, -1);

        rocks.sort_by(|r, o| o.y.cmp(&r.y));
        *rocks = self.tilt(rocks, 1, 0);

        rocks.sort_by(|r, o| o.x.cmp(&r.x));
        *rocks = self.tilt(rocks, 0, 1);
    }

    pub fn stage2(&mut self) -> usize {
        let mut rocks = self.round.clone();
        let mut prev: HashMap<Array<u8, Ix2>, usize> = HashMap::new();
        let max_steps = 1000000000;
        let mut last = 0;
        for i in 0..max_steps {
            self.make_step(&mut rocks);

            if prev.contains_key(&self.map) {
                last = i;
                break;
            }

            prev.insert(self.map.clone(), i);
        }

        let start = prev.get(&self.map).unwrap();
        let diff = last - start;
        let can_skip_mult = (max_steps - start) / diff - 1;
        let rewind_to = start + can_skip_mult * diff + 1;

        for _ in rewind_to..max_steps {
            self.make_step(&mut rocks);
        }

        let ymax: usize = self.map.shape()[0];
        rocks.iter().fold(0, |acc, r| acc + ymax - r.y)
    }
}
