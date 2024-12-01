use std::fmt;

use crate::config::Config;
use crate::config::Stage;

#[derive(Clone, PartialEq)]
struct Coord {
    x: f64,
    y: f64,
    z: f64,
}
impl fmt::Debug for Coord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.x, self.y)
    }
}

impl Coord {
    fn parse(line: &str) -> Coord {
        let coords: Vec<f64> = line
            .split(",")
            .map(|c| c.trim().parse::<f64>().unwrap())
            .collect();
        if coords.len() != 3 {
            panic!(
                "could not correctly parse coord line: {} -> {:?}",
                line, coords
            );
        }
        Coord {
            x: coords[0],
            y: coords[1],
            z: coords[2],
        }
    }

    fn add(&self, other: &Coord) -> Coord {
        Coord {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
    fn mult(&self, n: f64) -> Coord {
        Coord {
            x: self.x * n,
            y: self.y * n,
            z: self.z * n,
        }
    }
}
type Velocity = Coord;

#[derive(Clone, PartialEq)]
struct Stone {
    pos: Coord,
    vel: Velocity,
    a: f64,
    b: f64,
}
impl fmt::Debug for Stone {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} @ {:?}", self.pos, self.vel)
    }
}

impl Stone {
    fn parse(line: &str) -> Stone {
        let spl = line.split("@").collect::<Vec<&str>>();
        let pos = Coord::parse(spl[0].trim());
        let vel = Velocity::parse(spl[1].trim());

        let new_pos = pos.add(&vel.mult(100_f64));
        let a = if new_pos.x == pos.x {
            0_f64
        } else {
            (new_pos.y - pos.y) / (new_pos.x - pos.x)
        };
        let b = if new_pos.x == pos.x {
            new_pos.y
        } else {
            (pos.y * new_pos.x - pos.x * new_pos.y) / (new_pos.x - pos.x)
        };

        Stone { pos, vel, a, b }
    }

    fn location_after_time(&self, x: f64) -> Coord {
        self.pos.add(&self.vel.mult(x))
    }

    fn intersection(&self, other: &Stone) -> Option<Coord> {
        if self.a == other.a {
            return None;
        }

        let x = (other.b - self.b) / (self.a - other.a);
        let y = self.a * x + self.b;

        Some(Coord { x, y, z: 0_f64 })
    }
}

pub struct Solution {
    stones: Vec<Stone>,
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let sol = Solution {
            stones: config.content.lines().map(Stone::parse).collect(),
        };

        sol
    }

    fn intersections(&self, min: f64, max: f64) -> usize {
        let mut total_intersections = 0;
        for (i, s0) in self.stones.iter().enumerate() {
            for j in (i + 1)..self.stones.len() {
                let s1 = self.stones.get(j).unwrap();

                if let Some(c) = s0.intersection(s1) {
                    let time0 = (c.x - s0.pos.x) / s0.vel.x;
                    let time1 = (c.x - s1.pos.x) / s1.vel.x;
                    // println!(
                    //     "s0: {:?}, s1: {:?}, intersecion: {:?}, time0: {:.3}, time1: {:.3}",
                    //     s0, s1, c, time0, time1
                    // );

                    if min <= c.x && c.x <= max && min <= c.y && c.y <= max {
                        if time0 > 0_f64 && time1 > 0_f64 {
                            total_intersections += 1;
                        }
                    }
                }
            }
        }

        total_intersections
    }

    pub fn stage1(&self) -> usize {
        self.intersections(200000000000000_f64, 400000000000000_f64)
        //self.intersections(7_f64, 27_f64)
    }

    pub fn stage2(&self) -> usize {
        0
    }
}
