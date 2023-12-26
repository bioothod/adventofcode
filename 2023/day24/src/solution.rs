use std::fmt;

use crate::config::Config;

#[derive(Clone, PartialEq)]
struct Coord {
    x: isize,
    y: isize,
    z: isize,
}
impl fmt::Debug for Coord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.x, self.y)
    }
}

#[allow(dead_code)]
impl Coord {
    fn parse(line: &str) -> Coord {
        let coords: Vec<isize> = line
            .split(",")
            .map(|c| c.trim().parse::<isize>().unwrap())
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
    fn sub(&self, other: &Coord) -> Coord {
        Coord {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z,
        }
    }
    fn mult(&self, n: isize) -> Coord {
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
}
impl fmt::Debug for Stone {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} @ {:?}", self.pos, self.vel)
    }
}

struct Vecf {
    x: f64,
    y: f64,
    z: f64,
}

impl fmt::Debug for Vecf {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:.3}.{:.3}", self.x, self.y)
    }
}

impl Vecf {
    fn from_coord(c: &Coord) -> Vecf {
        Vecf {
            x: c.x as f64,
            y: c.y as f64,
            z: c.z as f64,
        }
    }
    fn mult(&self, n: f64) -> Vecf {
        Vecf {
            x: self.x * n,
            y: self.y * n,
            z: self.z * n,
        }
    }
    fn add(&self, other: &Vecf) -> Vecf {
        Vecf {
            x: self.x + other.x,

            y: self.y + other.y,
            z: self.z + other.z,
        }
    }

    fn inside_range_2d(&self, min: isize, max: isize) -> bool {
        let min = min as f64;
        let max = max as f64;

        min <= self.x && self.x <= max && min <= self.y && self.y <= max
    }
}

impl Stone {
    fn parse(line: &str) -> Stone {
        let spl = line.split("@").collect::<Vec<&str>>();
        let pos = Coord::parse(spl[0].trim());
        let vel = Velocity::parse(spl[1].trim());

        Stone { pos, vel }
    }

    fn intersection_2d(&self, other: &Stone) -> Option<(Vecf, Vecf)> {
        let c = self.vel.clone();
        let k = other.pos.sub(&self.pos);
        let d = other.vel.clone();

        let n = d.x * c.y - d.y * c.x;

        if n == 0 {
            return None;
        }

        let k = Vecf::from_coord(&k);
        let c = Vecf::from_coord(&c);
        let d = Vecf::from_coord(&d);

        let t1 = -(k.x * c.y - k.y * c.x) / (n as f64);
        let t0 = k.x / c.x + d.x / c.x * t1;

        if t0 < 0_f64 || t1 < 0_f64 {
            return None;
        }

        let self_vt = Vecf::from_coord(&self.vel).mult(t0);
        let self_dist = Vecf::from_coord(&self.pos).add(&self_vt);

        let other_vt = Vecf::from_coord(&other.vel).mult(t1);
        let other_dist = Vecf::from_coord(&other.pos).add(&other_vt);

        // println!(
        //     "s0: {:?}, s1: {:?}, t0: {:.3} -> {:?}, t1: {:.3} -> {:?}",
        //     self, other, t0, self_dist, t1, other_dist
        // );

        Some((self_dist, other_dist))
    }

    fn crosses(&self, other: &Stone) -> bool {
        if (self.pos.x - other.pos.x) * (other.vel.y - self.vel.y)
            != (self.pos.y - other.pos.y) * (other.vel.x - self.vel.x)
        {
            return false;
        }

        if (self.pos.y - other.pos.y) * (other.vel.z - self.vel.z)
            != (self.pos.z - other.pos.z) * (other.vel.y - self.vel.y)
        {
            return false;
        }

        true
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

    fn intersections_2d(&self, min: isize, max: isize) -> usize {
        let mut total_intersections = 0;
        for (i, s0) in self.stones.iter().enumerate() {
            for j in (i + 1)..self.stones.len() {
                let s1 = self.stones.get(j).unwrap();

                if let Some((d0, d1)) = s0.intersection_2d(s1) {
                    if d0.inside_range_2d(min, max) && d1.inside_range_2d(min, max) {
                        total_intersections += 1;
                    }
                }
            }
        }

        total_intersections
    }

    pub fn stage1(&self) -> usize {
        self.intersections_2d(200000000000000, 400000000000000)
        //self.intersections(7, 27)
    }

    pub fn stage2(&self) -> usize {
        let other = Stone {
            pos: Coord {
                x: 24,
                y: 13,
                z: 10,
            },
            vel: Coord { x: -3, y: 1, z: 2 },
        };
        for stone in self.stones.iter() {
            if stone.crosses(&other) {
                println!("cross: a: {:?}, b: {:?}", stone, other);
            }
        }
        0
    }
}
