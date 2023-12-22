use std::{collections::HashMap, fmt, hash::Hash};

use crate::config::Config;

#[derive(Clone, PartialEq, Eq, Hash)]
struct Position {
    x: isize,
    y: isize,
    z: isize,
}
impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}.{},{}]", self.x, self.y, self.z)
    }
}

impl Position {
    fn parse(line: &str) -> Position {
        let coords = line
            .trim()
            .split(",")
            .map(|c| c.parse::<isize>().unwrap())
            .collect::<Vec<isize>>();
        if coords.len() != 3 {
            panic!("invalid coords {:?} from line {}", coords, line);
        }

        Position {
            x: coords[0],
            y: coords[1],
            z: coords[2],
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
struct Brick {
    id: usize,
    start: Position,
    end: Position,
}
impl fmt::Debug for Brick {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{{}: ({:?},{:?})}}", self.id, self.start, self.end)
    }
}
impl std::hash::Hash for Brick {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.start.hash(state);
        self.end.hash(state);
    }
}

impl Brick {
    fn parse(id: usize, line: &str) -> Brick {
        let coords = line
            .split("~")
            .map(Position::parse)
            .collect::<Vec<Position>>();
        if coords.len() != 2 {
            panic!(
                "could not parse brick coords {:?} from line {}",
                coords, line
            );
        }

        let b = Brick {
            id,
            start: coords[0].clone(),
            end: coords[1].clone(),
        };

        if b.start.x > b.end.x || b.start.y > b.end.y || b.start.z > b.end.z {
            panic!(
                "invalid brick coordinates, start {:?} > end {:?}",
                b.start, b.end
            );
        }

        b
    }

    fn crossx(&self, other: &Brick) -> bool {
        let max_start = std::cmp::max(self.start.x, other.start.x);
        let min_end = std::cmp::min(self.end.x, other.end.x);
        max_start <= min_end
    }
    fn crossy(&self, other: &Brick) -> bool {
        let max_start = std::cmp::max(self.start.y, other.start.y);
        let min_end = std::cmp::min(self.end.y, other.end.y);
        max_start <= min_end
    }
    fn crossz(&self, other: &Brick) -> bool {
        let max_start = std::cmp::max(self.start.z, other.start.z);
        let min_end = std::cmp::min(self.end.z, other.end.z);
        max_start <= min_end
    }
    fn cross(&self, other: &Brick) -> bool {
        self.crossx(other) && self.crossy(other)
    }
}

struct Support {
    table: HashMap<usize, Vec<usize>>,
}
impl Support {
    fn new() -> Support {
        Support {
            table: HashMap::new(),
        }
    }

    fn insert(&mut self, key: usize, id: usize) {
        self.table.entry(key).or_insert(vec![]).push(id);
    }

    fn get(&self, key: &usize) -> Option<&Vec<usize>> {
        self.table.get(key)
    }
}
struct SupportMap {
    supports: Support,
    supported: Support,
}

impl SupportMap {
    fn new() -> SupportMap {
        SupportMap {
            supports: Support::new(),
            supported: Support::new(),
        }
    }
}

pub struct Solution {
    bricks: HashMap<usize, Brick>,
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let mut sol = Solution {
            bricks: config
                .content
                .lines()
                .enumerate()
                .map(|(id, line)| (id, Brick::parse(id, line)))
                .collect(),
        };

        let bricks: Vec<Brick> = sol.bricks.values().map(|b| b.to_owned()).collect();
        let bricks_sorted = sol.fall(&bricks);
        for brick in bricks_sorted.into_iter() {
            sol.bricks.entry(brick.id).and_modify(|ent| *ent = brick);
        }

        sol
    }

    fn fall(&self, bricks: &Vec<Brick>) -> Vec<Brick> {
        let mut bricks = bricks.clone();
        let mut bricks_sorted: Vec<Brick> = vec![];

        bricks.sort_by_key(|b| b.start.z);

        while bricks.len() != 0 {
            let mut brick = bricks.remove(0);

            let below = bricks_sorted
                .iter()
                .filter(|o| o.id != brick.id && brick.cross(o) && o.end.z < brick.start.z)
                .collect::<Vec<&Brick>>();

            let zdiff = if below.len() == 0 {
                brick.start.z - 1
            } else {
                let closest = below
                    .iter()
                    .min_by_key(|o| brick.start.z - o.end.z)
                    .unwrap();

                brick.start.z - closest.end.z - 1
            };

            brick.start.z -= zdiff;
            brick.end.z -= zdiff;

            bricks_sorted.push(brick);
        }

        if false {
            for b in bricks_sorted.iter() {
                let broken_sinlge = bricks_sorted
                    .iter()
                    .filter_map(|o| {
                        if o != b && o.cross(b) && o.crossz(b) {
                            Some(o.clone())
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<Brick>>();
                if broken_sinlge.len() > 0 {
                    println!("broken: {:?} -> {:?}", b, broken_sinlge);
                    panic!("there should be no broken locations after sorting");
                }
            }
        }

        bricks_sorted.sort_by_key(|b| b.start.z);
        bricks_sorted
    }

    fn get_support_maps(&self) -> SupportMap {
        let mut map = SupportMap::new();

        for brick in self.bricks.values() {
            let others = self
                .bricks
                .values()
                .filter(|o| o.id != brick.id && o.start.z == brick.end.z + 1 && brick.cross(o))
                .collect::<Vec<&Brick>>();

            for other in others.iter() {
                map.supports.insert(brick.id, other.id);
                map.supported.insert(other.id, brick.id);
            }
        }

        map
    }

    pub fn stage1(&self) -> usize {
        let map = self.get_support_maps();
        let mut can_be_removed = 0;
        for brick in self.bricks.values() {
            let Some(supports) = map.supports.get(&brick.id) else {
                can_be_removed += 1;
                continue;
            };

            if supports.len() == 0 {
                can_be_removed += 1;
            }

            let mut good_support = 0;
            for id in supports.iter() {
                let Some(supported) = map.supported.get(id) else {
                    continue;
                };

                if supported.len() > 1 {
                    good_support += 1;
                }
            }

            if good_support == supports.len() {
                can_be_removed += 1;
            }
        }

        can_be_removed
    }

    pub fn stage2(&self) -> usize {
        let mut will_fall = 0;
        for brick in self.bricks.values() {
            let other: Vec<Brick> = self
                .bricks
                .values()
                .filter_map(|o| {
                    if o.id != brick.id {
                        Some(o.to_owned())
                    } else {
                        None
                    }
                })
                .collect();
            let bricks_sorted = self.fall(&other);
            for o in bricks_sorted.iter() {
                let orig = self.bricks.get(&o.id).unwrap();
                if orig.start.z != o.start.z {
                    will_fall += 1;
                }
            }
        }

        will_fall
    }
}
