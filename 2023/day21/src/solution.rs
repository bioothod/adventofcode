use std::collections::{HashMap, HashSet};

use crate::config::Config;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
enum Step {
    N,
    S,
    E,
    W,
}

impl Step {
    fn reverse(&self) -> Step {
        match self {
            Step::N => Step::S,
            Step::S => Step::N,
            Step::E => Step::W,
            Step::W => Step::E,
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
enum Garden {
    Plot,
    Rock,
}
use Garden::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Coord {
    y: isize,
    x: isize,
}

impl Coord {
    fn step(&self, s: &Step, map: &Vec<Vec<Garden>>) -> Option<Coord> {
        let mut x = self.x;
        let mut y = self.y;
        match s {
            Step::N => y -= 1,
            Step::S => y += 1,
            Step::W => x -= 1,
            Step::E => x += 1,
        };

        let my = map.len() as isize;
        let mx = map[0].len() as isize;

        let mut moved_x = x % mx;
        let mut moved_y = y % my;
        if moved_x < 0 {
            moved_x += mx;
        }
        if moved_y < 0 {
            moved_y += my;
        }

        let map_x = moved_x as usize;
        let map_y = moved_y as usize;

        if map[map_y][map_x] == Rock {
            return None;
        }

        Some(Coord { y, x })
    }
}

struct Map {
    outputs: HashMap<Coord, usize>,
    points: HashMap<Coord, usize>,
}

impl Map {
    fn empty() -> Map {
        Map {
            outputs: HashMap::new(),
            points: HashMap::new(),
        }
    }
    fn create(start: &Coord, garden: &Vec<Vec<Garden>>) -> Map {
        let mut meta = Map::empty();
        let mut step_num = 1;

        let mx = garden[0].len() as isize;
        let my = garden.len() as isize;

        let mut start_points = vec![start.clone()];
        while start_points.len() != 0 {
            let mut new_start_points: Vec<Coord> = vec![];
            for src in start_points.iter() {
                for step in [Step::N, Step::S, Step::E, Step::W].iter() {
                    let Some(dst) = src.step(step, garden) else {
                        continue;
                    };

                    if meta.points.contains_key(&dst) {
                        continue;
                    }

                    if dst.x < 0 || dst.y < 0 || dst.x >= mx || dst.y >= my {
                        let mut new_map_dst = dst.clone();
                        new_map_dst.x %= mx;
                        new_map_dst.y %= my;
                        if new_map_dst.x < 0 {
                            new_map_dst.x += mx;
                        }
                        if new_map_dst.y < 0 {
                            new_map_dst.y += my;
                        }
                        let ent = meta.outputs.entry(new_map_dst).or_insert(step_num);
                        if *ent > step_num {
                            *ent = step_num;
                        }
                        // println!(
                        //     "start: {:?}, reached boundary: {:?} {}/{}",
                        //     start, dst, step_num, max_steps
                        // );
                        continue;
                    }

                    let ent = meta.points.entry(dst.clone()).or_insert(step_num);
                    if *ent % 2 != 0 && step_num % 2 == 0 {
                        *ent = step_num;
                        println!("{:?} {}", dst, ent);
                    }

                    new_start_points.push(dst.clone());
                }
            }

            if new_start_points.len() == 0 {
                break;
            }
            start_points = new_start_points;
            step_num += 1;
        }

        meta
    }

    fn count(&self, start_count: usize, max_steps: usize) -> usize {
        self.points
            .values()
            .filter(|&v| {
                let v = v + start_count;
                v <= max_steps && v % 2 == 0
            })
            .count()
    }

    fn merge(
        &mut self,
        start_count: usize,
        other: &Map,
        other_start_count: usize,
        max_steps: usize,
    ) {
        let mut ret_map = Map::empty();
        let mut insert = |start_count: usize, map: &Map| {
            for (coord, dist) in map.points.iter() {
                let v = dist + start_count;
                if v <= max_steps && v % 2 == 0 {
                    // println!(
                    //     "coord: {:?}, start_count: {}, dist: {}, v: {}/{}",
                    //     coord, start_count, dist, v, max_steps
                    // );
                    ret_map.points.insert(coord.clone(), v);
                }
            }
        };

        insert(start_count, self);
        insert(other_start_count, other);

        self.points = ret_map.points;
    }
}

pub struct Solution {
    map: Vec<Vec<Garden>>,
    start: Coord,
    mx: isize,
    my: isize,
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let mut start = Coord { y: 0, x: 0 };
        let mut sol = Solution {
            map: config
                .content
                .lines()
                .enumerate()
                .map(|(y, line)| {
                    line.chars()
                        .enumerate()
                        .map(|(x, c)| {
                            let g = match c {
                                '.' | 'S' => Plot,
                                '#' => Rock,
                                _ => panic!("unsupported char {} in line {}", c, line),
                            };

                            if c == 'S' {
                                start = Coord {
                                    y: y as isize,
                                    x: x as isize,
                                };
                            }

                            g
                        })
                        .collect::<Vec<Garden>>()
                })
                .collect(),
            start: start.clone(),
            mx: 0,
            my: 0,
        };

        sol.start = start;
        sol.mx = sol.map[0].len() as isize;
        sol.my = sol.map.len() as isize;

        sol
    }

    pub fn stage1(&self) -> usize {
        let map = Map::create(&self.start, &self.map);
        map.count(0, 64)
    }

    // fn try_one1(&self) -> usize {
    //     let max_steps: usize = 6;
    //     let mut reached = 0;
    //     let mut r_cache: HashMap<usize, usize> = HashMap::new();
    //     for n in 1..=max_steps {
    //         let st = (n + 1).pow(2);

    //         let r = if n <= (self.mx as usize - 1) / 2 {
    //             let f = self.count_rocks_front(n as isize);
    //             let rc = r_cache.get(&(n - 2)).or(Some(&0)).unwrap();
    //             //println!("n: {}, f: {}, cached: {}", n, f, rc);
    //             f + rc
    //         } else {
    //             let p2 = r_cache.get(&(n - 2)).unwrap();
    //             let p1 = r_cache.get(&(n - 1)).unwrap();
    //             p1 + p2
    //         };

    //         let reachable = st - r;

    //         println!("n: {}, st: {}, r: {}, reacable: {}", n, st, r, reachable);
    //         r_cache.insert(n, r);

    //         reached = st - r;
    //     }

    //     reached
    // }

    fn try_one(&self) -> usize {
        let max_steps: usize = 26501365;
        let mut reached = 0;
        let mut f_cache: HashMap<usize, usize> = HashMap::new();

        let mut map: HashSet<Coord> = HashSet::new();
        for y in 0..self.my {
            for x in 0..self.mx {
                if self.map[y as usize][x as usize] != Garden::Rock {
                    let c = Coord { y, x };
                    map.insert(c);
                }
            }
        }
        println!("non-rocks: {}", map.len());

        let mut boundaries: HashSet<Coord> = HashSet::new();
        let mut dist_boundaries: HashMap<Coord, usize> = HashMap::new();
        let gen_boundaries = |d: isize| {
            let s = Coord {
                y: -d * self.my,
                x: -d * self.mx,
            };
            let e = Coord {
                y: (d + 1) * self.my - 1,
                x: (d + 1) * self.mx - 1,
            };

            let mut b: HashSet<Coord> = HashSet::new();
            for y in s.y..=e.y {
                for x in [s.x, e.x].into_iter() {
                    b.insert(Coord { y, x });
                }
            }
            for x in s.x..=e.x {
                for y in [s.y, e.y].into_iter() {
                    b.insert(Coord { y, x });
                }
            }

            b
        };

        let d_start = 2;
        for d in d_start..(d_start + 1) {
            let b = gen_boundaries(d);
            for c in b.iter() {
                boundaries.insert(c.clone());
            }
        }

        let mut frontiers: Vec<HashSet<Coord>> = vec![HashSet::new()];
        frontiers.last_mut().unwrap().insert(self.start.clone());
        f_cache.insert(0, 1);

        for n in 1..=max_steps {
            let mut new_frontier: HashSet<Coord> = HashSet::new();
            for p in frontiers.last().unwrap().iter() {
                let _ = [
                    Coord { y: 0, x: -1 },
                    Coord { y: 0, x: 1 },
                    Coord { y: -1, x: 0 },
                    Coord { y: 1, x: 0 },
                ]
                .iter()
                .map(|c| {
                    let mut new_pos = Coord {
                        y: p.y + c.y,
                        x: p.x + c.x,
                    };
                    let new_pos_for_cache = new_pos.clone();

                    new_pos.y %= self.my;
                    new_pos.x %= self.mx;

                    if new_pos.y < 0 {
                        new_pos.y += self.my;
                    }
                    if new_pos.y == self.my {
                        new_pos.y = 0;
                    }
                    if new_pos.x < 0 {
                        new_pos.x += self.mx;
                    }
                    if new_pos.x == self.mx {
                        new_pos.x = 0;
                    }

                    if n > 1 {
                        if frontiers[0].contains(&new_pos_for_cache) {
                            return;
                        }
                    }

                    if new_frontier.contains(&new_pos_for_cache) {
                        return;
                    }

                    if boundaries.contains(&new_pos_for_cache) {
                        if !dist_boundaries.contains_key(&new_pos_for_cache) {
                            dist_boundaries.insert(new_pos_for_cache.clone(), n);
                        }
                    }

                    if self.map[new_pos.y as usize][new_pos.x as usize] != Rock {
                        new_frontier.insert(new_pos_for_cache);
                    }
                })
                .collect::<Vec<_>>();
            }

            let b = gen_boundaries(d_start);
            if dist_boundaries.len() == b.len() {
                println!(
                    "d: {}, boundary lens: {}, dists: {}",
                    d_start,
                    b.len(),
                    dist_boundaries.len()
                );
                for c in b.iter() {
                    println!(" {:?}  {:?}", c, dist_boundaries.get(c).unwrap());
                }
            }

            let new_len = new_frontier.len();
            let fc = f_cache.get(&(n - 2)).or(Some(&0)).unwrap();
            reached = new_len + fc;
            if n % 1 == 0 {
                println!(
                    "n: {}, new_len: {}, cached: {}, reached: {}, dist_boundaries: {}/{}, frontier: {} -> {}",
                    n,
                    new_len,
                    fc,
                    reached,
                    dist_boundaries.len(),
                    boundaries.len(),
                    frontiers.last().unwrap().len(),
                    new_frontier.len()
                );
            }

            frontiers.push(new_frontier);
            if frontiers.len() > 2 {
                frontiers.remove(0);
            }

            f_cache.insert(n, reached);

            if new_len == 0 {
                break;
            }
        }

        reached
    }

    pub fn stage2(&self) -> usize {
        return self.try_one();

        let mut meta_cache: HashMap<Coord, Map> = HashMap::new();
        for y in 0..self.my {
            for start in [Coord { y, x: 0 }, Coord { y, x: self.mx - 1 }] {
                meta_cache.insert(start.clone(), Map::create(&start, &self.map));
            }
        }
        for x in 0..self.mx {
            for start in [Coord { y: 0, x }, Coord { y: self.my - 1, x }] {
                meta_cache.insert(start.clone(), Map::create(&start, &self.map));
            }
        }
        meta_cache.insert(self.start.clone(), Map::create(&self.start, &self.map));

        let mut non_rocks = 0;
        for y in 0..self.map.len() {
            for x in 0..self.map[0].len() {
                if self.map[y][x] != Rock {
                    non_rocks += 1;
                }
            }
        }
        for (start, meta) in meta_cache.iter() {
            println!(
                "meta: start: {:?}, points: {}, non_rocks: {}, outputs: {}",
                start,
                meta.points.len(),
                non_rocks,
                meta.outputs.len()
            );
        }

        //let max_steps = 26501365;
        let max_steps: usize = 50;

        let mut total_count = 0;
        let mut start_points: HashMap<Coord, usize> = HashMap::from([(self.start.clone(), 0)]);
        while start_points.len() != 0 {
            let mut new_start_points: HashMap<Coord, usize> = HashMap::new();

            let mut caches: HashMap<Step, Map> = HashMap::from([
                (Step::N, Map::empty()),
                (Step::S, Map::empty()),
                (Step::E, Map::empty()),
                (Step::W, Map::empty()),
            ]);

            for (start, start_offset) in start_points.iter() {
                let meta = meta_cache.get(start).unwrap();

                for (cross, dist) in meta.outputs.iter() {
                    if cross.x == 0 && start.x == self.mx - 1 {
                        continue;
                    }
                    if cross.y == 0 && start.y == self.my - 1 {
                        continue;
                    }
                    if start.x == 0 && cross.x == self.mx - 1 {
                        continue;
                    }
                    if start.y == 0 && cross.y == self.my - 1 {
                        continue;
                    }
                    let dist = *dist + start_offset;
                    if dist < max_steps {
                        // println!(
                        //     "outputs: start: {:?}, cross: {:?}, start_offset: {}, dist: {}",
                        //     start, cross, start_offset, dist
                        // );

                        let ent = new_start_points.entry(cross.clone()).or_insert(dist);
                        if *ent < dist {
                            *ent = dist;
                        }
                    }
                }

                let key = if start.x == 0 {
                    Some(Step::W)
                } else if start.x == self.mx - 1 {
                    Some(Step::E)
                } else if start.y == 0 {
                    Some(Step::N)
                } else if start.y == self.my - 1 {
                    Some(Step::S)
                } else {
                    None
                };

                if let Some(key) = key {
                    let count = caches.get(&key).unwrap().count(0, max_steps);
                    caches
                        .get_mut(&key)
                        .unwrap()
                        .merge(0, meta, *start_offset, max_steps);
                    let new_count = caches.get(&key).unwrap().count(0, max_steps);
                    println!(
                        "merge: start: {:?}, start_offset: {}, key: {:?}, count: {} -> {}",
                        start, start_offset, key, count, new_count
                    );
                } else {
                    total_count += meta.count(*start_offset, max_steps);
                }
            }
            let count = caches
                .values()
                .map(|meta| meta.count(0, max_steps))
                .sum::<usize>();

            println!(
                "start_points: {} -> {}, count: {}, total_count: {} -> {}",
                start_points.len(),
                new_start_points.len(),
                count,
                total_count,
                total_count + count
            );
            total_count += count;
            start_points = new_start_points;
        }

        total_count
    }
}
