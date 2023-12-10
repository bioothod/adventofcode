use std::collections::HashSet;
use std::fmt;

use crate::config::Config;

#[derive(PartialEq, Eq, Hash, Clone)]
enum Pipe {
    NS,
    EW,
    NE,
    NW,
    SW,
    SE,
    GROUND,
    START,
}

impl Pipe {
    fn print(&self) -> char {
        match self {
            Pipe::NS => char::from_u32(0x2502),
            Pipe::EW => char::from_u32(0x2501),
            Pipe::NE => char::from_u32(0x2514),
            Pipe::NW => char::from_u32(0x2518),
            Pipe::SW => char::from_u32(0x2510),
            Pipe::SE => char::from_u32(0x250c),
            Pipe::GROUND => Some('.'),
            Pipe::START => Some('S'),
        }.unwrap()
    }
}

impl fmt::Debug for Pipe {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.print())
    }
}


#[derive(PartialEq, Eq, Hash, Debug, Clone)]
struct Coord {
    y: isize,
    x: isize,
}

impl Coord {
    fn new(y: isize, x: isize) -> Coord {
        Coord {y: y, x: x}
    }
    fn default() -> Coord {
        Coord {y: -1, x: -1}
    }

    fn connects(&self, pipe: &Pipe) -> Vec<Coord> {
        match pipe {
            Pipe::NS => vec![Coord::new(self.y-1, self.x), Coord::new(self.y+1, self.x)],
            Pipe::EW => vec![Coord::new(self.y, self.x+1), Coord::new(self.y, self.x-1)],
            Pipe::NE => vec![Coord::new(self.y-1, self.x), Coord::new(self.y, self.x+1)],
            Pipe::NW => vec![Coord::new(self.y-1, self.x), Coord::new(self.y, self.x-1)],
            Pipe::SW => vec![Coord::new(self.y+1, self.x), Coord::new(self.y, self.x-1)],
            Pipe::SE => vec![Coord::new(self.y+1, self.x), Coord::new(self.y, self.x+1)],
            _ => vec![Coord::default(); 2],
        }
    }

    fn is_default(&self) -> bool {
        self.x < 0 && self.y < 0
    }
}

struct Map {
    data: Vec<Vec<Pipe>>,
    orig: HashSet<Coord>,
    start: Coord,
}

impl Map {
    fn parse_line(line: &str) -> Vec<Pipe> {
        line.chars().map(|c| {
            match c {
                '|' => Pipe::NS,
                '-' => Pipe::EW,
                'L' => Pipe::NE,
                'J' => Pipe::NW,
                '7' => Pipe::SW,
                'F' => Pipe::SE,
                '.' => Pipe::GROUND,
                'S' => Pipe::START,
                _ => panic!("unsupported map symbol {c}"),
            }
        }).collect::<Vec<Pipe>>()
    }

    fn is_valid(&self, c: &Coord) -> bool {
        c.y >= 0 && c.y < (self.data.len() as isize) && c.x >= 0 && c.x < (self.data[0].len() as isize)
    }

    fn get(&self, c: &Coord) -> Option<Pipe> {
        if self.is_valid(c) {
            return Some(self.data[c.y as usize][c.x as usize].clone());
        }
        None
    }

    fn find_start(&mut self) {
        for (y, row) in self.data.iter().enumerate() {
            for (x, column) in row.iter().enumerate() {
                if column == &Pipe::START {
                    self.start = Coord::new(y as isize, x as isize);
                    return
                }
            }
        }

        panic!("could not find starting point");
    }

    fn next(&self, start: &Coord) -> Vec<Coord> {
        let Some(pipe) = self.get(start) else { panic!("start {:?} is outside the map", start); };

        if pipe == Pipe::START {
            let mut start_connects: Vec<Coord> = vec![];
            for y in (start.y-1)..=(start.y+1) {
                for x in (start.x-1)..=(start.x+1) {
                    let c = Coord::new(y, x);
                    if let Some(p) = self.get(&c) {
                        if p == Pipe::GROUND {
                            continue
                        }

                        let connects: Vec<Coord> = c.connects(&p)
                            .into_iter()
                            .filter(|pc| self.is_valid(pc) && !pc.is_default() && pc == start)
                            .collect();
                        if connects.len() > 0 {
                            start_connects.push(c);
                        }
                    }
                }
            }

            return start_connects;
        }

        let connects: Vec<Coord> = start.connects(&pipe).into_iter().map(|c| {
            if self.is_valid(&c) && !c.is_default() {
                c
            } else {
                Coord::default()
            }
        }).collect();
        connects
    }

    fn make_loop(&self) -> Marks {
        let mut marks = Marks::new();

        let mut starts = self.next(&self.start);
        marks.loop_vec.insert(self.start.clone());

        while starts.len() > 0 {
            let mut new_starts: Vec<Coord> = vec![];
            for sc in starts {
                let sc_next = self.next(&sc).into_iter().filter(|n| !marks.loop_vec.contains(n)).collect::<Vec<Coord>>();
                for sc_entry in sc_next.iter() {
                    marks.loop_vec.insert(sc_entry.clone());
                    new_starts.push(sc_entry.clone());
                }
            }

            starts = new_starts;
        }

        marks
    }

    fn expand_map(&self) -> Map {
        let mut new_map = Map {
            data: vec![],
            start: Coord::new(self.start.y*2, self.start.x*2),
            orig: HashSet::new(),
        };

        for (ridx, row) in self.data.iter().enumerate() {
            let mut new_row0: Vec<Pipe> = vec![];
            for (cidx, column) in row.iter().enumerate() {
                let ext = match column {
                    Pipe::GROUND => vec![Pipe::GROUND, Pipe::GROUND],
                    Pipe::NS => vec![Pipe::NS, Pipe::GROUND],
                    Pipe::EW => vec![Pipe::EW, Pipe::EW],
                    Pipe::NE => vec![Pipe::NE, Pipe::EW],
                    Pipe::NW => vec![Pipe::NW, Pipe::GROUND],
                    Pipe::SW => vec![Pipe::SW, Pipe::GROUND],
                    Pipe::SE => vec![Pipe::SE, Pipe::EW],
                    Pipe::START => vec![Pipe::START, Pipe::START],
                };

                let c = Coord::new(2*ridx as isize, 2*cidx as isize);
                new_map.orig.insert(c);

                if new_row0.len() > 0 {
                    if let Some(last) = new_row0.last_mut() {
                        if last == &Pipe::START {
                            *last = match column {
                                Pipe::GROUND => Pipe::GROUND,
                                Pipe::NS => Pipe::GROUND,
                                Pipe::EW => Pipe::EW,
                                Pipe::NE => Pipe::GROUND,
                                Pipe::NW => Pipe::EW,
                                Pipe::SW => Pipe::EW,
                                Pipe::SE => Pipe::GROUND,
                                _ => panic!("can not have multiple starts in the row"),
                            };
                        }
                    }
                }

                new_row0.extend(ext);
            }


            let mut new_row1: Vec<Pipe> = vec![];
            for (cidx, column) in new_row0.iter().enumerate() {
                let ext = match column {
                    Pipe::GROUND => Pipe::GROUND,
                    Pipe::NS => Pipe::NS,
                    Pipe::EW => Pipe::GROUND,
                    Pipe::NE => Pipe::GROUND,
                    Pipe::NW => Pipe::GROUND,
                    Pipe::SW => Pipe::NS,
                    Pipe::SE => Pipe::NS,
                    Pipe::START => Pipe::START,
                };

                if new_map.data.len() != 0 {
                    let ridx = new_map.data.len() - 1;
                    let prev = new_map.data.get_mut(ridx).unwrap().get_mut(cidx).unwrap();
                    if prev == &Pipe::START {
                        *prev = match column {
                            Pipe::GROUND => Pipe::GROUND,
                            Pipe::NS => Pipe::NS,
                            Pipe::EW => Pipe::GROUND,
                            Pipe::NE => Pipe::NS,
                            Pipe::NW => Pipe::NS,
                            Pipe::SW => Pipe::GROUND,
                            Pipe::SE => Pipe::GROUND,
                            _ => panic!("can not have multiple starts in the column"),
                        };
                    }
                }
                new_row1.push(ext);
            }

            new_map.data.push(new_row0);
            new_map.data.push(new_row1);
        }
        new_map
    }

    fn print(&self, marks: &Marks) {
        for y in 0..self.data.len() {
            let mut row: Vec<char> = vec![];
            for x in 0..self.data[0].len() {
                let coord = Coord::new(y as isize, x as isize);
                if marks.loop_vec.contains(&coord) {
                    row.push(self.data[y][x].print());
                } else {
                    row.push('.');
                }
            }
            row.push('\n');
            let s = String::from_iter(row);
            println!("{}", s);
        }
    }
}

struct Marks {
    loop_vec: HashSet<Coord>,
    non_loop: HashSet<Coord>,
}

impl Marks {
    fn new() -> Marks {
        Marks {
            loop_vec: HashSet::new(),
            non_loop: HashSet::new(),
        }
    }

    fn mark_non_loop(&mut self, map: &Map) {
        for y in 0..(map.data.len()) {
            for x in 0..(map.data[0].len()) {
                let c = Coord::new(y as isize, x as isize);
                if !self.loop_vec.contains(&c) {
                    self.non_loop.insert(c);
                }
            }
        }
    }

    fn try_escape(&mut self, map: &Map) -> usize {
        let mut enclosed = 0;
        let mut neigh_map: HashSet<Coord> = HashSet::new();
        for start in self.non_loop.iter() {
            if neigh_map.contains(start) {
                continue;
            }

            let mut history_map: HashSet<Coord> = HashSet::new();
            let mut escaped = false;

            let mut check_next: Vec<Coord> = vec![start.clone()];
            while check_next.len() > 0 {
                let mut new_check_next: Vec<Coord> = vec![];
                for start in &check_next {
                    if neigh_map.contains(start) || self.loop_vec.contains(start) {
                        continue
                    }

                    neigh_map.insert(start.clone());

                    for y in (start.y-1)..=(start.y+1) {
                        for x in (start.x-1)..=(start.x+1) {
                            let c = Coord::new(y, x);
                            if !map.is_valid(&c) {
                                escaped = true;
                                continue
                            }

                            if neigh_map.contains(&c) {
                                continue
                            }

                            if self.non_loop.contains(&c) {
                                history_map.insert(start.clone());

                                new_check_next.push(c);
                            }
                        }
                    }
                }

                check_next = new_check_next;
            }

            if !escaped {
                for c in history_map.iter() {
                    if map.orig.contains(c) {
                        enclosed += 1;
                    }
                }
            }

        }

        enclosed
    }
}

pub struct Solution {
    map: Map,
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let mut sol = Solution {
            map: Map {
                data: config.content.lines().map(|line| Map::parse_line(line)).collect::<Vec<Vec<Pipe>>>(),
                start: Coord::default(),
                orig: HashSet::new(),
            },
        };

        sol.map.find_start();

        sol
    }

    pub fn stage1(&self) -> usize {
        let marks = self.map.make_loop();
        marks.loop_vec.len() / 2
    }

    pub fn stage2(&self) -> usize {
        let new_map = self.map.expand_map();
        let mut marks = new_map.make_loop();
        marks.mark_non_loop(&new_map);

        marks.try_escape(&new_map)
    }
}
