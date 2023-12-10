use std::collections::HashMap;
use std::collections::HashSet;

use num_iter;
use num_iter::RangeStep;

use crate::config::Config;

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
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
        *marks.loop_vec.entry(self.start.clone()).or_insert(0) += 1;

        while starts.len() > 0 {
            let mut new_starts: Vec<Coord> = vec![];
            for sc in starts {
                let sc_next = self.next(&sc).into_iter().filter(|n| !marks.loop_vec.contains_key(n)).collect::<Vec<Coord>>();
                for sc_entry in sc_next.iter() {
                    *marks.loop_vec.entry(sc_entry.clone()).or_insert(0) += 1;
                    new_starts.push(sc_entry.clone());
                }
            }

            starts = new_starts;
        }

        marks
    }
}

struct Marks {
    loop_vec: HashMap<Coord, isize>,
    non_loop: HashMap<Coord, isize>,
}

impl Marks {
    fn new() -> Marks {
        Marks {
            loop_vec: HashMap::new(),
            non_loop: HashMap::new(),
        }
    }

    fn mark_non_loop(&mut self, map: &Map) {
        for y in 0..(map.data.len()) {
            for x in 0..(map.data[0].len()) {
                let c = Coord::new(y as isize, x as isize);
                if !self.loop_vec.contains_key(&c) {
                    self.non_loop.entry(c).or_insert(0);
                }
            }
        }
    }

    fn can_escape_iter(&self, start: &Coord, range: std::vec::IntoIter<Coord>, map: &Map, intersection: &HashSet<Pipe>) -> bool {
        let mut intersections = 0;

        for check in range {
            let pipe = map.get(&check).unwrap();
            if self.loop_vec.contains_key(&check) && intersection.contains(&pipe) {
                intersections += 1;
            }
        }
        println!("c: {start:?} {intersections}");
        return intersections % 2 == 0;
    }

    fn can_squeeze_to_escape(&self, c: &Coord, map: &Map) -> bool {
        let intersections_vertical = HashSet::from([Pipe::NE, Pipe::NW, Pipe::SW, Pipe::SE, Pipe::NS]);
        let intersections_horizontal = HashSet::from([Pipe::NE, Pipe::NW, Pipe::SW, Pipe::SE, Pipe::EW]);

        let check_iters = vec![
            num_iter::range_step::<isize>(c.y-1, -1, -1).map(|y| Coord::new(y, c.x)).collect::<Vec<Coord>>().into_iter(),
            ((c.y+1)..(map.data.len() as isize)).map(|y| Coord::new(y, c.x)).collect::<Vec<Coord>>().into_iter(),
            num_iter::range_step::<isize>(c.x-1, -1, -1).map(|x| Coord::new(c.y, x)).collect::<Vec<Coord>>().into_iter(),
            ((c.x+1)..(map.data[0].len() as isize)).map(|x| Coord::new(c.y, x)).collect::<Vec<Coord>>().into_iter(),
        ];

        let intersection_iters = vec![
            &intersections_horizontal,
            &intersections_horizontal,
            &intersections_vertical,
            &intersections_vertical,
        ];

        for (can_escape_iter, intersection) in check_iters.into_iter().zip(intersection_iters.into_iter()) {
            if !self.can_escape_iter(c, can_escape_iter, map, intersection) {
                return false;
            }
        }

        true
    }

    fn try_escape1(&mut self, map: &Map) -> usize {
        let mut enclosed = 0;
        let mut neigh_map: HashMap<Coord, isize> = HashMap::new();
        for (start, val) in self.non_loop.iter() {
            if neigh_map.contains_key(start) {
                continue;
            }

            let mut history_map: HashMap<Coord, isize> = HashMap::new();
            let mut escaped = false;

            let mut check_next: Vec<Coord> = vec![start.clone()];
            while check_next.len() > 0 {
                let mut new_check_next: Vec<Coord> = vec![];
                for start in &check_next {
                    if neigh_map.contains_key(start) || self.loop_vec.contains_key(start) {
                        continue
                    }

                    for y in (start.y-1)..=(start.y+1) {
                        for x in (start.x-1)..=(start.x+1) {
                            let c = Coord::new(y, x);
                            if !map.is_valid(&c) {
                                escaped = true;
                                continue
                            }

                            if neigh_map.contains_key(&c) {
                                continue
                            }

                            if self.non_loop.contains_key(&c) {
                                *neigh_map.entry(start.clone()).or_insert(0) += 1;
                                *history_map.entry(start.clone()).or_insert(0) += 1;

                                new_check_next.push(c);
                            }
                        }
                    }
                }

                println!("check_next: {} -> {}, neight_map: {}", check_next.len(), new_check_next.len(), neigh_map.len());
                check_next = new_check_next;
            }

            if !escaped {
                for c in history_map.keys() {
                    if !self.can_squeeze_to_escape(&c, &map) {
                        enclosed += 1;
                    }
                }

                println!("enclosed: history_map {:?}", history_map);
            }

        }

        enclosed
    }
    fn try_escape(&mut self, map: &Map) -> usize {
        println!("loop_vec: {:?}", self.loop_vec);
        let mut enclosed = 0;
        for (start, val) in self.non_loop.iter() {
            if !self.can_squeeze_to_escape(start, &map) {
                println!("c: {start:?} can not escape");
                enclosed += 1;
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
        let mut marks = self.map.make_loop();
        marks.mark_non_loop(&self.map);
        marks.try_escape(&self.map)
    }
}
