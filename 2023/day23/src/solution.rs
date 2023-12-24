use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;

use crate::config::Config;
use crate::config::Stage;

#[derive(Clone, Hash, PartialEq, Eq)]
struct Location {
    x: isize,
    y: isize,
}

impl fmt::Debug for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.x, self.y)
    }
}

impl Location {
    fn step(&self, step: &Location) -> Location {
        Location {
            y: self.y + step.y,
            x: self.x + step.x,
        }
    }

    fn default() -> Location {
        Location { x: -1, y: -1 }
    }
    fn right() -> Location {
        Location { x: 1, y: 0 }
    }
    fn left() -> Location {
        Location { x: -1, y: 0 }
    }
    fn up() -> Location {
        Location { x: 0, y: -1 }
    }
    fn down() -> Location {
        Location { x: 0, y: 1 }
    }
}

struct Path {
    path: HashSet<Location>,
    last: Location,
    cloned_at: Location,
    len_from_cloned: isize,
    total_len: isize,
}

impl Path {
    fn new() -> Path {
        Path {
            path: HashSet::new(),
            last: Location::default(),
            cloned_at: Location::default(),
            len_from_cloned: 0,
            total_len: 0,
        }
    }

    fn insert(&mut self, loc: &Location, step: isize) {
        self.path.insert(loc.clone());
        self.last = loc.clone();
        if self.cloned_at == Location::default() {
            self.cloned_at = loc.clone();
        }
        self.len_from_cloned += step;
        self.total_len += step;
    }

    fn split(&self) -> Path {
        Path {
            path: self.path.clone(),
            last: self.last.clone(),
            cloned_at: self.last.clone(),
            len_from_cloned: 0,
            total_len: self.total_len,
        }
    }

    fn len(&self) -> usize {
        self.total_len as usize
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Meta {
    position: Location,
    connects: HashMap<Location, isize>,
}

pub struct Solution {
    map: HashMap<Location, char>,
    start: Location,
    end: Location,
    xsize: isize,
    ysize: isize,
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let mut sol = Solution {
            map: HashMap::new(),
            start: Location { x: 0, y: 0 },
            end: Location { x: 0, y: 0 },
            xsize: 0,
            ysize: 0,
        };

        let lines = config.content.lines().collect::<Vec<&str>>();
        for (y, line) in lines.iter().enumerate() {
            for (x, c) in line.chars().enumerate() {
                let loc = Location {
                    x: x as isize,
                    y: y as isize,
                };

                let c = if config.stage == Stage::ONE {
                    c
                } else {
                    match c {
                        '#' => '#',
                        '>' => '.',
                        '<' => '.',
                        'v' => '.',
                        '^' => '.',
                        '.' => '.',
                        _ => panic!("unsupported char {}", c),
                    }
                };

                sol.map.insert(loc, c);
            }
        }

        sol.xsize = lines[0].len() as isize;
        sol.ysize = lines.len() as isize;

        sol.start = Location {
            y: 0,
            x: lines[0].chars().position(|c| c == '.').unwrap() as isize,
        };
        sol.end = Location {
            y: sol.ysize - 1,
            x: lines[(sol.ysize - 1) as usize]
                .chars()
                .position(|c| c == '.')
                .unwrap() as isize,
        };

        sol
    }

    fn next_step(&self, loc: &Location, step: &Location) -> Option<Location> {
        let current_ground = self.map[&loc];
        if current_ground != '.' {
            let step_matches = match current_ground {
                '>' => step == &Location::right(),
                '<' => step == &Location::left(),
                '^' => step == &Location::up(),
                'v' => step == &Location::down(),
                _ => panic!("unsupported current ground {} at {:?}", current_ground, loc),
            };

            if !step_matches {
                return None;
            }
        }

        let new_loc = loc.step(step);
        match self.map.get(&new_loc) {
            None => None,
            Some(c) => match c {
                '#' => None,
                _ => Some(new_loc),
            },
        }
    }
    fn find_path(&self) -> usize {
        let mut max_path = 0;

        let mut start_path = Path::new();
        start_path.insert(&self.start, 1);

        let mut start_locations: Vec<Path> = vec![start_path];

        let all_directions = [
            Location::right(),
            Location::left(),
            Location::up(),
            Location::down(),
        ];

        while start_locations.len() != 0 {
            let mut new_locations: Vec<Path> = vec![];

            for mut path in start_locations.into_iter() {
                if path.last == self.end {
                    let path_len = path.len();
                    if path_len > max_path {
                        max_path = path_len;
                    }
                    continue;
                }

                let possible_steps = all_directions
                    .iter()
                    .filter_map(|d| {
                        if let Some(next_step) = self.next_step(&path.last, d) {
                            if path.path.contains(&next_step) {
                                return None;
                            }
                            return Some(next_step);
                        }
                        None
                    })
                    .collect::<Vec<Location>>();

                if possible_steps.len() == 0 {
                    continue;
                }

                if possible_steps.len() == 1 {
                    let next_step = &possible_steps[0];
                    path.insert(next_step, 1);
                    new_locations.push(path);
                    continue;
                }

                for next_step in possible_steps.iter() {
                    let mut new_path = path.split();

                    new_path.insert(next_step, 1);
                    new_locations.push(new_path);
                }
            }

            start_locations = new_locations;
        }
        max_path - 1
    }

    fn create_graph(&self) -> HashMap<Location, Meta> {
        let mut crossings: HashMap<Location, Meta> = HashMap::new();

        let mut start_path = Path::new();
        start_path.insert(&self.start, 1);

        crossings.insert(
            self.start.clone(),
            Meta {
                position: self.start.clone(),
                connects: HashMap::new(),
            },
        );
        crossings.insert(
            self.end.clone(),
            Meta {
                position: self.end.clone(),
                connects: HashMap::new(),
            },
        );

        let mut start_locations: Vec<Path> = vec![start_path];

        let all_directions = [
            Location::right(),
            Location::left(),
            Location::up(),
            Location::down(),
        ];

        while start_locations.len() != 0 {
            let mut new_locations: Vec<Path> = vec![];

            for mut path in start_locations.into_iter() {
                if path.last == self.end {
                    crossings.entry(path.cloned_at.clone()).and_modify(|e| {
                        e.connects.insert(path.last.clone(), path.len_from_cloned);
                    });
                    crossings.entry(path.last.clone()).and_modify(|e| {
                        e.connects
                            .insert(path.cloned_at.clone(), path.len_from_cloned);
                    });
                }
                let possible_steps = all_directions
                    .iter()
                    .filter_map(|d| {
                        if let Some(next_step) = self.next_step(&path.last, d) {
                            if path.path.contains(&next_step) {
                                return None;
                            }
                            return Some(next_step);
                        }
                        None
                    })
                    .collect::<Vec<Location>>();

                if possible_steps.len() == 0 {
                    continue;
                }

                if possible_steps.len() == 1 {
                    let next_step = &possible_steps[0];
                    path.insert(next_step, 1);
                    new_locations.push(path);
                    continue;
                }

                if !crossings.contains_key(&path.last) {
                    crossings.insert(
                        path.last.clone(),
                        Meta {
                            position: path.last.clone(),
                            connects: HashMap::from([(
                                path.cloned_at.clone(),
                                path.len_from_cloned,
                            )]),
                        },
                    );
                    crossings.entry(path.cloned_at.clone()).and_modify(|e| {
                        e.connects.insert(path.last.clone(), path.len_from_cloned);
                    });

                    for next_step in possible_steps.iter() {
                        let mut new_path: Path = path.split();
                        new_path.insert(next_step, 1);
                        new_locations.push(new_path);
                    }
                    continue;
                }

                let meta = crossings.get_mut(&path.last).unwrap();
                if !meta.connects.contains_key(&path.cloned_at) {
                    meta.connects
                        .insert(path.cloned_at.clone(), path.len_from_cloned);
                    crossings.entry(path.cloned_at.clone()).and_modify(|e| {
                        e.connects.insert(path.last.clone(), path.len_from_cloned);
                    });

                    for next_step in possible_steps.iter() {
                        let mut new_path: Path = path.split();
                        new_path.insert(next_step, 1);
                        new_locations.push(new_path);
                    }
                }
            }

            start_locations = new_locations;
        }

        // println!("crossings:");
        // for meta in crossings.values() {
        //     println!("  {:?}: {:?}", meta.position, meta.connects);
        // }

        crossings
    }

    pub fn stage1(&self) -> usize {
        self.find_path()
    }

    pub fn stage2(&self) -> usize {
        let meta_graph = self.create_graph();

        let mut max_path = 0;

        let mut start_path = Path::new();
        start_path.insert(&self.start, 0);

        let mut start_locations: Vec<Path> = vec![start_path];

        while start_locations.len() != 0 {
            let mut new_locations: Vec<Path> = vec![];

            for mut path in start_locations.into_iter() {
                if path.last == self.end {
                    let path_len = path.len();
                    if path_len > max_path {
                        max_path = path_len;
                    }
                    continue;
                }

                let connects = &meta_graph
                    .get(&path.last)
                    .unwrap()
                    .connects
                    .iter()
                    .filter(|(loc, _dist)| loc != &&path.last && !path.path.contains(loc))
                    .collect::<Vec<_>>();

                if connects.len() == 1 {
                    let (next_step, &dist) = connects[0];
                    path.insert(&next_step, dist);
                    new_locations.push(path);
                    continue;
                }

                for (next_step, &dist) in connects.iter() {
                    let mut new_path = path.split();

                    new_path.insert(next_step, dist);
                    new_locations.push(new_path);
                }
            }

            start_locations = new_locations;
        }
        max_path - 1
    }
}
