use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::fmt;

use crate::config::Config;

#[derive(Clone, Hash, PartialEq, Eq)]
struct Position {
    y: isize,
    x: isize,
}

impl Position {
    fn new(y: isize, x: isize) -> Position {
        Position { y, x }
    }

    fn dist(&self, other: &Position) -> usize {
        ((self.y - other.y).abs() + (self.x - other.x).abs()) as usize
    }

    fn rev(&self) -> Position {
        Position {
            y: -self.y,
            x: -self.x,
        }
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.y, self.x)
    }
}

fn dir_to_str(dir: &Position) -> String {
    match dir {
        Position { y: 0, x: 1 } => "R".to_string(),
        Position { y: 0, x: -1 } => "L".to_string(),
        Position { y: 1, x: 0 } => "D".to_string(),
        Position { y: -1, x: 0 } => "U".to_string(),
        Position { y: 0, x: 0 } => ".".to_string(),
        _ => format!("{:?}", dir).to_string(),
    }
}

#[derive(Clone, Eq)]
struct Move {
    pos: Position,
    dir: Position,
    step: usize,

    weight: usize,
    dist: usize,
}
impl fmt::Debug for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:?} {}{} w{}, d:{}",
            self.pos,
            dir_to_str(&self.dir),
            self.step,
            self.weight,
            self.dist
        )
    }
}
impl std::hash::Hash for Move {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.pos.hash(state);
        self.dir.hash(state);
        self.step.hash(state);
    }
}
impl PartialOrd for Move {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Move {
    fn cmp(&self, other: &Self) -> Ordering {
        // max heap
        //self.dist.cmp(&other.dist)

        // min heap
        other.dist.cmp(&self.dist)
    }
}

impl PartialEq for Move {
    fn eq(&self, other: &Self) -> bool {
        // compare keys my src/dst positions, where dst position is number of steps and direction
        // do not use metadata like accumulated dist and weight
        self.pos == other.pos && self.dir == other.dir && self.step == other.step
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Block {
    weight: usize,
}

impl Block {
    fn new(weight: usize) -> Block {
        Block { weight }
    }
}

pub struct Solution {
    map: HashMap<Position, Block>,
    ysize: isize,
    xsize: isize,
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let lines = config.content.lines().collect::<Vec<&str>>();
        let sol = Solution {
            map: lines.iter().enumerate().fold(
                HashMap::new(),
                |acc: HashMap<Position, Block>, (y, line)| {
                    line.chars().enumerate().fold(acc, |mut acc, (x, c)| {
                        acc.insert(
                            Position::new(y as isize, x as isize),
                            Block::new(c.to_digit(10).unwrap() as usize),
                        );
                        acc
                    })
                },
            ),
            ysize: lines.len() as isize,
            xsize: lines[0].len() as isize,
        };

        sol
    }

    fn print(&self, end: &Move, start: &Position, steps_made: &HashMap<Move, Move>) {
        let mut path = vec![vec!['.'; self.xsize as usize]; self.ysize as usize];
        let mut path_weight = path.clone();
        for (k, v) in self.map.iter() {
            path_weight[k.y as usize][k.x as usize] =
                char::from_digit(v.weight as u32, 10).unwrap();
        }

        let mut mv = end;
        let mut weights: Vec<usize> = vec![];
        while &mv.pos != start {
            let block = self.map.get(&mv.pos).unwrap();
            weights.push(block.weight);
            path[mv.pos.y as usize][mv.pos.x as usize] =
                dir_to_str(&mv.dir).chars().nth(0).unwrap();

            if let Some(from) = steps_made.get(&mv) {
                mv = from;
            } else {
                break;
            }
        }

        for (p, w) in path.iter().zip(path_weight.iter()) {
            let ps = String::from_iter(p.iter());
            let ws = String::from_iter(w.iter());
            println!("{}   {}", ws, ps);
        }

        println!(
            "weights: end: {:?} sum: {}",
            end,
            weights.iter().sum::<usize>()
        );
    }

    fn find_path(&self, min_steps: isize, max_steps: isize) -> usize {
        let default_dist = 10000;

        let start = Position::new(0, 0);
        let end = Position::new(self.ysize - 1, self.xsize - 1);
        let mut final_move = Move {
            pos: end.clone(),
            dir: Position::new(0, 0),
            step: 0,
            weight: self.map.get(&end).unwrap().weight,
            dist: default_dist,
        };
        let mut final_move_dist = default_dist;

        let mut steps_made: HashMap<Move, Move> = HashMap::new();
        let mut open_list: BinaryHeap<Move> = BinaryHeap::new();
        open_list.push(Move {
            pos: start.clone(),
            dir: Position::new(0, 0),
            step: 0,
            weight: self.map.get(&start).unwrap().weight,
            dist: 0,
        });

        let directions = [
            Position::new(0, 1),
            Position::new(0, -1),
            Position::new(1, 0),
            Position::new(-1, 0),
        ];

        while open_list.len() > 0 {
            let mv = open_list.pop().unwrap();
            //println!("start: mv: {:?}, open_list: {}", mv, open_list.len());

            let mut allowed_directions: Vec<Position> = directions
                .iter()
                .filter(|&d| d != &mv.dir && d.rev() != mv.dir)
                .map(|d| d.to_owned())
                .collect();

            let mut prev_moves: HashMap<Position, Move> = allowed_directions
                .iter()
                .map(|d| (d.clone(), mv.clone()))
                .collect();

            for i in 1..=max_steps {
                let mut dst = allowed_directions
                    .iter()
                    .filter_map(|d| {
                        let x = mv.pos.x + d.x * i;
                        let y = mv.pos.y + d.y * i;

                        if x < 0 || x >= self.xsize || y < 0 || y >= self.ysize {
                            return None;
                        }

                        let new_pos = Position::new(y, x);
                        let mut tmp_mv = Move {
                            weight: self.map.get(&new_pos).unwrap().weight,
                            pos: new_pos,
                            dir: d.clone(),
                            step: i as usize,
                            dist: default_dist,
                        };

                        if let Some((k, _v)) = steps_made.get_key_value(&tmp_mv) {
                            tmp_mv.dist = k.dist;
                        };

                        Some(tmp_mv)
                    })
                    .collect::<Vec<Move>>();

                if dst.len() == 0 {
                    break;
                }

                for new_mv in dst.iter_mut() {
                    if steps_made.contains_key(new_mv) {
                        continue;
                    }

                    let prev_mv = prev_moves.get_mut(&new_mv.dir).unwrap();
                    let alt = prev_mv.dist + new_mv.weight;

                    // println!(
                    //     "src: {:?}, {}: {:?} -> {:?}, alt: {} vs {} {}",
                    //     mv,
                    //     i,
                    //     prev_mv,
                    //     new_mv,
                    //     alt,
                    //     new_mv.dist,
                    //     if alt < new_mv.dist { "insert" } else { "" }
                    // );

                    if alt < new_mv.dist {
                        new_mv.dist = alt;

                        steps_made.insert(new_mv.clone(), prev_mv.clone());
                        if i >= min_steps {
                            open_list.push(new_mv.clone());
                        }

                        if new_mv.pos == end && new_mv.dist < final_move_dist {
                            final_move = new_mv.clone();
                            final_move_dist = new_mv.dist;
                        }
                    } else {
                        allowed_directions = allowed_directions
                            .into_iter()
                            .filter(|d| d != &new_mv.dir)
                            .collect();
                    }

                    *prev_mv = new_mv.clone();
                }

                if allowed_directions.len() == 0 {
                    break;
                }
            }
        }

        //self.print(&final_move, &start, &steps_made);

        final_move.dist
    }

    pub fn stage1(&self) -> usize {
        self.find_path(1, 3)
    }

    pub fn stage2(&self) -> usize {
        self.find_path(4, 10)
    }
}
