use std::collections::HashMap;
use std::env;
use std::error::Error;
use std::fs;
use std::process;

#[derive(Debug, PartialEq, Clone)]
enum Stage {
    ONE,
    TWO,
}

impl Stage {
    fn parse(arg: &str) -> Stage {
        let stage_int = arg.parse::<i32>().unwrap();
        if stage_int == 1 {
            return Stage::ONE;
        } else if stage_int == 2 {
            return Stage::TWO;
        } else {
            panic!("Invalid stage {stage_int}");
        }

    }
}

#[derive(Debug)]
struct Config {
    stage: Stage,
    content: String,
}

impl Config {
    fn build(args: &[String]) -> Result<Config, Box<dyn Error>> {
        if args.len() < 2 {
            return Err("not enough arguments, input file and stage are required".into());
        }

        let mut content: String = "".to_string();

        if args.len() == 3 {
            let file_path = args[2].clone();

            content = fs::read_to_string(&file_path).map_err(|err| {
                format!("could not read file \"{file_path}\": {err}")
            })?;
        }

        Ok(Config{
            stage: Stage::parse(&args[1]),
            content: content,
        })
    }
}

#[derive(Debug, Clone)]
struct Number {
    n: u32,
    row: i32,
    start_pos: i32,
    end_pos: i32,
}

impl Number {
    fn new(line: &str, row: i32, start_pos: i32, end_pos: i32) -> Number {
        let s = &line[(start_pos as usize)..(end_pos as usize)];
        let n = s.parse::<u32>().unwrap();

        Number {
            n: n,
            row: row,
            start_pos: start_pos,
            end_pos: end_pos,
        }
    }
}

#[derive(Debug)]
struct Gear {
    pos_y: usize,
    pos_x: usize,

    adj: Vec<Number>,
}

impl PartialEq<Gear> for Gear {
    fn eq(&self, other: &Gear) -> bool {
        self.pos_y == other.pos_y && self.pos_x == other.pos_x
    }
}
impl Eq for Gear {}

impl std::hash::Hash for Gear {
    fn hash<H>(&self, hasher: &mut H)
    where
        H: std::hash::Hasher,
    {
        (self.pos_y, self.pos_x).hash(hasher);
    }
}

impl Gear {
    fn new(pos_y: usize, pos_x: usize) -> Gear {
        Gear {
            pos_y: pos_y,
            pos_x: pos_x,
            adj: vec![],
        }
    }
}

struct Map {
    map: Vec<Vec<char>>,
    feed_row: i32,
    numbers: Vec<Number>,
    gears: HashMap<(usize, usize), Gear>,
}

impl Map {
    fn new() -> Map {
        Map {
            map: vec![],
            feed_row: 0,
            numbers: vec![],
            gears: HashMap::new(),
        }
    }

    fn feed(&mut self, line: &str) {
        let mut start_idx: i32 = -1;

        self.map.push(vec!['.'; line.len()]);

        for (i, c) in line.chars().enumerate() {
            self.map[self.feed_row as usize][i] = c;

            if c == '*' {
                let g = Gear::new(self.feed_row as usize, i);
                self.gears.insert((g.pos_y, g.pos_x), g);
            }

            if c.is_digit(10) {
                if start_idx == -1 {
                    start_idx = i as i32;
                }
            }

            if i == line.len() - 1 || !c.is_digit(10) {
                let end_idx = if i == line.len() - 1 && c.is_digit(10) {
                    i + 1
                } else {
                    i
                } as i32;

                if start_idx != -1 {
                    let n = Number::new(line, self.feed_row, start_idx, end_idx);
                    self.numbers.push(n);

                    start_idx = -1;
                }
            }
        }

        self.feed_row += 1;
    }


    fn number_has_gear(&self, n: &Number) -> Option<(usize, usize)> {
        let check = |y, x| {
            if y < 0 || x < 0 || y >= self.map.len() as i32 || x >= self.map.last().unwrap().len() as i32 {
                return false;
            }

            let c = self.map[y as usize][x as usize];
            c == '*'
        };

        for y in (n.row-1)..=(n.row+1) {
            if check(y, n.start_pos - 1) {
                return Some((y as usize, (n.start_pos - 1) as usize));
            }
            if check(y, n.end_pos) {
                return Some((y as usize, n.end_pos as usize));
            }
        }
        for x in (n.start_pos-1)..=(n.end_pos) {
            if check(n.row - 1, x) {
                return Some(((n.row - 1) as usize, x as usize));
            }
            if check(n.row + 1, x) {
                return Some(((n.row + 1) as usize, x as usize));
            }
        }

        None
    }

    fn find_gears(&mut self) {
        for n in self.numbers.iter() {
            if let Some((y, x)) = self.number_has_gear(n) {
                let g = self.gears.get_mut(&(y, x)).unwrap();
                g.adj.push(n.to_owned());
            }
        }
    }

    fn number_is_valid(&self, n: &Number) -> bool {
        let check = |y, x| {
            if y < 0 || x < 0 || y >= self.map.len() as i32 || x >= self.map.last().unwrap().len() as i32 {
                return false;
            }

            let c = self.map[y as usize][x as usize];
            c != '.' && !c.is_digit(10)
        };

        for y in (n.row-1)..=(n.row+1) {
            if check(y, n.start_pos - 1) || check(y, n.end_pos) {
                return true;
            }
        }
        for x in (n.start_pos-1)..=(n.end_pos) {
            if check(n.row-1, x) || check(n.row+1, x) {
                return true;
            }
        }

        false
    }
}

struct Solution {
    map: Map,
}

impl Solution {
    fn build(config: &Config) -> Solution {
        let mut sol = Solution {
            map: Map::new(),
        };

        for line in config.content.lines() {
            sol.map.feed(line);
        }

        sol
    }

    fn find_numbers(&self) -> u32 {
        let mut sum = 0;
        for n in self.map.numbers.iter() {
            let valid = self.map.number_is_valid(n);
            if valid {
                sum += n.n;
            }
        }

        sum
    }

    fn find_gears(&mut self) -> u32 {
        let mut sum = 0;

        self.map.find_gears();

        for (_, g) in self.map.gears.iter() {
            if g.adj.len() == 2 {
                let mut mult = 1;
                for n in g.adj.iter() {
                    mult *= n.n;
                }

                sum += mult;
            }
        }

        sum
    }
}

fn run(config: Config) -> Result<(), Box<dyn Error>> {
    if config.stage == Stage::ONE {
        let sol = Solution::build(&config);
        let ret = sol.find_numbers();
        println!("stage: {:?}, ret: {}", config.stage, ret);
    } else {
        let mut sol = Solution::build(&config);
        let ret = sol.find_gears();
        println!("stage: {:?}, ret: {}", config.stage, ret);
    }
    Ok(())
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let config = Config::build(&args).unwrap_or_else(|err| {
        println!("Problem parsing arguments: {err}");
        process::exit(-1);
    });

    if let Err(e) = run(config) {
        println!("Application error: {e}");
        process::exit(-1);
    }
}
