use std::cmp;
use std::env;
use std::error::Error;
use std::fmt;
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

struct Subset {
    r: u32,
    g: u32,
    b: u32,
}

impl fmt::Debug for Subset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("subset")
            .field("r", &self.r)
            .field("g", &self.g)
            .field("b", &self.b)
            .finish()
    }
}

impl Subset {
    fn empty() -> Subset {
        Subset {
            r: 0,
            g: 0,
            b: 0,
        }
    }
    fn parse(subset_line: &str) -> Subset {
        let mut subset = Subset::empty();

        for one_colour in subset_line.trim().split(",") {
            let parts = one_colour.trim().split(" ").collect::<Vec<&str>>();

            let number = parts[0].trim().parse::<u32>().unwrap();
            let colour = parts[1].trim();

            match colour {
                "red" => subset.r = number,
                "green" => subset.g = number,
                "blue" => subset.b = number,
                _ => panic!("unsupported colour {colour}"),
            }
        }

        subset
    }

    fn possible(&self, limits: &Subset) -> bool {
        self.r <= limits.r && self.g <= limits.g && self.b <= limits.b
    }

    fn power(&self) -> u32 {
        self.r * self.g * self.b
    }
}

#[derive(Debug)]
struct Game {
    id: u32,
    subsets: Vec<Subset>,
}

impl Game {
    fn build(line: &str) -> Game {
        let mut g = Game {
            id: 0,
            subsets: vec![],
        };

        let id_substr = line.split(":").collect::<Vec<&str>>();
        g.id = id_substr[0].trim().split(" ").last().unwrap().parse::<u32>().unwrap();

        for subset_line in id_substr[1].trim().split(";") {
            g.subsets.push(Subset::parse(subset_line));
        }

        g
    }

    fn possible(&self, limits: &Subset) -> bool {
        for s in self.subsets.iter() {
            if !s.possible(limits) {
                return false;
            }
        }

        true
    }

    fn min_possible(&self) -> Subset {
        let mut min_possible = Subset::empty();
        for s in self.subsets.iter() {
            min_possible.r = cmp::max(min_possible.r, s.r);
            min_possible.g = cmp::max(min_possible.g, s.g);
            min_possible.b = cmp::max(min_possible.b, s.b);
        }
        min_possible
    }
}

struct Solution {
    games: Vec<Game>,
}

impl Solution {
    fn create(config: &Config) -> Solution {
        let mut sol = Solution {
            games: vec![],
        };

        for line in config.content.lines() {
            sol.games.push(Game::build(line));
        }

        sol
    }

    fn search(&self, limits: &Subset) -> u32 {
        let mut possible_games = 0;
        for g in self.games.iter() {
            if g.possible(limits) {
                possible_games += g.id;
            }
        }

        possible_games
    }

    fn min_subset_search(&self) -> u32 {
        let mut power = 0;
        for g in self.games.iter() {
            power += g.min_possible().power();
        }
        power
    }
}

fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let sol = Solution::create(&config);
    if config.stage == Stage::ONE {
        let limits = Subset {
            r: 12,
            g: 13,
            b: 14,
        };

        let ret = sol.search(&limits);
        println!("stage: {:?}, ret: {}", config.stage, ret);
    } else {
        let ret = sol.min_subset_search();
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
