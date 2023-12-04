use std::collections::HashMap;
use std::cmp;
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

#[derive(Debug)]
struct Card {
    _id: u32,
    winning: Vec<u32>,
    own: Vec<u32>,
}

impl Card {
    fn parse(line: &str) -> Card {
        let id_parts = line.split(":").collect::<Vec<_>>();
        let id = id_parts[0].split(" ").last().unwrap().trim().parse::<u32>().unwrap();
        let card_parts = id_parts[1].split("|").collect::<Vec<&str>>();

        let convert_to_numbers = |parts: &str| {
            parts.trim().split_whitespace().map(|n| {
                n.trim().parse::<u32>().unwrap()
            }).collect::<Vec<u32>>()
        };

        let winning_numbers = convert_to_numbers(card_parts[0]);
        let own_numbers = convert_to_numbers(card_parts[1]);

        Card {
            _id: id,
            winning: winning_numbers,
            own: own_numbers,
        }
    }

    fn order(&self) -> u32 {
        let mut mix = HashMap::<u32, u32>::new();
        for n in self.winning.iter() {
            mix.insert(*n, 0);
        }

        for n in self.own.iter() {
            mix.get_mut(n).and_then(|v| Some(*v += 1));
        }

        mix.values().sum::<u32>()
    }

    fn points(&self) -> u32 {
        let order = self.order();
        if order == 0 {
            0
        } else {
            2_u32.pow(order - 1)
        }
    }
}

struct Solution {
    cards: Vec<Card>,
}

impl Solution {
    fn build(config: &Config) -> Solution {
        let cards = config.content.lines().map(|line| Card::parse(line)).collect::<Vec<Card>>();

        Solution {
            cards: cards,
        }
    }

    fn points(&self) -> u32 {
        let mut points = 0;
        for c in self.cards.iter() {
            points += c.points();
        }

        points
    }

    fn scratchcards(&self) -> u32 {
        let mut copies: Vec<u32> = vec![1; self.cards.len()];

        for (i, c) in self.cards.iter().enumerate() {
            let additional_copies = c.order() as usize;
            let max_idx = cmp::min(i+additional_copies+1, self.cards.len());
            for idx in (i+1)..max_idx {
                copies[idx] += copies[i];
            }
        }

        copies.into_iter().sum()
    }
}

fn run(config: Config) -> Result<(), Box<dyn Error>> {
    if config.stage == Stage::ONE {
        let sol = Solution::build(&config);
        let ret = sol.points();
        println!("stage: {:?}, ret: {}", config.stage, ret);
    } else {
        let sol = Solution::build(&config);
        let ret = sol.scratchcards();
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
