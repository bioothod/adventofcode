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

struct Conversion<T: AsRef<str>> {
    word: T,
    output: u32,
}

const CONVERSIONS: [Conversion<&'static str>; 9] = [
    Conversion{word: "one", output: 1},
    Conversion{word: "two", output: 2},
    Conversion{word: "three", output: 3},
    Conversion{word: "four", output: 4},
    Conversion{word: "five", output: 5},
    Conversion{word: "six", output: 6},
    Conversion{word: "seven", output: 7},
    Conversion{word: "eight", output: 8},
    Conversion{word: "nine", output: 9},
];

fn parse_single_digit(s: &str) -> Option<u32> {
    for c in CONVERSIONS {
        if s.contains(&c.word) {
            return Some(c.output);
        }
    }

    None
}

fn parse_letters(letters: &Vec<char>) -> Option<u32> {
    let s = String::from_iter(letters);
    parse_single_digit(&s)
}

fn prefix_match(sub: &str) -> bool {
    for c in CONVERSIONS {
        if c.word.starts_with(sub) {
            return true;
        }
    }

    false
}

fn clear_letters_check_prefix(letters: &Vec<char>) -> Vec<char> {
    let mut longest_len = 0;

    for idx in 0..(letters.len()-1) {
        let end_idx = letters.len() - 1 - idx;
        let sub = String::from_iter(letters[end_idx..].iter());
        if !prefix_match(sub.as_str()) {
            longest_len = idx;
            break;
        }
    }

    letters[letters.len() - longest_len..].to_vec()
}

fn calibrate(config: &Config) -> u32 {
    let mut sum = 0;
    for line in config.content.lines() {
        let mut digits: Vec<u32> = vec![];

        for c in line.chars() {
            if c.is_digit(10) {
                let digit = c.to_digit(10).unwrap();
                digits.push(digit);
            }
        }

        sum += digits.first().unwrap() * 10 + digits.last().unwrap();
    }

    sum
}


fn calibrate_with_digits(config: &Config) -> u32 {
    let mut sum = 0;
    for line in config.content.lines() {
        let mut digits: Vec<u32> = vec![];
        let mut letters_after_digit: Vec<char> = vec![];

        for c in line.chars() {
            if c.is_digit(10) {
                let digit = c.to_digit(10).unwrap();
                digits.push(digit);
                letters_after_digit.clear();
            } else {
                letters_after_digit.push(c);
                if let Some(digit) = parse_letters(&letters_after_digit) {
                    digits.push(digit);
                    letters_after_digit = clear_letters_check_prefix(&letters_after_digit);
                }
            }
        }

        sum += digits.first().unwrap() * 10 + digits.last().unwrap();
    }

    sum
}

fn run(config: Config) -> Result<(), Box<dyn Error>> {
    if config.stage == Stage::ONE {
        let ret = calibrate(&config);
        println!("stage: {:?}, ret: {}", config.stage, ret);
    } else {
        let ret = calibrate_with_digits(&config);
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
