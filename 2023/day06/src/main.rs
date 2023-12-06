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
struct Race {
    time: usize,
    distance: usize,
}

impl Race {
    fn num_winning_solutions(&self) -> usize {
        let det = (self.time * self.time - 4 * (self.distance+1)) as f64;
        let det = det.sqrt().floor() as usize;
        let s0 = ((self.time - det)).div_ceil(2) as usize;
        let s1 = ((self.time + det) / 2) as usize;

        s1 - s0 + 1
    }

    fn num_winning_solutions_slow(&self) -> usize {
        let mut num = 0;
        for n in 1..(self.time-1) {
            if (self.time - n) * n > self.distance {
                num += 1;
            }
        }

        num
    }
}

struct Solution {
    races: Vec<Race>,
}

impl Solution {
    fn build(config: &Config) -> Solution {
        let mut sol = Solution {
            races: vec![],
        };

        if config.stage == Stage::ONE {
            let mut times: Option<_> = None;
            let mut distances: Option<_> = None;

            for line in config.content.lines() {
                if line.starts_with("Time:") {
                    times = Some(line.split(":").last().unwrap().trim().split_whitespace().map(|x| x.parse::<usize>().unwrap()));
                }
                if line.starts_with("Distance:") {
                    distances = Some(line.split(":").last().unwrap().trim().split_whitespace().map(|x| x.parse::<usize>().unwrap()));
                }
            }

            if times.is_none() || distances.is_none() {
                panic!("could not parse input file");
            }

            for (t, d) in times.unwrap().zip(distances.unwrap()) {
                sol.races.push(
                    Race {
                        time: t,
                        distance: d,
                    }
                )
            }
        } else {
            let mut time: Option<_> = None;
            let mut distance: Option<_> = None;

            for line in config.content.lines() {
                if line.starts_with("Time:") {
                    let ln: Vec<_> = line.split(":").last().unwrap().trim().split_whitespace().collect();
                    let ln = ln.join("");
                    time = Some(ln.parse::<usize>().unwrap());
                }
                if line.starts_with("Distance:") {
                    let ln: Vec<_> = line.split(":").last().unwrap().trim().split_whitespace().collect();
                    let ln = ln.join("");
                    distance = Some(ln.parse::<usize>().unwrap());
                }
            }

            if time.is_none() || distance.is_none() {
                panic!("could not parse input file");
            }

            sol.races.push(
                Race {
                    time: time.unwrap(),
                    distance: distance.unwrap(),
                }
            )
        }
        println!("races: {:?}", sol.races);
        sol
    }

    fn num_winning_solutions(&self) -> usize {
        let mut total = 1;
        for r in self.races.iter() {
            total *= r.num_winning_solutions();
        }

        total
    }
}

fn run(config: Config) -> Result<(), Box<dyn Error>> {
    if config.stage == Stage::ONE {
        let sol = Solution::build(&config);
        let ret = sol.num_winning_solutions();
        println!("stage: {:?}, ret: {}", config.stage, ret);
    } else {
        let sol = Solution::build(&config);
        let ret = sol.num_winning_solutions();
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
