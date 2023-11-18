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

const SUM: i32 = 1;
const MULT: i32 = 2;
const END: i32 = 99;
const STAGE_TWO_CONST: i32 = 19690720;

struct Machine {
    data: Vec<i32>,
    pos: usize,
}

impl Machine {
    fn build(config: &Config) -> Machine {
        let mut m = Machine {
            data: config.content.trim().split(",").map(|x| x.parse::<i32>().unwrap()).collect(),
            pos: 0,
        };

        m.data[1] = 12;
        m.data[2] = 2;

        m
    }

    fn run(&mut self) -> i32 {
        loop {
            let opcode = self.data[self.pos];
            if opcode == END {
                return self.data[0];
            }

            let l = self.data[self.pos + 1] as usize;
            let r = self.data[self.pos + 2] as usize;
            let dst = self.data[self.pos + 3] as usize;

            let l = self.data[l];
            let r = self.data[r];
            match opcode {
                SUM  => {
                    self.data[dst] = l + r;
                },
                MULT => {
                    self.data[dst] = l * r;
                },
                _ => panic!("unsupported opcode {opcode}"),
            };

            self.pos += 4;
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
        if args.len() < 3 {
            return Err("not enough arguments, input file and stage are required".into());
        }

        let file_path = &args[2];

        let content = fs::read_to_string(file_path).map_err(|err| {
            format!("could not read file \"{file_path}\": {err}")
        })?;

        Ok(Config{
            stage: Stage::parse(&args[1]),
            content: content,

        })
    }
}

fn run(config: Config) -> Result<(), Box<dyn Error>> {
    if config.stage == Stage::ONE {
        let mut m = Machine::build(&config);
        let res = m.run();
        println!("stage: {0:?}, result: {1}", config.stage, res);
    } else {
        for verb in 0..99 {
            for noun in 0..99 {
                let mut m = Machine::build(&config);
                m.data[1] = noun;
                m.data[2] = verb;

                let res = m.run();
                if res == STAGE_TWO_CONST {
                    println!("stage: {0:?}, verb: {1}, noun: {2}, result: {3}", config.stage, verb, noun, 100*noun+verb);
                    return Ok(());
                }
            }
        }
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
