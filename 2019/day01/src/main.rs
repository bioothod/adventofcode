use std::env;
use std::error::Error;
use std::fs;
use std::process;

#[derive(Debug, PartialEq)]
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

    fn calculate_fuel(&self, mut mass: f32) -> i32 {
        let mut total_mass: i32 = 0;

        loop {
            mass = (mass / 3.).floor() - 2.;

            if mass <= 0. {
                break
            }

            total_mass += mass as i32;

            if self.stage == Stage::ONE {
                break
            }
        }

        total_mass
    }
}

fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let mut total_mass: i32 = 0;
    for line in config.content.lines() {
        let mass = line.parse::<f32>().unwrap();

        total_mass += config.calculate_fuel(mass)
    }

    println!("total mass: {total_mass}");
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
