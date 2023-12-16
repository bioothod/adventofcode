use std::env;
use std::error::Error;
use std::process;

use crate::solution::Solution;
mod solution;

use crate::config::Config;
use crate::config::Stage;
mod config;

fn run(config: Config) -> Result<(), Box<dyn Error>> {
    if config.stage == Stage::ONE {
        let mut sol = Solution::build(&config);
        let ret = sol.stage1();
        println!("stage: {:?}, ret: {}", config.stage, ret);
    } else {
        let mut sol = Solution::build(&config);
        let ret = sol.stage2();
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
