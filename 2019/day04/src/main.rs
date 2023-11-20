use std::env;
use std::error::Error;
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

        let stage = Stage::parse(&args[1]);
        let content = "108457-562041";
        //let content = "488888-488888";

        Ok(Config{
            stage: stage,
            content: content.to_string(),
        })
    }

}

fn to_digits(mut num: i32) -> Vec<i32> {
    let mut ret: Vec<i32> = vec![];
    loop {
        let m = num % 10;
        ret.push(m);

        if num < 10 {
            break
        }
        num = num / 10;
    }

    ret.reverse();
    ret
}

fn is_good_stage1(password: Vec<i32>) -> bool {
    let mut have_the_same = false;
    let mut prev: &i32 = &password[0];
    for (i, digit) in password.iter().enumerate() {
        if i == 0 {
            continue
        }

        if digit == prev {
            have_the_same = true;
        }

        if digit < prev {
            return false
        }

        prev = digit;
    }

    have_the_same
}

fn is_good_stage2(password: Vec<i32>) -> bool {
    let mut group: Vec<i32> = vec![];
    let mut have_good_group = false;

    for (i, digit) in password.iter().enumerate() {
        if i == 0 {
            group.push(*digit);
            continue
        }

        let prev = password[i-1];

        if *digit < prev {
            return false
        }

        if *digit == prev {
            group.push(*digit);
        } else {
            if group.len() == 2 {
                have_good_group = true;
            }

            group = vec![*digit];
        }
    }

    if have_good_group || group.len() == 2 {
        return true
    }

    false
}

fn check_passwords(stage: Stage, num0: i32, num1: i32) -> i32 {
    let check_good = if stage == Stage::ONE { is_good_stage1 } else { is_good_stage2 };

    let mut num_good = 0;
    for num in num0..=num1 {
        let digits = to_digits(num);


        if check_good(digits) {
            num_good += 1;
        }
    }

    num_good
}

fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let nums: Vec<i32> = config.content.split("-").map(|n| n.parse::<i32>().unwrap()).collect();

    let num_passwords = check_passwords(config.stage.clone(), nums[0], nums[1]);
    println!("stage: {:?}, num_passwords: {}", config.stage, num_passwords);

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
