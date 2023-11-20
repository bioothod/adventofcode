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

#[derive(Debug, PartialEq, Clone, Copy)]
enum Mode {
    POSITION,
    IMMEDIATE,
}

#[derive(Debug, Clone)]
struct Opcode {
    code: i32,
    stride: i32,
    modes: Vec<Mode>,
}

impl Opcode {
    fn parse(code: i32) -> Opcode {
        let mut digits: Vec<i32> = vec![];
        let mut num = code;

        for _ in 0..5 {
            let m = num % 10;
            digits.push(m);

            num = num / 10;
        }

        let opcode = digits[0] + digits[1]*10;
        let modes: Vec<Mode> = digits[2..5].iter().map(|d| {
            match d {
                0 => Mode::POSITION,
                1 => Mode::IMMEDIATE,
                _ => panic!("unsupported mode {0:?} in opcode digits {1:?} while parsing opcode data {2:?}", d, digits, code),
            }
        }).collect();

        let stride = match opcode {
            1 | 2 => 4,
            3 | 4 => 2,
            5 | 6 => 3,
            7 | 8 => 4,
            99 => 1,
            _ => panic!("unsupported opcode {0} while parsing opcode data {1:?}", opcode, code),
        };

        Opcode{
            code: opcode,
            stride: stride,
            modes: modes,
        }
    }
}

struct Machine {
    data: Vec<i32>,
    pos: i32,
}

impl Machine {
    fn build(config: &Config) -> Machine {
        let m = Machine {
            data: config.content.trim().split(",").map(|x| x.parse::<i32>().unwrap()).collect(),
            pos: 0,
        };

        m
    }

    fn read_value(&self, input: i32, mode: Mode) -> i32 {
        let src = self.data[input as usize];
        match mode {
            Mode::POSITION => self.data[src as usize],
            Mode::IMMEDIATE => src,
        }
    }
    fn write_value(&mut self, input: i32, value: i32) {
        let dst = self.data[input as usize];
        self.data[dst as usize] = value;
    }

    fn add(&mut self, opcode: &Opcode) -> i32 {
        let v0 = self.read_value(self.pos + 1 + 0, opcode.modes[0]);
        let v1 = self.read_value(self.pos + 1 + 1, opcode.modes[1]);
        let result = v0 + v1;

        self.write_value(self.pos + 1 + 2, result);
        opcode.stride
    }

    fn multiple(&mut self, opcode: &Opcode) -> i32 {
        let v0 = self.read_value(self.pos + 1 + 0, opcode.modes[0]);
        let v1 = self.read_value(self.pos + 1 + 1, opcode.modes[1]);
        let result = v0 * v1;

        self.write_value(self.pos + 1 + 2, result);
        opcode.stride
    }

    fn save(&mut self, opcode: &Opcode, value: i32) -> i32 {
        self.write_value(self.pos + 1, value);
        opcode.stride
    }

    fn output(&self, opcode: &Opcode) -> i32 {
        let output = self.read_value(self.pos + 1, opcode.modes[0]);
        output
    }

    fn jump_if_true(&mut self, opcode: &Opcode) -> i32 {
        let v0 = self.read_value(self.pos + 1 + 0, opcode.modes[0]);
        let v1 = self.read_value(self.pos + 1 + 1, opcode.modes[1]);
        if v0 != 0 {
            self.pos = v1;
            return 0;
        }

        opcode.stride
    }

    fn jump_if_false(&mut self, opcode: &Opcode) -> i32 {
        let v0 = self.read_value(self.pos + 1 + 0, opcode.modes[0]);
        let v1 = self.read_value(self.pos + 1 + 1, opcode.modes[1]);
        if v0 == 0 {
            self.pos = v1;
            return 0;
        }

        opcode.stride
    }

    fn less_than(&mut self, opcode: &Opcode) -> i32 {
        let v0 = self.read_value(self.pos + 1 + 0, opcode.modes[0]);
        let v1 = self.read_value(self.pos + 1 + 1, opcode.modes[1]);

        if v0 < v1 {
            self.write_value(self.pos+3, 1);
        } else {
            self.write_value(self.pos+3, 0);
        }

        opcode.stride
    }

    fn equals(&mut self, opcode: &Opcode) -> i32 {
        let v0 = self.read_value(self.pos + 1 + 0, opcode.modes[0]);
        let v1 = self.read_value(self.pos + 1 + 1, opcode.modes[1]);

        if v0 == v1 {
            self.write_value(self.pos+3, 1);
        } else {
            self.write_value(self.pos+3, 0);
        }

        opcode.stride
    }

    fn run(&mut self, input: i32) -> i32 {
        let mut output = 0;
        loop {
            let opcode = Opcode::parse(self.data[self.pos as usize]);
            let stride = match opcode.code {
                1 => self.add(&opcode),
                2 => self.multiple(&opcode),
                3 => self.save(&opcode, input),
                4 => {
                    output = self.output(&opcode);
                    //println!("output: {output}");
                    opcode.stride
                },
                5 => self.jump_if_true(&opcode),
                6 => self.jump_if_false(&opcode),
                7 => self.less_than(&opcode),
                8 => self.equals(&opcode),
                99 => return output,
                _ => panic!("unsupported opcode: {0:#?}", opcode),
            };

            self.pos += stride;
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
        let res = m.run(1);
        println!("stage: {0:?}, result: {1}", config.stage, res);
    } else {
        let mut m = Machine::build(&config);
        let res = m.run(5);
        println!("stage: {0:?}, result: {1}", config.stage, res);
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
