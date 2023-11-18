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
struct Line {
    x0: i32,
    y0: i32,
    x1: i32,
    y1: i32,
}

impl Line {
    fn build(x0: i32, y0: i32, desc: &str) -> Line {
        let mut l = Line{
            x0: x0,
            y0: y0,
            x1: x0,
            y1: y0,
        };
        let dir = desc.chars().nth(0).unwrap();
        let step = desc[1..].parse::<i32>().unwrap();

        match dir {
            'R' => l.x1 += step,
            'L' => l.x1 -= step,
            'U' => l.y1 += step,
            'D' => l.y1 -= step,
            _ => panic!("unsupported direction {dir}"),
        }

        l
    }

    fn xin(&self, other_x: i32) -> bool {
        if self.x0 <= other_x && other_x <= self.x1 {
            return true
        }
        if self.x1 <= other_x && other_x <= self.x0 {
            return true
        }

        return false
    }

    fn yin(&self, other_y: i32) -> bool {
        if self.y0 <= other_y && other_y <= self.y1 {
            return true
        }
        if self.y1 <= other_y && other_y <= self.y0 {
            return true
        }

        return false
    }

    fn intersect(&self, other: &Line) -> Vec<(i32, i32)> {
        let mut v: Vec<(i32, i32)> = vec!();

        if self.x0 == self.x1 {
            if other.y0 == other.y1 {
                if self.yin(other.y0) && other.xin(self.x0) {
                    return vec!((self.x0, other.y0));
                }
            } else {
                if other.x0 != other.x1 {
                    panic!("non straight line construction 1: self: {0:?}, other: {1:?}", self, other);
                }

                if other.x0 == self.x0 {
                    if self.yin(other.y0) {
                        v.push((self.x0, other.y0));
                    }
                    if self.yin(other.y1) {
                        v.push((self.x0, other.y1));
                    }
                    if other.yin(self.y0) {
                        v.push((self.x0, self.y0));
                    }
                    if other.yin(self.y1) {
                        v.push((self.x0, self.y1));
                    }

                    return v;
                }
            }
        } else /* self.y0 == self.y1 */ {
            if other.x0 == other.x1 {
                if self.xin(other.x0) && other.yin(self.y0) {
                    return vec!((other.x0, self.y0));
                }
            } else {
                if other.y0 != other.y1 {
                    panic!("non straight line construction 2: self: {0:?}, other: {1:?}", self, other);
                }

                if other.y0 == self.y0 {
                    if self.xin(other.x0) {
                        v.push((other.x0, self.y0));
                    }
                    if self.xin(other.x1) {
                        v.push((other.x1, self.y0));
                    }
                    if other.xin(self.x0) {
                        v.push((self.x0, self.y0));
                    }
                    if other.xin(self.x1) {
                        v.push((self.x1, self.y0));
                    }

                    return v;
                }
            }
        }

        return v;
    }
}

struct Wire {
    lines: Vec<Line>,
}

impl Wire {
    fn build(conf: &str) -> Wire {
        let mut x0 = 0;
        let mut y0 = 0;
        let mut wire = Wire{
            lines: vec![],
        };

        for desc in conf.split(",") {
            let l = Line::build(x0, y0, desc);
            x0 = l.x1;
            y0 = l.y1;

            wire.lines.push(l);
        }

        wire
    }

    fn intersect(&self, stage: Stage, o: &Wire) -> Option<i32> {
        let mut min_dist: Option<i32> = None;

        let mut w0_dist = 0;

        for w0 in self.lines.iter() {
            w0_dist += (w0.x0 - w0.x1).abs() + (w0.y0 - w0.y1).abs();

            let mut w1_dist = 0;
            for w1 in o.lines.iter() {
                w1_dist += (w1.x0 - w1.x1).abs() + (w1.y0 - w1.y1).abs();

                for intersect in w0.intersect(&w1) {
                    if intersect.0 == 0 && intersect.0 == 0 {
                        continue
                    }

                    let dist = if stage == Stage::ONE {
                        intersect.0.abs() + intersect.1.abs()
                    } else {
                        let mut d = w0_dist + w1_dist;
                        if w0.x0 == w0.x1 {
                            d -= (w0.y1 - intersect.1).abs();
                        } else {
                            d -= (w0.x1 - intersect.0).abs();
                        }

                        if w1.x0 == w1.x1 {
                            d -= (w1.y1 - intersect.1).abs();
                        } else {
                            d -= (w1.x1 - intersect.0).abs();
                        }

                        d
                    };

                    if min_dist == None || dist < min_dist? {
                        min_dist = Some(dist);
                    }
                }
            }
        }

        min_dist
    }
}

fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let wires: Vec<Wire> = config.content.lines().map(|line| {
        Wire::build(line)
    }).collect();

    let min_dist = wires[1].intersect(config.stage.clone(), &wires[0]);
    if let Some(min_dist) = min_dist {
        println!("stage: {0:?}, min_dist: {1}", config.stage, min_dist);
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
