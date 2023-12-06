use std::cmp;
use std::env;
use std::error::Error;
use std::fs;
use std::process;
use std::thread;
use std::usize;

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

#[derive(Debug, Clone)]
struct SeedRange {
    start: usize,
    len: usize,
}

#[derive(Debug, Clone)]
struct RangeMap {
    src_start: usize,
    dst_start: usize,
    len: usize,
}

impl RangeMap {
    fn parse(line: &str) -> RangeMap {
        let ranges: Vec<usize> = line.trim().split_whitespace().map(|x| x.parse::<usize>().unwrap()).collect();
        if ranges.len() != 3 {
            panic!("invalid input line: {}", line);
        }

        RangeMap {
            src_start: ranges[1],
            dst_start: ranges[0],
            len: ranges[2],
        }
    }

    fn id_in_range(&self, id: usize) -> bool {
        id >= self.src_start && id < self.src_start + self.len
    }

    fn convert(&self, id: usize) -> Option<usize> {
        if self.id_in_range(id) {
            return Some(id - self.src_start + self.dst_start);
        }

        None
    }

    fn split_range(&self, range: &SeedRange) -> Option<RangeMap> {
        let start = cmp::max(range.start, self.src_start);
        let end = cmp::min(range.start + range.len, self.src_start + self.len);

        if start >= end {
            return None
        }

        Some(RangeMap {
            src_start: start,
            dst_start: self.convert(start).unwrap(),
            len: end - start,
        })
    }
}

#[derive(Debug, Clone)]
struct ConversionMap {
    name: String,
    maps: Vec<RangeMap>,
}

impl ConversionMap {
    fn new() -> ConversionMap {
        ConversionMap {
            name: "".to_string(),
            maps: vec![],
        }
    }

    fn feed(&mut self, line: &str) {
        if line.contains("map:") {
            self.name = line.to_string();
            return
        }

        self.maps.push(RangeMap::parse(line));
    }

    fn convert(&self, id: usize) -> usize {
        for rmap in self.maps.iter() {
            if let Some(dst_id) = rmap.convert(id) {
                return dst_id;
            }
        }

        id
    }

    fn split_range(&self, range: &SeedRange) -> Vec<RangeMap> {
        let mut convert_ranges: Vec<_> = vec![];
        for rmap in self.maps.iter() {
            if let Some(r) = rmap.split_range(range) {
                convert_ranges.push(r);
            }
        }

        if convert_ranges.len() == 0 {
            return vec![RangeMap {
                src_start: range.start,
                dst_start: range.start,
                len: range.len,
            }];
        }

        convert_ranges.sort_by_key(|r| r.src_start);

        let mut start = range.start;

        let mut ret: Vec<_> = vec![];
        for rmap in convert_ranges.into_iter() {
            if start < rmap.src_start {
                ret.push(RangeMap {
                    src_start: start,
                    dst_start: start,
                    len: rmap.src_start - start,
                });
            }

            start = rmap.src_start + rmap.len;
            ret.push(rmap);
        }
        ret
    }
}

struct Solution {
    seeds: Vec<usize>,
    seed_ranges: Vec<SeedRange>,
    maps: Vec<ConversionMap>,
}

impl Solution {
    fn build(config: &Config) -> Solution {
        let mut sol = Solution {
            seeds: vec![],
            maps: vec![],
            seed_ranges: vec![],
        };

        for line in config.content.lines() {
            if line.starts_with("seeds: ") {
                if config.stage == Stage::ONE {
                    sol.seeds = line.split(":").last().unwrap().trim().split_whitespace().map(|x| x.parse::<usize>().unwrap()).collect();
                } else {
                    let ranges: Vec<usize> = line.split(":").last().unwrap().trim().split_whitespace().map(|x| x.parse::<usize>().unwrap()).collect();
                    for idx in 0..(ranges.len()/2) {
                        sol.seed_ranges.push(
                            SeedRange {
                                start: ranges[2*idx+0],
                                len: ranges[2*idx+1],
                            });
                    }
                }
                continue
            }

            if line.contains("map:") {
                sol.maps.push(ConversionMap::new())
            }

            if line.len() > 1 && sol.maps.len() > 0 {
                sol.maps.last_mut().unwrap().feed(line);
            }
        }

        sol
    }

    fn find_closest_location(&self) -> usize {
        let mut shortest_dist = usize::max_value();
        for seed in self.seeds.iter() {
            let mut conversion: usize = *seed;
            for cmap in self.maps.iter() {
                conversion = cmap.convert(conversion);
            }

            if conversion < shortest_dist {
                shortest_dist = conversion;
            }
        }

        shortest_dist
    }

    fn split_seed_ranges(&self) -> usize {
        let mut min_dist = usize::max_value();
        for seed_range in self.seed_ranges.iter() {
            let mut seed_ranges = vec![seed_range.to_owned()];

            for map in self.maps.iter() {
                let mut groupped_ranges = vec![];
                for sr in seed_ranges.into_iter() {
                    let split_src_ranges = map.split_range(&sr);
                    let mut converted_ranges = split_src_ranges.into_iter().map(|rm| {
                        SeedRange {
                            start: rm.dst_start,
                            len: rm.len,
                        }
                    }).collect();

                    groupped_ranges.append(&mut converted_ranges);
                }

                seed_ranges = groupped_ranges;
            }

            let min = seed_ranges.into_iter().min_by_key(|a| a.start).unwrap().start;
            if min < min_dist {
                min_dist = min;
            }
        }
        min_dist
    }

    fn parallel_brute_force(&self) -> usize {
        let mut handles: Vec<_> = vec![];
        for seed_range in self.seed_ranges.clone().iter() {
            let seed_range = seed_range.to_owned();
            let maps = self.maps.clone();

            let handle = thread::spawn(move || {
                let mut shortest_dist = usize::max_value();
                let maps = maps.clone();
                for seed in seed_range.start..(seed_range.start+seed_range.len) {
                    let mut conversion: usize = seed;
                    for cmap in maps.iter() {
                        conversion = cmap.convert(conversion);
                    }

                    if conversion < shortest_dist {
                        shortest_dist = conversion;
                    }
                }

                shortest_dist
            });

            handles.push(handle);
        }

        let ret: Vec<usize> = handles.into_iter().map(|h| { h.join().unwrap() }).collect();

        ret.into_iter().min().unwrap()

    }
}

fn run(config: Config) -> Result<(), Box<dyn Error>> {
    if config.stage == Stage::ONE {
        let sol = Solution::build(&config);
        let ret = sol.find_closest_location();
        println!("stage: {:?}, ret: {}", config.stage, ret);
    } else {
        let sol = Solution::build(&config);
        //let ret = sol.run_parallel();
        let ret = sol.split_seed_ranges();
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
