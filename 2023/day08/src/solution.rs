use regex::Regex;

use std::collections::HashMap;

use crate::config::Config;
use crate::numbers;

pub struct Solution {
    instruction: String,
    mapl: HashMap<String, String>,
    mapr: HashMap<String, String>,
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let mut sol = Solution {
            instruction: "".to_string(),
            mapl: HashMap::new(),
            mapr: HashMap::new(),
        };

        let re = Regex::new(r"(?<src>\w+) = \((?<dstl>\w+), (?<dstr>\w+)\)").unwrap();

        for (i, line) in config.content.lines().enumerate() {
            if i == 0 {
                sol.instruction = line.to_owned();
                continue
            }

            if line.len() == 0 {
                continue
            }

            let Some(caps) = re.captures(line) else {
                panic!("could not parse input string {line}");
            };
            let src = &caps["src"];
            let dstl = &caps["dstl"];
            let dstr = &caps["dstr"];

            sol.mapl.entry(src.to_string()).or_insert(dstl.to_string());
            sol.mapr.entry(src.to_string()).or_insert(dstr.to_string());
        }

        sol
    }

    fn num_steps<F>(&self, start: &str, finished: F) -> usize
        where F: Fn(&str) -> bool
    {
        let mut steps = 0;
        let mut start = start;

        loop {
            for step in self.instruction.chars() {
                start = match step {
                    'L' => self.mapl.get(start).unwrap(),
                    'R' => self.mapr.get(start).unwrap(),
                    _ => panic!("unsupported instruction \"{}\" in {}", step, self.instruction),
                };

                steps += 1;

                if finished(&start) {
                    return steps;
                }
            }
        }
    }

    pub fn stage1(&self) -> usize {
        self.num_steps("AAA", |x| x == "ZZZ")
    }

    pub fn stage2(&self) -> usize {
        let starts: Vec<_> = self.mapl.keys().filter(|src| src.ends_with('A')).collect();
        println!("starts: {:?}", starts);
        let steps_all: Vec<_> = starts.iter().map(|start| self.num_steps(start, |x| x.ends_with('Z'))).collect();

        let mut lcm_steps = 1;
        for (start, steps) in starts.iter().zip(steps_all.iter()) {
            lcm_steps = numbers::lcm(lcm_steps, *steps);

            println!("{:?}: {:?} -> {}", start, steps, lcm_steps);
        }

        lcm_steps
    }
}
