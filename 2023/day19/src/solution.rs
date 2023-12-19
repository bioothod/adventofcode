use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;

use regex::Regex;

use crate::config::Config;

#[derive(Clone, Debug)]
struct Part {
    categories: HashMap<char, isize>,
}

impl Part {
    fn new() -> Part {
        Part {
            categories: HashMap::new(),
        }
    }
    fn parse(line: &str) -> Part {
        let line = &line[1..(line.len() - 1)];
        let re = Regex::new(r"(?<id>[xmas])=(?<value>\d+),?").unwrap();
        re.captures_iter(line).fold(Part::new(), |mut acc, caps| {
            let id: char = caps["id"].chars().nth(0).unwrap();
            let value = caps["value"].parse::<isize>().unwrap();
            acc.categories.insert(id, value);
            acc
        })
    }
}

#[derive(Clone)]
struct Range {
    start: isize,
    end: isize,
}
impl fmt::Debug for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}-{}]", self.start, self.end)
    }
}

#[derive(Clone, Debug)]
struct PartRange {
    categories: HashMap<char, Range>,
    dst: String,
}

struct Rule {
    input: char,
    limit: isize,
    op: Ordering,
    output: String,
}

impl Rule {
    fn parse_op(line: &str) -> Option<Rule> {
        let re = Regex::new(r"(?<input>\w+)(?<op>[<>])(?<limit>\d+):(?<output>\w+)").unwrap();
        let Some(caps) = re.captures(line) else {
            return None;
        };

        let input: char = caps["input"].chars().nth(0).unwrap();
        let output = caps["output"].to_string();
        let limit: isize = caps["limit"].parse::<isize>().unwrap();
        let op: Ordering = match &caps["op"] {
            ">" => Ordering::Greater,
            "<" => Ordering::Less,
            _ => panic!("unsupported op {} in {}", &caps["op"], line),
        };

        Some(Rule {
            input,
            limit,
            op,
            output,
        })
    }

    fn parse_fallback(line: &str) -> Rule {
        let re = Regex::new(r"(?<output>\w+)").unwrap();
        let Some(caps) = re.captures(line) else {
            panic!("could not parse rule {}", line);
        };
        let output = caps["output"].to_string();

        Rule {
            input: ' ',
            limit: 0,
            op: Ordering::Equal,
            output,
        }
    }

    fn parse(line: &str) -> Rule {
        if let Some(rule) = Rule::parse_op(line) {
            return rule;
        }

        Rule::parse_fallback(line)
    }

    fn run(&self, p: &Part) -> Option<String> {
        if self.input != ' ' {
            if p.categories.get(&self.input).unwrap().cmp(&self.limit) == self.op {
                return Some(self.output.clone());
            }

            return None;
        }

        return Some(self.output.clone());
    }

    fn run_range(&self, input_range: &PartRange) -> (Option<PartRange>, Option<PartRange>) {
        if self.input == ' ' {
            let mut c: PartRange = input_range.clone();
            c.dst = self.output.clone();
            return (Some(c), None);
        }

        let check = input_range.categories.get(&self.input).unwrap();

        if self.op == Ordering::Less {
            if check.start >= self.limit {
                return (None, Some(input_range.clone()));
            }

            let mut matching_range = input_range.clone();
            let ent = matching_range.categories.get_mut(&self.input).unwrap();
            ent.end = std::cmp::min(check.end, self.limit - 1);
            matching_range.dst = self.output.clone();

            let non_matching_range_option = if check.end > self.limit {
                let mut non_matching_range = input_range.clone();
                let ent = non_matching_range.categories.get_mut(&self.input).unwrap();
                ent.start = self.limit;
                non_matching_range.dst = "None".to_string();
                Some(non_matching_range)
            } else {
                None
            };

            return (Some(matching_range), non_matching_range_option);
        }

        if self.op == Ordering::Greater {
            if check.end <= self.limit {
                return (None, Some(input_range.clone()));
            }

            let mut matching_range = input_range.clone();
            let ent = matching_range.categories.get_mut(&self.input).unwrap();
            ent.start = std::cmp::max(check.start, self.limit + 1);
            matching_range.dst = self.output.clone();

            let non_matching_range_option = if check.start < self.limit {
                let mut non_matching_range = input_range.clone();
                let ent = non_matching_range.categories.get_mut(&self.input).unwrap();
                ent.end = self.limit;
                non_matching_range.dst = "None".to_string();
                Some(non_matching_range)
            } else {
                None
            };

            return (Some(matching_range), non_matching_range_option);
        }

        unreachable!()
    }
}

struct Workflow {
    name: String,
    rules: Vec<Rule>,
    collected: Vec<Part>,
}

impl Workflow {
    fn parse(line: &str) -> Workflow {
        let re = Regex::new(r"(?<name>\w+)\{(?<rules>[a-zA-Z0-9,:<>]+\})").unwrap();
        let Some(caps) = re.captures(line) else {
            panic!("could not parse workflow {}", line);
        };

        let name = caps["name"].to_string();
        let rules = caps["rules"]
            .split(",")
            .map(|s| Rule::parse(s))
            .collect::<Vec<Rule>>();

        Workflow {
            name,
            rules,
            collected: vec![],
        }
    }

    fn collecting(name: &str) -> Workflow {
        Workflow {
            name: name.to_string(),
            rules: vec![],
            collected: vec![],
        }
    }

    fn run(&mut self, part: &Part) -> Option<String> {
        for rule in self.rules.iter() {
            if let Some(dst) = rule.run(part) {
                return Some(dst);
            }
        }

        self.collected.push(part.clone());
        None
    }

    fn run_range(&mut self, part: &PartRange) -> Vec<PartRange> {
        let mut ret: Vec<PartRange> = vec![];
        let mut part = part.clone();

        for rule in self.rules.iter() {
            let (matching, non_matching) = rule.run_range(&part);

            if let Some(m) = matching {
                ret.push(m);
            }

            if let Some(nonm) = non_matching {
                part = nonm;
            } else {
                break;
            }
        }

        ret
    }
}

pub struct Solution {
    workflows: HashMap<String, Workflow>,
    parts: Vec<Part>,
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let mut lines = config.content.lines();

        let mut sol = Solution {
            workflows: (&mut lines)
                .take_while(|line| line.len() > 0)
                .map(|line| {
                    let w = Workflow::parse(line);
                    (w.name.clone(), w)
                })
                .collect(),
            parts: lines.map(Part::parse).collect(),
        };

        sol.workflows
            .insert("R".to_string(), Workflow::collecting("R"));
        sol.workflows
            .insert("A".to_string(), Workflow::collecting("A"));

        sol
    }

    pub fn stage1(&mut self) -> isize {
        for p in self.parts.iter() {
            let mut w = self.workflows.get_mut("in").unwrap();
            while let Some(dst) = w.run(p) {
                w = self.workflows.get_mut(&dst).unwrap();
            }
        }

        let a = self.workflows.get("A").unwrap();
        a.collected
            .iter()
            .map(|p| p.categories.values().sum::<isize>())
            .sum::<isize>()
    }

    pub fn stage2(&mut self) -> isize {
        let start = PartRange {
            categories: "xmas"
                .chars()
                .map(|c| {
                    (
                        c,
                        Range {
                            start: 1,
                            end: 4000,
                        },
                    )
                })
                .collect::<HashMap<char, Range>>(),
            dst: "in".to_string(),
        };

        let mut finals: Vec<PartRange> = vec![];

        let mut ranges: Vec<PartRange> = vec![start];
        while ranges.len() > 0 {
            let mut new_ranges: Vec<PartRange> = vec![];
            for r in ranges.iter() {
                let w = self.workflows.get_mut(&r.dst).unwrap();

                for nr in w.run_range(r).iter() {
                    match nr.dst.as_str() {
                        "R" => continue,
                        "A" => finals.push(nr.clone()),
                        _ => new_ranges.push(nr.clone()),
                    };
                }
            }

            ranges = new_ranges;
        }

        finals
            .iter()
            .map(|r| {
                r.categories
                    .values()
                    .map(|r| r.end - r.start + 1)
                    .product::<isize>()
            })
            .sum()
    }
}
