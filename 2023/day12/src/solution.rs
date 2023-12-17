use std::collections::{HashMap, HashSet};
use std::fmt;

use std::sync::mpsc::channel;
use threadpool::ThreadPool;

use crate::config::Config;
use crate::Stage;

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
enum RecordType {
    OPERATIONAL,
    DAMAGED,
    UNKNOWN,
}
use RecordType::*;

impl fmt::Debug for RecordType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_char())
    }
}

fn vec_to_string(line: &[RecordType]) -> String {
    let chars = line.iter().map(|c| c.to_char()).collect::<Vec<char>>();
    String::from_iter(chars)
}

impl RecordType {
    fn parse(c: char) -> RecordType {
        match c {
            '.' => OPERATIONAL,
            '#' => DAMAGED,
            '?' => UNKNOWN,
            _ => panic!("unsupported char {}", c),
        }
    }

    fn to_char(&self) -> char {
        match self {
            OPERATIONAL => '.',
            DAMAGED => '#',
            UNKNOWN => '?',
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
struct Region {
    start: usize,
    size: usize,
    skip: bool,
}

impl fmt::Debug for Region {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let type_char = if self.skip { "." } else { "#" };
        write!(
            f,
            "[{},{} {})",
            self.start,
            self.start + self.size,
            type_char
        )
    }
}

impl Region {
    fn new(start: usize, size: usize, skip: bool) -> Region {
        Region { start, size, skip }
    }
}

fn regions_to_string(regions: &Vec<Region>) -> String {
    let region_strings = regions.iter().fold(vec![], |acc, r| {
        let fill_char = if r.skip { "." } else { "#" };
        let mut rstr = vec![fill_char; r.size];
        rstr.extend(acc);
        rstr
    });
    String::from_iter(region_strings.into_iter().rev())
}

#[derive(Clone)]
struct Record {
    mask: Vec<RecordType>,
    broken_regions: Vec<usize>,
}

impl Record {
    fn parse(line: &str, stage: Stage) -> Record {
        let mult = 5;

        let spl = line.split_whitespace().collect::<Vec<&str>>();
        let mask_string = if stage == Stage::ONE {
            spl[0].to_owned()
        } else {
            let v = vec![spl[0]; mult];
            v.join("?")
        };

        let regions_string = if stage == Stage::ONE {
            spl[1].to_owned()
        } else {
            let v = vec![spl[1]; mult];
            v.join(",")
        };
        let mask: Vec<RecordType> = mask_string.chars().map(RecordType::parse).collect();
        let broken_regions = regions_string
            .split(",")
            .map(|subl| subl.parse::<usize>().unwrap())
            .collect();
        let unknown_positions = mask
            .iter()
            .enumerate()
            .filter_map(|(i, &c)| if c == UNKNOWN { Some(i) } else { None })
            .collect::<Vec<usize>>();

        Record {
            mask,
            broken_regions,
        }
    }

    fn find_damaged_region(&self, start_idx: usize, region_size: usize) -> Option<usize> {
        let mut start_idx = start_idx;
        while start_idx + region_size <= self.mask.len() {
            let mask = &self.mask[start_idx..(start_idx + region_size)];
            let matching_region_size = mask.into_iter().filter(|&c| c != &OPERATIONAL).count();

            if matching_region_size == region_size {
                return Some(start_idx);
            }

            if self.mask[start_idx] == DAMAGED {
                return None;
            }

            start_idx += 1;
        }

        None
    }

    fn find_skip_region(
        &self,
        regions: &mut Vec<Region>,
        start_idx: usize,
        cache: &HashSet<Vec<Region>>,
    ) -> Option<usize> {
        let mut idx = start_idx;

        loop {
            if self.mask[idx] == DAMAGED {
                return None;
            }

            idx += 1;
            if idx == self.mask.len() {
                return None;
            }

            if idx == start_idx + 1 {
                regions.push(Region::new(start_idx, idx - start_idx, true));
            } else {
                *regions.last_mut().unwrap() = Region::new(start_idx, idx - start_idx, true);
            }

            if !cache.contains(regions) {
                break;
            }
        }

        return Some(idx);
    }

    fn match_regions(&mut self) -> usize {
        let mut matches = 0;
        let mut cache: HashSet<Vec<Region>> = HashSet::new();
        let mut reset = false;

        while !reset {
            let mut write_idx = 0;
            let mut damaged_regions: Vec<usize> = vec![];
            let mut regions: Vec<Region> = vec![];
            let mut broken_regions_idx_start = 0;

            println!(
                "start: {:?} {:?} cache: {}",
                vec_to_string(&self.mask),
                self.broken_regions,
                cache.capacity()
            );

            while !reset {
                for (broken_region_idx, r) in self.broken_regions
                    [broken_regions_idx_start..self.broken_regions.len()]
                    .iter()
                    .enumerate()
                {
                    if let Some(last) = regions.last() {
                        if !last.skip
                    let Some(start_idx) = self.find_damaged_region(write_idx, *r) else {
                        println!(
                        "{:?}: broken_regions: {:?}, broken_region_idx: {}, r: {}: could not find damaged region",
                        regions_to_string(&regions),
                        self.broken_regions, broken_region_idx, r
                    );

                        if broken_region_idx == 0 {
                            reset = true;
                        }

                        break;
                    };
                    if start_idx != write_idx {
                        regions.push(Region::new(write_idx, start_idx - write_idx, true));
                    }
                    regions.push(Region::new(start_idx, *r, false));

                    damaged_regions.push(*r);

                    println!(
                        "{:?}: damaged_regions: {:?}/{:?} [{}], write_idx: {} -> {}",
                        regions_to_string(&regions),
                        damaged_regions,
                        self.broken_regions,
                        if damaged_regions == self.broken_regions {
                            "+"
                        } else {
                            "-"
                        },
                        write_idx,
                        start_idx + r,
                    );

                    write_idx = start_idx + r;

                    if damaged_regions.len() == self.broken_regions.len() {
                        let rest_is_operational = self.mask[write_idx..self.mask.len()]
                            .iter()
                            .all(|&c| c != DAMAGED);
                        if rest_is_operational {
                            println!("{:?}: found match", regions_to_string(&regions));
                            matches += 1;
                        }

                        cache.insert(regions.clone());
                        break;
                    }

                    let Some(skip_idx) = self.find_skip_region(&mut regions, write_idx, &cache)
                    else {
                        println!(
                            "{:?}: regions: {:?}, broken_regions: {:?}, write_idx: {}: could not find a skip region",
                            regions_to_string(&regions),
                            regions, self.broken_regions, write_idx
                        );
                        break;
                    };

                    cache.insert(regions.clone());
                    write_idx = skip_idx;
                }

                if let Some(last) = regions.pop() {
                    if !last.skip {
                        damaged_regions.pop();
                        broken_regions_idx_start = damaged_regions.len();
                        println!(
                            "{:?}: removed last: {:?}, write_idx: {} -> {}",
                            regions_to_string(&regions),
                            last,
                            write_idx,
                            write_idx - last.size
                        );
                    }
                    write_idx -= last.size;
                } else {
                    break;
                }
            }
        }

        println!(
            "region done: {:?} {:?} -> {}",
            vec_to_string(&self.mask),
            self.broken_regions,
            matches,
        );
        matches
    }
}

pub struct Solution {
    records: Vec<Record>,
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let sol = Solution {
            records: config
                .content
                .lines()
                .map(|line| Record::parse(&line, config.stage.clone()))
                .collect(),
        };

        sol
    }

    pub fn stage1(&mut self) -> usize {
        self.records.iter_mut().map(|r| r.match_regions()).sum()
    }

    pub fn stage2(&mut self) -> usize {
        let n_workers = 8;
        let n_jobs = self.records.len();
        let pool = ThreadPool::new(n_workers);

        let (tx, rx) = channel();
        for r in self.records.iter() {
            let tx = tx.clone();
            let mut r = r.clone();
            pool.execute(move || {
                let num = r.match_regions();
                tx.send(num).expect("channel not closed");
            });
        }
        rx.iter().take(n_jobs).sum()
    }
    pub fn stage21(&mut self) -> usize {
        self.records.iter_mut().map(|r| r.match_regions()).sum()
    }
}
