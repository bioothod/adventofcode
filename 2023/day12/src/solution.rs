use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::fmt;

use std::hash::{Hash, Hasher};
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
    start_orig: usize,
    size: usize,
}

impl fmt::Debug for Region {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{},{})", self.start, self.start + self.size,)
    }
}

impl Region {
    fn new(start: usize, size: usize) -> Region {
        Region {
            start,
            start_orig: 0,
            size,
        }
    }
}

#[derive(Clone)]
struct Record {
    mask: Vec<RecordType>,
    broken_regions: Vec<usize>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
struct State {
    start: usize,
    idx: usize,
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

        Record {
            mask,
            broken_regions,
        }
    }

    fn regions_to_string(&self, regions: &Vec<Region>) -> String {
        let mut output: Vec<char> = self
            .mask
            .iter()
            .map(|r| match r {
                UNKNOWN => '?',
                DAMAGED => '#',
                OPERATIONAL => '.',
            })
            .collect();

        for r in regions.iter() {
            for i in r.start..(r.start + r.size) {
                output[i] = 'R';
            }
        }

        String::from_iter(output.into_iter())
    }

    fn find_damaged_region(&self, start_idx_orig: usize, region_size: usize) -> Option<usize> {
        let mut start_idx = start_idx_orig;

        if start_idx + region_size > self.mask.len() {
            return None;
        }

        if start_idx > 0 {
            if self.mask[start_idx - 1] == DAMAGED {
                return None;
            }
        }

        while start_idx + region_size <= self.mask.len() {
            let mask = &self.mask[start_idx..(start_idx + region_size)];
            let matching_region_size = mask.into_iter().filter(|&c| c != &OPERATIONAL).count();

            if matching_region_size == region_size {
                if start_idx + region_size == self.mask.len()
                    || self.mask[start_idx + region_size] != DAMAGED
                {
                    // println!("find_damaged_region: start_idx: {} -> {}, region_size: {}, start+size: {}/{}, after region: {:?}",
                    //          start_idx_orig, start_idx, region_size, start_idx+region_size, self.mask.len(),
                    //          if start_idx + region_size < self.mask.len() {self.mask[start_idx+region_size].to_char()}else {'E'}

                    // );
                    return Some(start_idx);
                }
            }

            // we could not match the whole @region_size, but the region starts with the DAMAGED entry,
            // which means we already have a DAMAGED region which has incorrect size
            if self.mask[start_idx] == DAMAGED {
                return None;
            }

            start_idx += 1;
        }

        None
    }

    fn match_regions(&mut self, match_regions_call_idx: usize) -> usize {
        let mut matches = 0;
        let mut gens: Vec<Region> = self
            .broken_regions
            .iter()
            .map(|r| Region::new(0, *r))
            .collect();
        let mut broken_region_idx = 0;
        let mut broken_region_start = 0;
        let mut cache: HashMap<State, (usize, usize)> = HashMap::new();
        let mut needs_update: HashSet<State> = HashSet::new();

        let rest_is_operational = |start| {
            if start >= self.mask.len() {
                return true;
            }

            (start..self.mask.len())
                .filter(|&idx| self.mask[idx] != DAMAGED)
                .count()
                == (self.mask.len() - start)
        };

        loop {
            let mut skip = false;
            let last_g = gens.get_mut(broken_region_idx).unwrap();
            let key = State {
                start: last_g.start_orig,
                idx: broken_region_idx,
            };
            if !cache.contains_key(&key) {
                cache.insert(key.clone(), (matches, 0));
                needs_update.insert(key);
            } else {
                let cached = cache.get(&key).unwrap();
                skip = true;

                last_g.start = cached.1;
                matches += cached.0;
                broken_region_idx += 1;
            }

            if !skip {
                let mut all_is_good = false;
                while broken_region_idx < self.broken_regions.len() {
                    all_is_good = false;

                    let g = gens.get_mut(broken_region_idx).unwrap();
                    if g.start == 0 {
                        g.start = broken_region_start;
                    }

                    broken_region_idx += 1;
                    let Some(start_idx) = self.find_damaged_region(g.start, g.size) else {
                        break;
                    };

                    g.start = start_idx;

                    broken_region_start = g.start + g.size + 1;

                    all_is_good = true;
                    if broken_region_start >= self.mask.len() {
                        break;
                    }
                }

                if broken_region_idx == self.broken_regions.len() && all_is_good {
                    if rest_is_operational(broken_region_start) {
                        matches += 1;
                    }
                }

                if broken_region_idx == 0 {
                    break;
                }
            }
            let mut updated = false;

            while broken_region_idx > 0 {
                broken_region_idx -= 1;

                let last_g = gens.get_mut(broken_region_idx).unwrap();
                let key = State {
                    start: last_g.start_orig,
                    idx: broken_region_idx,
                };

                if needs_update.contains(&key) {
                    needs_update.remove(&key);

                    let cached = cache.get_mut(&key).unwrap();
                    cached.0 = matches - cached.0;
                    cached.1 = last_g.start;
                }

                if self.mask[last_g.start] == DAMAGED {
                    continue;
                }

                last_g.start += 1;
                last_g.start_orig = last_g.start;

                if last_g.start == self.mask.len() {
                    continue;
                }
                for g in gens[(broken_region_idx + 1)..self.broken_regions.len()].iter_mut() {
                    g.start = 0;
                }
                updated = true;
                break;
            }

            if !updated {
                break;
            }
        }

        println!(
            "{}: end: {:?}, broken_regions: {:?}, matches: {}",
            match_regions_call_idx,
            vec_to_string(&self.mask),
            self.broken_regions,
            matches
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
        self.records
            .iter_mut()
            .enumerate()
            .map(|(idx, r)| r.match_regions(idx))
            .sum()
    }

    pub fn stage2(&mut self) -> usize {
        let n_workers = 8;
        let n_jobs = self.records.len();
        let pool = ThreadPool::new(n_workers);

        let (tx, rx) = channel();
        for (idx, r) in self.records.iter().enumerate() {
            let tx = tx.clone();
            let mut r = r.clone();
            pool.execute(move || {
                let num = r.match_regions(idx);
                tx.send(num).expect("channel not closed");
            });
        }
        rx.iter().take(n_jobs).sum()
    }
    pub fn stage21(&mut self) -> usize {
        self.records
            .iter_mut()
            .enumerate()
            .map(|(idx, r)| r.match_regions(idx))
            .sum()
    }
}
