use std::collections::HashMap;
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

#[derive(Clone)]
struct Generator {
    pos: usize,
    index: usize,
}
impl Generator {
    fn new(pos: usize) -> Generator {
        Generator { pos, index: 0 }
    }

    fn get(&self) -> Option<RecordType> {
        match self.index {
            0 => Some(OPERATIONAL),
            1 => Some(DAMAGED),
            _ => None,
        }
    }

    fn update(&mut self) {
        self.index += 1;
    }

    fn done(&self) -> bool {
        self.index >= 2
    }

    fn reset(&mut self) {
        self.index = 0;
    }
}

#[derive(Clone)]
struct Record {
    mask: Vec<RecordType>,
    broken_regions: Vec<usize>,

    gens: Vec<Generator>,
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
        let gens = unknown_positions
            .iter()
            .map(|&pos| Generator::new(pos))
            .collect::<Vec<Generator>>();

        Record {
            mask,
            broken_regions,
            gens,
        }
    }

    fn current_region_matches(&self, damaged_regions: &[usize], precise: bool, full: bool) -> bool {
        if damaged_regions.len() == 0 {
            return true;
        }
        if damaged_regions.len() > self.broken_regions.len() {
            return false;
        }

        if precise {
            if full {
                if damaged_regions.len() != self.broken_regions.len() {
                    return false;
                }
            }

            damaged_regions.last() == self.broken_regions.get(damaged_regions.len() - 1)
        } else {
            damaged_regions.last() <= self.broken_regions.get(damaged_regions.len() - 1)
        }
    }

    fn match_regions(&mut self) -> usize {
        let mut matches = 0;

        while !self.gens[0].done() {
            let mut var = self.mask.clone();
            let mut gidx = 0;
            let mut damaged_regions: Vec<usize> = vec![];
            let mut damaged_regions_cache: HashMap<usize, Vec<usize>> = HashMap::new();
            let mut mask_idx = 0;

            while mask_idx < self.mask.len() && !self.gens[0].done() {
                loop {
                    let mask_value = if self.mask[mask_idx] == UNKNOWN {
                        let Some(value) = self.gens[gidx].get() else {
                            break;
                        };
                        gidx += 1;
                        value
                    } else {
                        self.mask[mask_idx]
                    };

                    if mask_value == DAMAGED {
                        if (mask_idx == 0) || (var[mask_idx - 1] != DAMAGED) {
                            damaged_regions.push(0);
                        }

                        *damaged_regions.last_mut().unwrap() += 1;

                        if damaged_regions.len() > self.broken_regions.len() {
                            break;
                        }

                        let required = self.broken_regions[damaged_regions.len() - 1];
                        if damaged_regions.last().unwrap() == &required {
                            break;
                        }
                    }

                    var[mask_idx] = mask_value;

                    damaged_regions_cache.insert(mask_idx, damaged_regions.clone());
                }

                let precise_check = (mask_value != DAMAGED) || (mask_idx == self.mask.len() - 1);
                let region_matches = self.current_region_matches(
                    &damaged_regions,
                    precise_check,
                    mask_idx == self.mask.len() - 1,
                );

                // if region_matches && mask_idx == self.mask.len() - 1 {
                //     println!(
                //         "mask: {:?}, var: {:?}: regions: {:?}/{:?}",
                //         vec_to_string(&self.mask[0..=mask_idx]),
                //         vec_to_string(&var[0..=mask_idx]),
                //         &damaged_regions,
                //         &self.broken_regions,
                //     );
                // }

                println!(
                    "mask: {:?}, var: {:?}: mask_idx: {}/{}, regions: {:?}/{:?}, precise_check: {}, region_matches: {}, full: {}",
                    vec_to_string(&self.mask[0..=mask_idx]),
                    vec_to_string(&var[0..=mask_idx]),
                    mask_idx, self.mask.len(),
                    &damaged_regions,
                    &self.broken_regions,
                    precise_check,
                    region_matches,
                    region_matches && (mask_idx == self.mask.len() - 1)
                );

                if region_matches {
                    if mask_idx == self.mask.len() - 1 {
                        matches += 1;
                    }
                }

                mask_idx += 1;

                if !region_matches || mask_idx == self.mask.len() {
                    gidx -= 1;

                    loop {
                        let g = self.gens.get_mut(gidx).unwrap();
                        mask_idx = g.pos;
                        g.update();
                        if !g.done() || gidx == 0 {
                            break;
                        }
                        g.reset();
                        gidx -= 1;
                    }

                    damaged_regions = damaged_regions_cache.get(&mask_idx).unwrap().clone();
                }
            }
        }

        println!(
            "region done: {:?} {:?} -> {}, start_operational: {}, last_operational: {}",
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
