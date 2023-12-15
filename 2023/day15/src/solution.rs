use std::collections::HashMap;

use crate::config::Config;
use crate::config::Stage;

fn hash(line: &str) -> usize {
    line.chars()
        .fold(0, |acc, c| ((acc + (c as usize)) * 17) % 256)
}

struct Lens {
    focal_length: usize,
    pos: usize,
}

impl Lens {
    fn new(focal_length: usize, pos: usize) -> Lens {
        Lens { focal_length, pos }
    }
}

struct Box {
    id: usize,
    lenses: HashMap<String, Lens>,
}

impl Box {
    fn new(id: usize) -> Box {
        Box {
            id,
            lenses: HashMap::new(),
        }
    }

    fn insert(&mut self, label: &str, focal_length: usize) {
        let pos = self.lenses.len();
        self.lenses
            .entry(label.to_string())
            .or_insert(Lens::new(focal_length, pos))
            .focal_length = focal_length;
    }
    fn remove(&mut self, label: &str) {
        if let Some(old) = self.lenses.remove(&label.to_string()) {
            for l in self.lenses.values_mut() {
                if l.pos > old.pos {
                    l.pos -= 1;
                }
            }
        }
    }

    fn print(&self) {
        println!("Box {}", self.id);
        for (label, lens) in self.lenses.iter() {
            println!("  {}: f:{}, pos: {}", label, lens.focal_length, lens.pos);
        }
    }

    fn focus_power(&self) -> usize {
        (self.id + 1)
            * self
                .lenses
                .values()
                .fold(0, |acc, l| acc + (l.pos + 1) * l.focal_length)
    }
}

pub struct Solution {
    steps: Vec<String>,
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let sol = Solution {
            steps: config
                .content
                .trim()
                .split(",")
                .map(|s| s.to_string())
                .collect::<Vec<String>>(),
        };

        sol
    }

    pub fn stage1(&self) -> usize {
        self.steps.iter().map(|s| hash(&s)).sum()
    }

    pub fn stage2(&self) -> usize {
        let mut boxes: HashMap<usize, Box> = HashMap::new();

        for step in self.steps.iter() {
            if step.contains("=") {
                let spl = step.split("=").collect::<Vec<&str>>();
                let label = spl[0];
                let box_id = hash(label);
                let focal_length = spl[1].parse::<usize>().unwrap();
                boxes
                    .entry(box_id)
                    .or_insert(Box::new(box_id))
                    .insert(label, focal_length);
            } else if step.contains("-") {
                let spl = step.split("-").collect::<Vec<&str>>();
                let label = spl[0];
                let box_id = hash(label);

                boxes
                    .entry(box_id)
                    .or_insert(Box::new(box_id))
                    .remove(label);
            }
        }

        boxes.values().map(|b| b.focus_power()).sum::<usize>()
    }
}
