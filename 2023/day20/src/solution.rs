use std::collections::BTreeMap;
use std::collections::HashMap;

use itertools::Itertools;

use crate::config::Config;
use crate::config::Stage;

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord)]
enum Pulse {
    LOW,
    HIGH,
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum State {
    ON,
    OFF,
}

trait Module {
    fn name(&self) -> String;
    fn dst(&self) -> Vec<String>;
    fn set_inputs(&mut self, _inputs: &Vec<String>) {}
    fn process(&mut self, inputs: BTreeMap<String, Pulse>) -> BTreeMap<String, Pulse>;
    fn received(&self) -> BTreeMap<Pulse, usize> {
        BTreeMap::new()
    }
}

struct FlipFlop {
    name: String,
    dst: Vec<String>,

    state: State,
}

impl FlipFlop {
    fn new(name: String, dst: Vec<String>) -> FlipFlop {
        FlipFlop {
            name,
            dst,
            state: State::OFF,
        }
    }
}

impl Module for FlipFlop {
    fn name(&self) -> String {
        self.name.clone()
    }
    fn dst(&self) -> Vec<String> {
        self.dst.clone()
    }
    fn process(&mut self, input: BTreeMap<String, Pulse>) -> BTreeMap<String, Pulse> {
        if input.len() != 1 {
            panic!("flipflop module received multiple inputs {}", input.len());
        }

        let input = input.values().nth(0).unwrap();
        if input == &Pulse::HIGH {
            return BTreeMap::new();
        }

        let ret_pulse = if self.state == State::OFF {
            self.state = State::ON;
            Pulse::HIGH
        } else {
            self.state = State::OFF;
            Pulse::LOW
        };

        self.dst
            .iter()
            .map(|name| (name.clone(), ret_pulse.clone()))
            .collect()
    }
}

struct Conjunction {
    name: String,
    dst: Vec<String>,

    inputs: Vec<String>,
    num_inputs: usize,
    remembers: Pulse,
    last_inputs: BTreeMap<String, Pulse>,
}

impl Conjunction {
    fn new(name: String, dst: Vec<String>) -> Conjunction {
        Conjunction {
            name,
            dst,
            remembers: Pulse::LOW,
            inputs: vec![],
            num_inputs: 0,
            last_inputs: BTreeMap::new(),
        }
    }
}

impl Module for Conjunction {
    fn name(&self) -> String {
        self.name.clone()
    }
    fn dst(&self) -> Vec<String> {
        self.dst.clone()
    }
    fn process(&mut self, input: BTreeMap<String, Pulse>) -> BTreeMap<String, Pulse> {
        let _ = input
            .clone()
            .into_iter()
            .map(|(key, value)| {
                self.last_inputs.insert(key, value);
            })
            .collect::<Vec<_>>();

        let all_high = self.last_inputs.values().all(|m| m == &Pulse::HIGH);
        let ret_pulse = if all_high { Pulse::LOW } else { Pulse::HIGH };

        self.dst
            .iter()
            .map(|name| (name.clone(), ret_pulse.clone()))
            .collect()
    }
    fn set_inputs(&mut self, inputs: &Vec<String>) {
        self.num_inputs = inputs.len();
        self.inputs = inputs.clone();
        self.inputs
            .iter()
            .map(|input_name| {
                self.last_inputs
                    .insert(input_name.clone(), self.remembers.clone())
            })
            .count();
    }
}

struct Broadcaster {
    name: String,
    dst: Vec<String>,
}
impl Broadcaster {
    fn new(name: String, dst: Vec<String>) -> Broadcaster {
        Broadcaster { name, dst }
    }
}

impl Module for Broadcaster {
    fn name(&self) -> String {
        self.name.clone()
    }
    fn dst(&self) -> Vec<String> {
        self.dst.clone()
    }
    fn process(&mut self, input: BTreeMap<String, Pulse>) -> BTreeMap<String, Pulse> {
        if input.len() != 1 {
            panic!(
                "broadcaster module received multiple inputs {}",
                input.len()
            );
        }

        let input = input.values().nth(0).unwrap();
        if input == &Pulse::HIGH {
            return BTreeMap::new();
        }

        self.dst
            .iter()
            .map(|name| (name.clone(), input.clone()))
            .collect()
    }
}

struct Button {
    name: String,
    dst: Vec<String>,
}
impl Button {
    fn new(name: String, dst: Vec<String>) -> Button {
        Button { name, dst }
    }
}

impl Module for Button {
    fn name(&self) -> String {
        self.name.clone()
    }
    fn dst(&self) -> Vec<String> {
        self.dst.clone()
    }
    fn process(&mut self, _input: BTreeMap<String, Pulse>) -> BTreeMap<String, Pulse> {
        self.dst
            .iter()
            .map(|name| (name.clone(), Pulse::LOW))
            .collect()
    }
}

struct Output {
    name: String,
    dst: Vec<String>,
    received: BTreeMap<Pulse, usize>,
}
impl Output {
    fn new(name: String, _dst: Vec<String>) -> Output {
        Output {
            name,
            dst: vec![],
            received: BTreeMap::new(),
        }
    }
}

impl Module for Output {
    fn name(&self) -> String {
        self.name.clone()
    }
    fn dst(&self) -> Vec<String> {
        self.dst.clone()
    }
    fn process(&mut self, input: BTreeMap<String, Pulse>) -> BTreeMap<String, Pulse> {
        for pulse in input.values() {
            self.received
                .entry(pulse.clone())
                .and_modify(|ent| *ent += 1);
        }
        BTreeMap::new()
    }
    fn received(&self) -> BTreeMap<Pulse, usize> {
        self.received.clone()
    }
}

pub struct Solution {
    modules: BTreeMap<String, Box<dyn Module>>,
}

impl Solution {
    fn new_module(name: String, outputs: Vec<String>) -> Box<dyn Module> {
        if name.starts_with("%") {
            let name = &name[1..name.len()];

            return Box::new(FlipFlop::new(name.to_string(), outputs)) as Box<dyn Module>;
        }
        if name.starts_with("&") {
            let name = &name[1..name.len()];

            return Box::new(Conjunction::new(name.to_string(), outputs)) as Box<dyn Module>;
        }
        if name == "broadcaster" {
            return Box::new(Broadcaster::new(name, outputs)) as Box<dyn Module>;
        }
        if name == "output" {
            return Box::new(Output::new(name, outputs)) as Box<dyn Module>;
        }

        unreachable!()
    }

    pub fn build(config: &Config) -> Solution {
        let mut sol = Solution {
            modules: config
                .content
                .lines()
                .map(|line| {
                    let spl = line.split("->").collect::<Vec<&str>>();

                    let name = spl[0].trim();
                    let outputs = spl[1]
                        .trim()
                        .split(",")
                        .map(|dst| dst.trim().to_string())
                        .collect::<Vec<String>>();

                    Solution::new_module(name.to_string(), outputs)
                })
                .map(|m| (m.name().clone(), m))
                .collect(),
        };

        sol.modules.insert(
            "button".to_string(),
            Box::new(Button::new(
                "button".to_string(),
                vec!["broadcaster".to_string()],
            )),
        );

        let mut all_inputs: BTreeMap<String, Vec<String>> = BTreeMap::new();
        let _ = sol
            .modules
            .values()
            .map(|m| {
                for dst in m.dst().iter() {
                    all_inputs
                        .entry(dst.to_string())
                        .or_insert(vec![])
                        .push(m.name());
                }
            })
            .collect::<Vec<_>>();

        let _ = all_inputs
            .iter()
            .map(|(name, inputs)| {
                sol.modules
                    .entry(name.to_string())
                    .or_insert(Box::new(Output::new(name.to_string(), vec![])))
                    .set_inputs(inputs)
            })
            .collect::<Vec<_>>();
        sol
    }

    pub fn stage1(&mut self) -> usize {
        let mut impulse_counts: BTreeMap<Pulse, usize> = BTreeMap::new();
        let mut count_impulses = |impulses: &Vec<(String, BTreeMap<String, Pulse>)>| {
            let _ = impulses
                .iter()
                .map(|(src_name, pulses)| {
                    pulses
                        .values()
                        .map(|imp| *impulse_counts.entry(imp.clone()).or_insert(0) += 1)
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>();
        };

        loop {
            let button = self.modules.get_mut("button").unwrap();
            let mut impulses: Vec<(String, BTreeMap<String, Pulse>)> =
                vec![(button.name().clone(), button.process(BTreeMap::new()))];
            count_impulses(&impulses);

            while impulses.len() != 0 {
                //println!("{}: impulses: {:?}", i, impulses);
                let mut next_inputs: Vec<(String, BTreeMap<String, Pulse>)> = vec![];

                for (src_name, module_output_impulse) in impulses.iter() {
                    for (name, pulse) in module_output_impulse.iter() {
                        let module = self.modules.get_mut(name).unwrap();
                        let module_input: BTreeMap<String, Pulse> =
                            BTreeMap::from([(src_name.clone(), pulse.clone())]);
                        let outputs = module.process(module_input.clone());
                        //println!("{}: {:?} -> {:?}", name, module_input, outputs);
                        next_inputs.push((name.clone(), outputs));
                    }
                }

                count_impulses(&next_inputs);
                impulses = next_inputs;
            }

            let rx = self.modules.get("rx").unwrap();
            let received = rx.received();
            if received.len() != 0 {
                println!("rx: {:?}", received);
                break;
            }
        }

        impulse_counts[&Pulse::LOW] * impulse_counts[&Pulse::HIGH]
    }

    pub fn stage2(&mut self) -> usize {
        0
    }
}
