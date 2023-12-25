use std::collections::HashMap;
use std::collections::HashSet;

use crate::config::Config;
use crate::config::Stage;

struct Node {
    connects: HashSet<String>,
}

impl Node {
    fn new() -> Node {
        Node {
            connects: HashSet::new(),
        }
    }
}

pub struct Solution {
    nodes: HashMap<String, Node>,
}

fn make_pair(a: &str, b: &str) -> (String, String) {
    if a < b {
        (a.to_string(), b.to_string())
    } else {
        (b.to_string(), a.to_string())
    }
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let mut sol = Solution {
            nodes: HashMap::new(),
        };

        for line in config.content.lines() {
            let spl = line.split(":").collect::<Vec<&str>>();
            let node_id = spl[0].trim().to_string();
            let ids = spl[1]
                .split_whitespace()
                .map(|n| n.trim().to_string())
                .collect::<HashSet<String>>();

            let main_ent = sol.nodes.entry(node_id.clone()).or_insert(Node::new());
            for id in ids.iter() {
                main_ent.connects.insert(id.to_string());
            }

            for id in ids.iter() {
                let ent = sol.nodes.entry(id.to_string()).or_insert(Node::new());
                ent.connects.insert(node_id.clone());
            }
        }

        sol
    }

    fn reaches(&self, start: &str, breaks: &HashSet<(String, String)>) -> HashSet<String> {
        let mut reaches: HashSet<String> = HashSet::new();
        let mut start_nodes: HashSet<&str> = HashSet::from_iter([start]);

        while start_nodes.len() != 0 {
            let mut new_nodes: HashSet<&str> = HashSet::new();

            for node_id in start_nodes.into_iter() {
                if reaches.contains(node_id) {
                    continue;
                }

                for other_id in self.nodes.get(node_id).unwrap().connects.iter() {
                    let pair = make_pair(node_id, other_id);
                    if !breaks.contains(&pair) {
                        reaches.insert(node_id.to_owned());
                        new_nodes.insert(other_id);
                    }
                }
            }

            start_nodes = new_nodes;
        }

        reaches
    }

    fn shortest_path(&self, a: &str, b: &str) -> Vec<String> {
        let mut steps_made: HashMap<&str, &str> = HashMap::new();
        let mut start_nodes: Vec<&str> = vec![a];

        while start_nodes.len() != 0 {
            let mut new_nodes: Vec<&str> = vec![];

            for start in start_nodes.iter() {
                for dst in self.nodes.get(start as &str).unwrap().connects.iter() {
                    if !steps_made.contains_key(dst as &str) {
                        steps_made.insert(dst as &str, start as &str);
                        new_nodes.push(dst as &str);
                    }

                    if dst == b {
                        let mut path: Vec<String> = vec![dst.clone()];

                        let mut dst = dst as &str;
                        while dst != a {
                            let Some(src) = steps_made.get(dst as &str) else {
                                break;
                            };

                            path.push(src.to_string());
                            dst = src;
                        }

                        return path;
                    }
                }
            }

            start_nodes = new_nodes;
        }
        vec![]
    }

    pub fn stage1(&self) -> usize {
        let mut nodes: HashMap<(String, String), usize> = HashMap::new();
        let mut breaks: HashSet<(String, String)> = HashSet::new();
        let mut checked: HashSet<(String, String)> = HashSet::new();

        for (node_id, node) in self.nodes.iter() {
            for (other_id, other) in self.nodes.iter() {
                if node_id == other_id {
                    continue;
                }

                let pair = make_pair(node_id, other_id);
                if checked.contains(&pair) {
                    continue;
                }

                checked.insert(pair);

                // all paths calculated here can be cached
                let path = self.shortest_path(node_id, other_id);
                let mut prev: &String = &path[0];
                for n in path.iter() {
                    if n != prev {
                        let pair = make_pair(prev, n);
                        let ent = nodes.entry(pair).or_insert(0);
                        *ent += 1;
                        prev = n;
                    }
                }
            }
        }

        let mut nodes = Vec::from_iter(nodes.iter());
        nodes.sort_by_key(|n| usize::MAX - n.1);
        for i in 0..3 {
            breaks.insert(nodes[i].0.clone());
        }

        let nodes_to_check = self
            .nodes
            .keys()
            .map(|k| k.to_owned())
            .collect::<Vec<String>>();

        let start_node = &nodes_to_check[0];
        let group_a: HashSet<String> = self.reaches(start_node, &breaks);
        group_a.len() * (self.nodes.len() - group_a.len())
    }

    pub fn stage2(&self) -> usize {
        0
    }
}
