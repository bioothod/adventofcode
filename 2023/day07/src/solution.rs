use std::collections::HashMap;
use std::cmp::Ordering;

use crate::config::Config;
use crate::permutations::*;

#[derive(Debug, Clone)]
struct Hand {
    name: String,
    id: String,
    strength: usize,
    bid: usize,
}

#[derive(Debug)]
struct Counts {
    counts: HashMap<char, usize>,
}


impl Counts {
    fn new(line: &str) -> Counts {
        let mut counts: HashMap<char, usize> = HashMap::new();
        for c in line.chars() {
            *counts.entry(c).or_insert(0) += 1;
        }

        Counts {
            counts: counts,
        }
    }

    fn len(&self) -> usize {
        self.counts.len()
    }

    fn sorted_ints(&self) -> Vec<usize> {
        let mut count_ints: Vec<usize> = self.counts.values().cloned().collect();
        count_ints.sort_by_key(|&w| std::cmp::Reverse(w));

        count_ints
    }
}

impl Hand {
    fn try_five_of_a_kind(line: &str) -> Option<Hand> {
        let counts = Counts::new(line);
        if counts.len() == 1 {
            return Some(Hand {
                name: "five of a kind".to_string(),
                id: line.to_owned(),
                strength: 7,
                bid: 0,
            });
        }

        None
    }

    fn try_four_of_a_kind(line: &str) -> Option<Hand> {
        let counts = Counts::new(line);

        if counts.sorted_ints() == [4, 1] {
            return Some(Hand {
                name: "four of a kind".to_string(),
                id: line.to_owned(),
                strength: 6,
                bid: 0,
            });
        }

        None
    }

    fn try_full_house(line: &str) -> Option<Hand> {
        let counts = Counts::new(line);

        if counts.sorted_ints() == &[3, 2] {
            return Some(Hand {
                name: "full house".to_string(),
                id: line.to_owned(),
                strength: 5,
                bid: 0,
            });
        }

        None
    }

    fn try_three_of_a_kind(line: &str) -> Option<Hand> {
        let counts = Counts::new(line);

        if counts.sorted_ints() == &[3, 1, 1] {
            return Some(Hand {
                name: "three of a kind".to_string(),
                id: line.to_owned(),
                strength: 4,
                bid: 0,
            });
        }

        None
    }

    fn try_two_pair(line: &str) -> Option<Hand> {
        let counts = Counts::new(line);

        if counts.sorted_ints() == &[2, 2, 1] {
            return Some(Hand {
                name: "two pair".to_string(),
                id: line.to_owned(),
                strength: 3,
                bid: 0,
           });
        }

        None
    }

    fn try_one_pair(line: &str) -> Option<Hand> {
        let counts = Counts::new(line);

        if counts.sorted_ints() == &[2, 1, 1, 1] {
            return Some(Hand {
                name: "one pair".to_string(),
                id: line.to_owned(),
                strength: 2,
                bid: 0,
            });
        }

        None
    }

    fn try_high_card(line: &str) -> Option<Hand> {
        let counts = Counts::new(line);

        if counts.len() == 5 {
            return Some(Hand {
                name: "high card".to_string(),
                id: line.to_owned(),
                strength: 1,
                bid: 0,
            });
        }

        None
    }

    fn parse(line: &str) -> Hand {
        let hand_options = vec![
            Hand::try_five_of_a_kind,
            Hand::try_four_of_a_kind,
            Hand::try_full_house,
            Hand::try_three_of_a_kind,
            Hand::try_two_pair,
            Hand::try_one_pair,
            Hand::try_high_card,
        ];

        for option in hand_options.iter() {
            if let Some(hand) = option(line) {
                return hand;
            }
        }
        panic!("unsupported card description {}", line);
    }

    fn try_upgrade(&mut self) {
        if !self.id.contains('J') {
            return
        }
        let counts = Counts::new(&self.id);
        let num_jokers = counts.counts.get(&'J').unwrap();
        let joker_indexes = self.id.match_indices('J');
        let mut max_hand = self.clone();
        let original_id = self.id.clone();
        let original_bid = self.bid;

        let cartesian = STRENGTHS[0..(STRENGTHS.len()-1)].iter().permutations_with_replacement(*num_jokers);
        for possible in cartesian {
            let mut new_str: Vec<char> = self.id.chars().collect();

            for (possible_idx, (idx, _)) in joker_indexes.clone().enumerate() {
                new_str[idx] = *possible[possible_idx];
            }

            let new_str = String::from_iter(new_str);
            let new_hand = Hand::parse(&new_str);
            let is_better = new_hand > max_hand;

            if is_better {
                max_hand = new_hand.clone();
            }
        }

        if &max_hand > self {
            *self = max_hand;

            self.id = original_id;
            self.bid = original_bid;
        }
    }
}

const STRENGTHS: [char; 14] = ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J'];

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}
impl Eq for Hand {}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.strength != other.strength {
            return self.strength.partial_cmp(&other.strength);
        }

        for (s, o) in self.id.chars().zip(other.id.chars()) {
            if s == o {
                continue
            }

            let self_strength = STRENGTHS.iter().rev().position(|x| *x == s).unwrap();
            let other_strength = STRENGTHS.iter().rev().position(|x| *x == o).unwrap();

            return self_strength.partial_cmp(&other_strength);
        }

        Some(Ordering::Equal)
    }
}

impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
        self.id  == other.id
    }
}

pub struct Solution {
    list_of_hands: Vec<Hand>,
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let mut sol = Solution {
            list_of_hands: vec![],
        };

        for line in config.content.lines() {
            let spl: Vec<_> = line.split_whitespace().collect();
            let desc = spl.first().unwrap();
            let bid = spl.last().unwrap().parse::<usize>().unwrap();

            let mut hand = Hand::parse(desc);
            hand.bid = bid;
            sol.list_of_hands.push(hand);
        }

        sol
    }

    pub fn stage1(&self) -> usize {
        let mut sorted_hands = self.list_of_hands.clone();
        sorted_hands.sort();

        let mut ret = 0;
        for (i, hand) in sorted_hands.iter().enumerate() {
            let rank = i + 1;

            ret += rank * hand.bid;
        }

        ret
    }

    pub fn stage2(&mut self) -> usize {
        for hand in self.list_of_hands.iter_mut() {
            hand.try_upgrade();
        }

        self.stage1()
    }
}
