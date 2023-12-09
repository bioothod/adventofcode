use crate::config::Config;

struct History {
    values: Vec<isize>,
}

impl History {
    fn new(line: &str) -> History {
        History {
            values: line.split_whitespace().map(|v| v.parse::<isize>().unwrap()).collect(),
        }
    }

    fn extrapolate(&self) -> (isize, isize) {
        let mut new_values: Vec<isize> = self.values.clone();
        let mut last_values: Vec<isize> = vec![*self.values.last().unwrap()];
        let mut first_values: Vec<isize> = vec![*self.values.first().unwrap()];
        for i in 0..self.values.len() {
            new_values = (&new_values).windows(2).map(|v| v[1] - v[0]).collect();
            last_values.push(*new_values.last().unwrap());
            let sign = if i % 2 == 0 { -1 } else { 1 };
            first_values.push(sign*new_values.first().unwrap());

            if new_values.iter().all(|&v| v == 0) {
                break
            }
        }

        let sum_first = first_values.iter().sum::<isize>();
        let sum_last = last_values.iter().sum::<isize>();
        (sum_first, sum_last)
    }
}

pub struct Solution {
    all_hist: Vec<History>,
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let sol = Solution {
            all_hist:  config.content.lines().map(|line| History::new(line)).collect(),
        };

        sol
    }

    pub fn stage1(&self) -> isize {
        self.all_hist.iter().map(|hist| hist.extrapolate().1).sum()
    }

    pub fn stage2(&self) -> isize {
        self.all_hist.iter().map(|hist| hist.extrapolate().0).sum()
    }
}
