use std::collections::HashSet;
use std::fmt;

use regex::Regex;

use crate::config::Config;
use crate::config::Stage;

#[derive(Clone, PartialEq, Eq, Hash)]
struct Dir {
    y: isize,
    x: isize,
}

impl fmt::Debug for Dir {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.y, self.x)
    }
}

impl Dir {
    fn parse1(line: &str) -> Dir {
        let spl = line.split_whitespace().collect::<Vec<&str>>();
        let steps = spl[1].parse::<isize>().unwrap();
        match spl[0] {
            "R" => Dir { y: 0, x: steps },
            "L" => Dir { y: 0, x: -steps },
            "U" => Dir { y: -steps, x: 0 },
            "D" => Dir { y: steps, x: 0 },
            _ => panic!("unsupported direction {} in {}", spl[0], line),
        }
    }

    fn parse2(line: &str) -> Dir {
        let spl = line.split_whitespace().collect::<Vec<&str>>();
        let re = Regex::new(r"\(#([\dabcdef]{6})\)").unwrap();
        let Some(caps) = re.captures(spl[2]) else {
            panic!("unsupported line {}", line)
        };

        let steps = isize::from_str_radix(&caps[1][0..5], 16).unwrap();
        match caps[1].chars().nth(5).unwrap() {
            '0' => Dir { y: 0, x: steps },
            '2' => Dir { y: 0, x: -steps },
            '3' => Dir { y: -steps, x: 0 },
            '1' => Dir { y: steps, x: 0 },
            _ => panic!("unsupported instruction {} in {}", &caps[1], line),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
struct Line {
    start: Dir,
    end: Dir,
}

impl Line {
    fn new(start: &Dir, step: &Dir) -> Line {
        Line {
            start: start.clone(),
            end: Dir {
                y: start.y + step.y,
                x: start.x + step.x,
            },
        }
    }
    fn max(&self) -> Dir {
        Dir {
            y: std::cmp::max(self.start.y, self.end.y),
            x: std::cmp::max(self.start.x, self.end.x),
        }
    }
    fn min(&self) -> Dir {
        Dir {
            y: std::cmp::min(self.start.y, self.end.y),
            x: std::cmp::min(self.start.x, self.end.x),
        }
    }

    fn horizontal(&self) -> bool {
        self.start.y == self.end.y
    }
    fn vertical(&self) -> bool {
        self.start.x == self.end.x
    }

    fn cross(&self, l: &Line) -> Vec<Dir> {
        if self.horizontal() && l.horizontal() {
            if self.start.y != l.start.y {
                return vec![];
            }

            let max_start = std::cmp::max(self.start.x, l.start.x);
            let min_end = std::cmp::min(self.end.x, l.end.x);
            if max_start > min_end {
                return vec![];
            }

            return vec![
                Dir {
                    x: max_start,
                    y: self.start.y,
                },
                Dir {
                    x: min_end,
                    y: self.start.y,
                },
            ];
        }

        if self.vertical() && l.vertical() {
            if self.start.x != l.start.x {
                return vec![];
            }

            let max_start = std::cmp::max(self.start.y, l.start.y);
            let min_end = std::cmp::min(self.end.y, l.end.y);
            if max_start > min_end {
                return vec![];
            }

            return vec![
                Dir {
                    y: max_start,
                    x: self.start.x,
                },
                Dir {
                    y: min_end,
                    x: self.start.x,
                },
            ];
        }

        let v = if self.vertical() { &self } else { &l };
        let h = if self.horizontal() { &self } else { &l };

        let vminy = std::cmp::min(v.start.y, v.end.y);
        let vmaxy = std::cmp::max(v.start.y, v.end.y);
        let vx = v.start.x;

        let hminx = std::cmp::min(h.start.x, h.end.x);
        let hmaxx = std::cmp::max(h.start.x, h.end.x);
        let hy = h.start.y;

        if hminx <= vx && vx <= hmaxx && vminy <= hy && hy <= vmaxy {
            return vec![Dir { y: hy, x: vx }];
        }

        vec![]
    }
}

impl fmt::Debug for Line {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}-{:?}", self.start, self.end)
    }
}

pub struct Solution {
    map: Vec<Line>,
    ysize: isize,
    xsize: isize,
}

impl Solution {
    pub fn build(config: &Config) -> Solution {
        let mut start = Dir { y: 0, x: 0 };
        let mut map: Vec<Line> = vec![];
        for line in config.content.lines() {
            let step = if config.stage == Stage::ONE {
                Dir::parse1(line)
            } else {
                Dir::parse2(line)
            };
            let new_start = Dir {
                y: start.y + step.y,
                x: start.x + step.x,
            };

            let l = Line::new(&start.clone(), &step);
            map.push(l);
            start = new_start;
        }

        let xmin = map
            .iter()
            .min_by_key(|l| std::cmp::min(l.start.x, l.end.x))
            .unwrap()
            .min()
            .x;
        let ymin = map
            .iter()
            .min_by_key(|l| std::cmp::min(l.start.y, l.end.y))
            .unwrap()
            .min()
            .y;

        let mut sol = Solution {
            xsize: 0,
            ysize: 0,
            map: map
                .iter()
                .map(|line_orig| {
                    let mut line = Line {
                        start: line_orig.min(),
                        end: line_orig.max(),
                    };

                    line.start.x -= xmin;
                    line.start.y -= ymin;
                    line.end.x -= xmin;
                    line.end.y -= ymin;

                    line
                })
                .collect(),
        };

        sol.xsize = sol.map.iter().max_by_key(|l| l.end.x).unwrap().max().x + 1;
        sol.ysize = sol.map.iter().max_by_key(|l| l.end.y).unwrap().max().y + 1;

        println!("ysize: {}, xsize: {}", sol.ysize, sol.xsize);

        sol
    }

    fn count_cross(&self) -> usize {
        let mut count = 0;

        for ypos in 0..self.ysize {
            let hor = Line {
                start: Dir { y: ypos, x: 0 },
                end: Dir {
                    y: ypos,
                    x: self.xsize - 1,
                },
            };

            let mut ranges: Vec<(Dir, &Line)> = vec![];
            let mut true_ranges: HashSet<Vec<Dir>> = HashSet::new();

            for line in self.map.iter() {
                let cross = line.cross(&hor);
                if cross.len() == 0 {
                    continue;
                }

                if cross.len() == 1 {
                    ranges.push((cross[0].clone(), line));
                } else {
                    true_ranges.insert(cross.clone());
                }
            }

            ranges.sort_by_key(|d| d.0.x);

            let mut minus_one = false;
            let mut wn = 0;

            for i in 0..(ranges.len() - 1) {
                let (prev_point, prev_line) = ranges[i].clone();
                let (cur_point, _cur_line) = ranges[i + 1].clone();

                let true_ranges_key = vec![prev_point.clone(), cur_point.clone()];
                let on_the_edge = true_ranges.contains(&true_ranges_key);

                let line_status = |line: &Line| {
                    let line_up = line.max().y == hor.start.y;
                    let line_down = line.min().y == hor.start.y;
                    let line_vert = line.start.y != hor.start.y && line.end.y != hor.end.y;
                    (line_up, line_down, line_vert)
                };

                let (_cur_line_up, cur_line_down, _cur_line_vert) = line_status(prev_line);

                if !cur_line_down {
                    wn += 1;
                }

                let skip = (wn % 2 == 0) && !on_the_edge;

                if !skip {
                    let c = cur_point.x - prev_point.x + 1;
                    count += c;

                    if minus_one {
                        count -= 1;
                    }

                    minus_one = true;
                } else {
                    minus_one = false;
                }
            }
        }

        count as usize
    }

    pub fn stage1(&self) -> usize {
        self.count_cross()
    }

    pub fn stage2(&self) -> usize {
        self.stage1()
    }
}
