
fn min_distance(&self, start: &Position, end: &Position) -> usize {
    let y = if start.y > end.y {
        start.y - end.y
    } else {
        end.y - start.y
    };
    let x = if start.x > end.x {
        start.x - end.x
    } else {
        end.x - start.x
    };

    0
}

fn find_path_astar(&self, max_steps: usize) -> usize {
    let mut map = self.map.clone();
    let start = Position::new(0, 0);
    let end = Position::new(self.ysize - 1, self.xsize - 1);
    let mut steps_made: HashMap<Position, Position> = HashMap::new();

    let mut open_list: HashSet<Position> = HashSet::new();
    let start_node = map.get_mut(&start).unwrap();
    start_node.g = 0;
    start_node.f = self.min_distance(&start, &end);

    open_list.insert(start.clone());
    while open_list.len() > 0 {
        let current_pos = open_list
            .iter()
            .min_by(|&b0, &b1| {
                let n0 = map.get(b0).unwrap();
                let n1 = map.get(b1).unwrap();
                let cmp = n0.f.cmp(&n1.f);
                //println!("cmp: {:?} {:?} vs {:?} {:?} -> {:?}", b0, n0, b1, n1, cmp);
                cmp
            })
            .unwrap()
            .clone();

        if current_pos == end {
            break;
        }

        let current_node = map.get(&current_pos).unwrap().clone();
        println!(
            "current_node: {:?} {:?}, end: {:?}, open_list: {}",
            current_pos,
            current_node,
            end,
            open_list.len()
        );
        open_list.remove(&current_pos);

        let mut dst: Vec<Position> = vec![];

        if current_pos.y + 1 < self.ysize {
            dst.push(Position::new(current_pos.y + 1, current_pos.x));
        }
        if current_pos.y >= 1 {
            dst.push(Position::new(current_pos.y - 1, current_pos.x));
        }

        if current_pos.x + 1 < self.xsize {
            dst.push(Position::new(current_pos.y, current_pos.x + 1));
        }
        if current_pos.x >= 1 {
            dst.push(Position::new(current_pos.y, current_pos.x - 1));
        }

        println!("current_pos: {:?}, dst: {:?}", current_pos, dst);

        for d in dst.iter() {
            let neigh = map.get_mut(d).unwrap();
            let tmp_g = current_node.g + neigh.weight;
            println!(
                "current_node: {:?} {:?}, neigh: {:?} {:?}, tmp_g: {}",
                current_pos, current_node, d, neigh, tmp_g
            );
            if tmp_g < neigh.g {
                steps_made.insert(d.clone(), current_pos.clone());
                neigh.g = tmp_g;
                neigh.f = tmp_g + self.min_distance(&d, &end);
                open_list.insert(d.clone());
            }
        }
    }

    let mut path = vec![vec!['.'; self.xsize]; self.ysize];
    let mut path_weight = path.clone();
    for (k, v) in map.iter() {
        path_weight[k.y][k.x] = char::from_digit(v.weight as u32, 10).unwrap();
    }

    let mut weights: Vec<usize> = vec![];
    let mut node = end;
    loop {
        let block = map.get(&node).unwrap();
        weights.push(block.weight);
        path[node.y][node.x] = '#';

        let Some(from) = steps_made.get(&node) else {
            break;
        };

        if from == &start {
            break;
        }

        node = from.clone();
    }

    println!("weights: {:?}", weights);
    for (p, w) in path.iter().zip(path_weight.iter()) {
        let ps = String::from_iter(p.iter());
        let ws = String::from_iter(w.iter());
        println!("{}   {}", ws, ps);
    }
    weights.iter().sum()
}

fn find_path_dijkstra(&self, max_steps: usize) -> usize {
    #[derive(Debug, Clone)]
    struct V {
        weight: usize,
        dist: usize,
    }

    let mut map: HashMap<Position, V> = HashMap::new();
    let start = Position::new(0, 0);
    let end = Position::new(self.ysize - 1, self.xsize - 1);
    let mut steps_made: HashMap<Position, Position> = HashMap::new();

    let mut open_list: HashSet<Position> = HashSet::new();

    for (pos, block) in self.map.iter() {
        map.insert(
            pos.clone(),
            V {
                weight: block.weight,
                dist: 1000000,
            },
        );
        open_list.insert(pos.clone());
    }
    let start_node = map.get_mut(&start).unwrap();
    start_node.dist = 0;

    while open_list.len() > 0 {
        let current_pos = open_list
            .iter()
            .min_by(|&b0, &b1| {
                let n0 = map.get(b0).unwrap();
                let n1 = map.get(b1).unwrap();
                let cmp = n0.dist.cmp(&n1.dist);
                //println!("cmp: {:?} {:?} vs {:?} {:?} -> {:?}", b0, n0, b1, n1, cmp);
                cmp
            })
            .unwrap()
            .clone();

        let current_node = map.get(&current_pos).unwrap().clone();
        println!(
            "current_node: {:?} {:?}, open_list: {}",
            current_pos,
            current_node,
            open_list.len()
        );
        open_list.remove(&current_pos);

        let mut dst: Vec<Position> = vec![];
        if current_pos.y + 1 < self.ysize {
            dst.push(Position::new(current_pos.y + 1, current_pos.x));
        }
        if current_pos.y >= 1 {
            dst.push(Position::new(current_pos.y - 1, current_pos.x));
        }
        if current_pos.x + 1 < self.xsize {
            dst.push(Position::new(current_pos.y, current_pos.x + 1));
        }
        if current_pos.x >= 1 {
            dst.push(Position::new(current_pos.y, current_pos.x - 1));
        }

        println!("neighbours: current_pos: {:?}, dst: {:?}", current_pos, dst);

        for d in dst.iter() {
            if !open_list.contains(&d) {
                continue;
            }

            let neigh = map.get_mut(d).unwrap();
            let alt = current_node.dist + neigh.weight;

            println!(
                "current_node: {:?} {:?}, neigh: {:?} {:?}, alt: {}",
                current_pos, current_node, d, neigh, alt
            );
            if alt < neigh.dist {
                steps_made.insert(d.clone(), current_pos.clone());
                neigh.dist = alt;
            }
        }
    }

    let mut path = vec![vec!['.'; self.xsize]; self.ysize];
    let mut path_weight = path.clone();
    for (k, v) in map.iter() {
        path_weight[k.y][k.x] = char::from_digit(v.weight as u32, 10).unwrap();
    }

    let mut weights: Vec<usize> = vec![];
    let mut node = end;
    loop {
        let block = map.get(&node).unwrap();
        weights.push(block.weight);
        path[node.y][node.x] = '#';

        let Some(from) = steps_made.get(&node) else {
            break;
        };

        if from == &start {
            break;
        }

        node = from.clone();
    }

    println!("weights: {:?}", weights);
    for (p, w) in path.iter().zip(path_weight.iter()) {
        let ps = String::from_iter(p.iter());
        let ws = String::from_iter(w.iter());
        println!("{}   {}", ws, ps);
    }
    weights.iter().sum()
}

fn check_loop(&self, end: &Position, path: &HashMap<Position, Position>) -> bool {
    let mut node = end;
    loop {
        let Some(parent) = path.get(&node) else {
            return false;
        };
        if parent == end {
            return true;
        }

        node = parent;
    }
}
