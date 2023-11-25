use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::error::Error;
use std::fs;
use std::process;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
enum Stage {
    ONE,
    TWO,
}

impl Stage {
    fn parse(arg: &str) -> Stage {
        let stage_int = arg.parse::<i32>().unwrap();
        if stage_int == 1 {
            return Stage::ONE;
        } else if stage_int == 2 {
            return Stage::TWO;
        } else {
            panic!("Invalid stage {stage_int}");
        }

    }
}

#[derive(Debug)]
struct Config {
    stage: Stage,
    content: String,
}

impl Config {
    fn build(args: &[String]) -> Result<Config, Box<dyn Error>> {
        if args.len() < 2 {
            return Err("not enough arguments, input file and stage are required".into());
        }

        let mut content: String = "".to_string();

        if args.len() == 3 {
            let file_path = args[2].clone();

            content = fs::read_to_string(&file_path).map_err(|err| {
                format!("could not read file \"{file_path}\": {err}")
            })?;
        }

        Ok(Config{
            stage: Stage::parse(&args[1]),
            content: content,
        })
    }
}

#[derive(Debug)]
struct SpaceObject {
    name: String,
    orbits: RefCell<Option<Rc<SpaceObject>>>,
    planets: RefCell<Vec<Rc<SpaceObject>>>,
}

impl SpaceObject {
    fn new(name: &str) -> SpaceObject {
        SpaceObject {
            name: name.to_string(),
            orbits: RefCell::new(None),
            planets: RefCell::new(vec![]),
        }
    }
}

struct Solution {
    planets: HashMap<String, Rc<SpaceObject>>,
}

impl Solution {
    fn build(config: &Config) -> Solution {
        let mut sol = Solution {
            planets: HashMap::new(),
        };

        for line in config.content.lines() {
            let ids: Vec<&str> = line.split(")").collect();
            if ids.len() != 2 {
                panic!("Invalid input line \"{}\"", line);
            }

            sol.planets.entry(ids[0].to_string()).or_insert(Rc::new(SpaceObject::new(ids[0])));
            sol.planets.entry(ids[1].to_string()).or_insert(Rc::new(SpaceObject::new(ids[1])));

            let base = sol.planets.get(ids[0]).unwrap();
            let planet = sol.planets.get(ids[1]).unwrap();

            base.planets.borrow_mut().push(Rc::clone(planet));
            *planet.orbits.borrow_mut() = Some(Rc::clone(base));

            //println!("planet: {}, base: {:#?}", &planet.name, (*planet.orbits.borrow()).upgrade());
        }

        sol
    }

    fn count_orbits(&self) -> usize {
        let mut orbits = 0;
        for (_, planet) in self.planets.iter() {
            let local_orbits = self.get_orbits(&planet.name);
            orbits += local_orbits.len();
        }

        orbits
    }


    fn get_orbits(&self, start: &str) -> Vec<String> {
        let mut orbits: Vec<String> = vec![];

        let planet = self.planets.get(start).unwrap();
        let mut planet = planet.clone();

        loop {
            if planet.orbits.borrow().is_none() {
                break
            }

            orbits.push(planet.name.to_string());
            planet = planet.orbits.clone().into_inner().unwrap().clone();
        }

        orbits.reverse();
        orbits
    }

    fn count_orbital_transfers(&self) -> usize {
        let my_orbits = self.get_orbits("YOU");
        let santa_orbits = self.get_orbits("SAN");

        for (i, (mo, so)) in my_orbits.iter().zip(santa_orbits.iter()).enumerate() {
            if mo != so {
                let my_transfers = my_orbits.len() - i - 1;
                let santa_transfers = santa_orbits.len() - i - 1;

                return my_transfers + santa_transfers;
            }
        }
        0
    }
}

fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let sol = Solution::build(&config);

    if config.stage == Stage::ONE {
        println!("stage: {:?}, orbits: {}", config.stage, sol.count_orbits());
    } else {
        println!("stage: {:?}, transfers: {}", config.stage, sol.count_orbital_transfers());
    }
    Ok(())
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let config = Config::build(&args).unwrap_or_else(|err| {
        println!("Problem parsing arguments: {err}");
        process::exit(-1);
    });

    if let Err(e) = run(config) {
        println!("Application error: {e}");
        process::exit(-1);
    }
}
