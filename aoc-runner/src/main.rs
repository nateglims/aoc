use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() {
    println!("Day 1, Part 1: {}", day_one::part1(load_input()));
    println!("Day 1, Part 2: {}", day_one::part2(load_input()));
}

pub fn load_input() -> Vec<String> {
    let input_file = std::env::args().nth(1).expect("no input given");
    let input_path = Path::new(&input_file);

    read_lines(input_path)
        .unwrap()
        .map(|s| s.unwrap())
        .filter(|s| !s.is_empty())
        .collect()
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
