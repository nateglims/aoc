// Part 1
pub fn part1(input: Vec<String>) -> i64 {
    let mut sum = 0;
    for line in input {
        let s: Vec<&str> = line.split(":").collect();

        if s[1].split(';').all(eval_cube_set) {
            let game = s[0].strip_prefix("Game ").unwrap();
            let game = i64::from_str_radix(game, 10).unwrap();
            sum += game;
        }
    }
    sum
}

fn eval_cube_set(s: &str) -> bool {
    for cube in s.split(",") {
        let c: Vec<&str> = cube.trim().split(" ").collect();
        let goodp = match c[1] {
            "blue" => usize::from_str_radix(c[0], 10).unwrap() <= 14,
            "green" => usize::from_str_radix(c[0], 10).unwrap() <= 13,
            "red" => usize::from_str_radix(c[0], 10).unwrap() <= 12,
            _ => unreachable!(),
        };
        if !goodp {
            return false;
        }
    }
    true
}

// Part 2
pub fn part2(input: Vec<String>) -> i64 {
    let mut sum = 0;
    for line in input {
        let s: Vec<&str> = line.split(":").collect();

        sum += eval_cube_set_2(s[1]);
    }
    sum
}

fn eval_cube_set_2(s: &str) -> i64 {
    let (mut blue, mut red, mut green) = (0, 0, 0);

    for s in s.split(';') {
        for cube in s.split(",") {
            let c: Vec<&str> = cube.trim().split(" ").collect();
            match c[1] {
                "blue" => blue = std::cmp::max(i64::from_str_radix(c[0], 10).unwrap(), blue),
                "green" => green = std::cmp::max(i64::from_str_radix(c[0], 10).unwrap(), green),
                "red" => red = std::cmp::max(i64::from_str_radix(c[0], 10).unwrap(), red),
                _ => unreachable!(),
            };
        }
    }
    println!("{}, {}, {}", blue, red, green);
    blue * red * green
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_1() {
        let input = [
            "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
            "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
            "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
            "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green",
        ]
        .map(String::from)
        .to_vec();

        assert_eq!(part1(input), 8);
    }

    #[test]
    fn test_part_2() {
        let input = [
            "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
            "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
            "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
            "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green",
        ]
        .map(String::from)
        .to_vec();

        assert_eq!(part2(input), 2286);
    }
}
