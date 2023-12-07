// Part 1
pub fn part1(input: Vec<String>) -> i64 {
    let mut sum: i64 = 0;

    for line in input {
        for c in line.chars() {
            if let Some(d) = c.to_digit(10) {
                sum += (d as i64) * 10;
                break;
            }
        }

        for c in line.chars().rev() {
            if let Some(d) = c.to_digit(10) {
                sum += d as i64;
                break;
            }
        }
    }

    sum
}

// Part 2... Oof...
pub fn part2(input: Vec<String>) -> i64 {
    let mut sum: i64 = 0;
    for line in input {
        let mut nums = Vec::new();
        for idx in 0..line.len() {
            let s = &line[idx..];
            if let Some(n) = match s.get(0..1) {
                Some("0") => Some(0),
                Some("1") => Some(1),
                Some("2") => Some(2),
                Some("3") => Some(3),
                Some("4") => Some(4),
                Some("5") => Some(5),
                Some("6") => Some(6),
                Some("7") => Some(7),
                Some("8") => Some(8),
                Some("9") => Some(9),
                _ => None,
            } {
                nums.push(n);
                continue;
            } else if let Some(n) = match s.get(0..3) {
                Some("one") => Some(1),
                Some("two") => Some(2),
                Some("six") => Some(6),
                _ => None,
            } {
                nums.push(n);
                continue;
            } else if let Some(n) = match s.get(0..4) {
                Some("four") => Some(4),
                Some("five") => Some(5),
                Some("nine") => Some(9),
                _ => None,
            } {
                nums.push(n);
                continue;
            } else if let Some(n) = match s.get(0..5) {
                Some("seven") => Some(7),
                Some("three") => Some(3),
                Some("eight") => Some(8),
                _ => None,
            } {
                nums.push(n);
                continue;
            }
        }
        sum += nums.first().unwrap() * 10 + nums.last().unwrap();
    }

    sum
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_1() {
        let input = ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]
            .map(String::from)
            .to_vec();

        assert_eq!(part1(input), 142);
    }

    #[test]
    fn test_part_2() {
        let input = [
            "two1nine",
            "eightwothree",
            "abcone2threexyz",
            "xtwone3four",
            "4nineeightseven2",
            "zoneight234",
            "7pqrstsixteen",
        ]
        .map(String::from)
        .to_vec();

        assert_eq!(part2(input), 281);
    }

    #[test]
    fn special_case() {
        let input = ["oneight"].map(String::from).to_vec();

        assert_eq!(part2(input), 18);
    }
}
