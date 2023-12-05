use regex::Regex;

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
    // Run this first:
    // sed -e s/oneight/oneeight/ \
    //  -e s/twone/twoone/ \
    //  -e s/threeight/threeeight/ \
    //  -e s/fiveight/fiveeight/ \
    //  -e s/sevenine/sevennine/ \
    //  -e s/eightwo/eighttwo/ \
    //  -e s/eighthree/eightthree/ \
    //  -e s/nineight/nineeight/ \
    //  day1.input > treated.input

    let re = Regex::new(r"(one|two|three|four|five|six|seven|eight|nine|ten|\d)")
        .expect("regex init failed.");

    for line in input {
        let mut caps = re.captures_iter(line.as_str()).map(|c| c.extract());

        let (_, [first]) = caps.nth(0).expect("first :(");

        let (_, [last]) = caps.last().unwrap_or(("", [first]));

        let val = get_val(first) * 10 + get_val(last);
        //        println!("{} + {} = {}", first, last, val);

        sum += val;
    }

    sum
}

fn get_val(s: &str) -> i64 {
    match s {
        "0" => 0,
        "1" => 1,
        "2" => 2,
        "3" => 3,
        "4" => 4,
        "5" => 5,
        "6" => 6,
        "7" => 7,
        "8" => 8,
        "9" => 9,
        "one" => 1,
        "two" => 2,
        "three" => 3,
        "four" => 4,
        "five" => 5,
        "six" => 6,
        "seven" => 7,
        "eight" => 8,
        "nine" => 9,
        _ => unreachable!(":("),
    }
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
    fn why_is_this_missing() {
        let input = ["6278teight3three"].map(String::from).to_vec();

        assert_eq!(part2(input), 63);
    }
}
