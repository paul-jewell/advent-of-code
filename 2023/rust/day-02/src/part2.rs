use crate::custom_error::AocError;

use std::collections::BTreeMap;

use nom::{
    bytes::complete::tag,
    character::complete::{
        self, alpha1, digit1, line_ending,
    },
    multi::separated_list1,
    sequence::{preceded, separated_pair},
    IResult,
};

#[derive(Debug)]
struct Cube<'a> {
    color: &'a str,
    amount: u32,
}

#[derive(Debug)]
struct Game<'a> {
    id: &'a str,
    rounds: Vec<Vec<Cube<'a>>>,
}

impl<'a> Game<'a> {
    fn minimum_cube_set(&self) -> u32 {
        let map: BTreeMap<&str, u32> = BTreeMap::new();
        self.rounds
            .iter()
            .fold(map, |mut acc, round| {
                for cube in round.iter() {
                    acc.entry(cube.color).and_modify(|v| {
                        *v = (*v).max(cube.amount);
                    })
                    .or_insert(cube.amount);
                }
                acc
            })
            .values()
            .product()
        }
}


fn cube(input: &str) -> IResult<&str, Cube> {
    let (input, (amount, color)) = separated_pair(complete::u32, tag(" "), alpha1)(input)?;
    Ok((input, Cube {color, amount}))
}

fn round(input: &str) -> IResult<&str, Vec<Cube>> {
    let (input, cubes) = separated_list1(tag(", "), cube)(input)?;
    Ok((input, cubes))
}

fn game(input:&str) -> IResult<&str, Game> {
    let (input, id) =
        preceded(tag("Game "), digit1)(input)?;
    let (input, rounds) = preceded(
        tag(": "),
        separated_list1(tag("; "), round),
    )(input)?;
    Ok((input, Game {rounds, id}))
}

fn parse_games(input: &str) -> IResult<&str, Vec<Game>> {
    let(input, games) = separated_list1(line_ending, game)(input)?;
    Ok((input, games))
}

#[tracing::instrument]
pub fn process(
    input: &str,
) -> miette::Result<String, AocError> {
    let games = parse_games(input).expect("Should parse");
    Ok(games
        .1
        .iter()
        .map(|game| game.minimum_cube_set())
        .sum::<u32>()
        .to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process() -> miette::Result<()> {
        let input = include_str!("../part1-test-input.txt");
        assert_eq!("2286", process(input)?);
        Ok(())
    }

    #[test]
    fn test_part2_result() -> miette::Result<()> {
        let input = include_str!("../input2.txt");
        assert_eq!("87984", process(input)?);
        Ok(())
    }
}
