use countmap::CountMap;
use itertools::Itertools;
use std::collections::HashMap;

fn main() {
  use std::io::BufRead;
  let mut adapters: Vec<u64> = std::io::stdin()
    .lock()
    .lines()
    .map(|x| x.unwrap().parse().unwrap())
    .collect();
  adapters.sort();

  println!("{}", a(&adapters));
  println!("{}", b(&adapters));
}

fn a(adapters: &[u64]) -> u64 {
  let mut differences: CountMap<u64> = adapters.iter()
    .copied()
    .tuple_windows()
    .map(|(a, b)| b - a)
    .collect();
  differences.insert_or_increment(adapters[0]); // outlet to lowest-rated adapter
  differences.insert_or_increment(3); // highest-rated adapter to device
  differences[&1] * differences[&3]
}

#[test]
fn test_a() {
  assert_eq!(a(&[1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19]), 35);
  assert_eq!(a(&[1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31, 32, 33, 34, 35, 38, 39, 42, 45, 46, 47, 48, 49]), 220);
}

fn b(adapters: &[u64]) -> u64 {
  let mut ways_to_make: HashMap<u64, u64> = HashMap::new();
  ways_to_make.insert(0, 1);
  for joltage in adapters.iter() {
    let joltage = *joltage;
    ways_to_make.insert(joltage, joltage.checked_sub(1).and_then(|j| ways_to_make.get(&j)).cloned().unwrap_or(0)
                               + joltage.checked_sub(2).and_then(|j| ways_to_make.get(&j)).cloned().unwrap_or(0)
                               + joltage.checked_sub(3).and_then(|j| ways_to_make.get(&j)).cloned().unwrap_or(0));
  }
  ways_to_make[adapters.last().unwrap()]
}

#[test]
fn test_b() {
  assert_eq!(b(&[1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19]), 8);
  assert_eq!(b(&[1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31, 32, 33, 34, 35, 38, 39, 42, 45, 46, 47, 48, 49]), 19208);
}
