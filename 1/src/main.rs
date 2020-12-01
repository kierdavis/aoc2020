use std::collections::HashSet;
use std::io::BufRead;

fn main() {
  const TARGET: i64 = 2020;
  let input: HashSet<i64> = std::io::BufReader::new(std::io::stdin())
    .lines()
    .map(|line| line.unwrap().parse().unwrap())
    .collect();

  let two_numbers: Vec<i64> = input.iter()
    .copied()
    .filter(|x| input.contains(&(TARGET - *x)))
    .collect();
  assert_eq!(two_numbers.len(), 2);
  println!("Two numbers that sum to {}: {:?}", TARGET, two_numbers);
  println!("  Product: {}", two_numbers.iter().product::<i64>());

  let three_numbers: Vec<i64> = input.iter()
    .copied()
    .filter(|x| {
      input.iter()
        .copied()
        .any(|y| input.contains(&(TARGET - *x - y)))
    })
    .collect();
  assert_eq!(three_numbers.len(), 3);
  println!("Three numbers that sum to {}: {:?}", TARGET, three_numbers);
  println!("  Product: {}", three_numbers.iter().product::<i64>());
}
