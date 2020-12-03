#[derive(Debug)]
struct Slope { right: usize, down: usize }

fn count_trees<S: AsRef<str>>(lines: &[S], slope: &Slope) -> u64 {
  lines.iter()
    .step_by(slope.down)
    .enumerate()
    .map(|(i, line)| line.as_ref().chars().cycle().nth(i*slope.right).unwrap())
    .filter(|&cell| cell == '#')
    .count() as u64
}

fn count_trees_all_slopes<S: AsRef<str>>(lines: &[S]) -> u64 {
  [
    Slope { right: 1, down: 1 },
    Slope { right: 3, down: 1 },
    Slope { right: 5, down: 1 },
    Slope { right: 7, down: 1 },
    Slope { right: 1, down: 2 },
  ]
  .iter()
  .map(|slope| count_trees(lines, slope))
  .product::<u64>()
}

#[test]
fn test_count_trees() {
  let lines = vec![
    "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#",
  ];
  assert_eq!(count_trees(&lines, &Slope { right: 3, down: 1 }), 7);
  assert_eq!(count_trees_all_slopes(&lines), 336);
}

fn main() {
  use std::io::BufRead;
  let lines: Vec<String> = std::io::stdin().lock().lines().map(Result::unwrap).collect();
  println!("Part 1: {}", count_trees(&lines, &Slope { right: 3, down: 1 }));
  println!("Part 2: {}", count_trees_all_slopes(&lines));
}
