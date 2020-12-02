use std::str::FromStr;

#[derive(Debug)]
struct Password(usize, usize, char, String);
impl FromStr for Password {
  type Err = &'static str;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    use lazy_static::lazy_static;
    use regex::Regex;
    lazy_static! {
      static ref REGEX: Regex = Regex::new(r"^(\d+)-(\d+) ([a-z]): ([a-z]+)\s*$").unwrap();
    }
    match REGEX.captures(s) {
      Some(caps) => Ok(Password(
        caps[1].parse().unwrap(),
        caps[2].parse().unwrap(),
        caps[3].chars().next().unwrap(),
        caps[4].to_owned(),
      )),
      _ => Err("bad line"),
    }
  }
}
impl Password {
  fn is_valid_1(&self) -> bool {
    (self.0 ..= self.1).contains(&self.3.chars().filter(|c| *c == self.2).count())
  }
  fn char_at(&self, index: usize) -> char {
    self.3.chars().nth(index-1).unwrap()
  }
  fn is_valid_2(&self) -> bool {
    (self.char_at(self.0) == self.2) ^ (self.char_at(self.1) == self.2)
  }
}

#[test]
fn test_is_valid_1() {
  assert!(Password(1, 3, 'a', "abcde".into()).is_valid_1());
  assert!(!Password(1, 3, 'b', "cdefg".into()).is_valid_1());
  assert!(Password(2, 9, 'c', "ccccccccc".into()).is_valid_1());
}

#[test]
fn test_is_valid_2() {
  assert!(Password(1, 3, 'a', "abcde".into()).is_valid_2());
  assert!(!Password(1, 3, 'b', "cdefg".into()).is_valid_2());
  assert!(!Password(2, 9, 'c', "ccccccccc".into()).is_valid_2());
}

fn main() {
  use std::io::BufRead;
  let passwords: Vec<Password> = std::io::BufReader::new(std::io::stdin())
    .lines()
    .map(|line| line.unwrap().parse().unwrap())
    .collect();
  println!("There are {} valid passwords according to the old policy.", passwords.iter().filter(|p| p.is_valid_1()).count());
  println!("There are {} valid passwords according to the new policy.", passwords.iter().filter(|p| p.is_valid_2()).count());
}
