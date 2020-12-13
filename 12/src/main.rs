use std::ops::{Add, Mul};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Turn {
  Left,
  Right,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Vector(i64, i64);
impl Vector {
  const ZERO: Vector = Vector(0, 0);
  const NORTH: Vector = Vector(0, 1);
  const EAST: Vector = Vector(1, 0);
  const SOUTH: Vector = Vector(0, -1);
  const WEST: Vector = Vector(-1, 0);
  fn turn(mut self, direction: Turn, num_quarter_turns: i64) -> Self {
    for i in 1..=num_quarter_turns {
      self = match direction {
        Turn::Left => Vector(-self.1, self.0),
        Turn::Right => Vector(self.1, -self.0),
      }
    }
    self
  }
  fn manhattan(&self) -> i64 {
    self.0.abs() + self.1.abs()
  }
}
impl Add<Vector> for Vector {
  type Output = Vector;
  fn add(self, other: Vector) -> Vector {
    Vector(self.0 + other.0, self.1 + other.1)
  }
}
impl Mul<i64> for Vector {
  type Output = Vector;
  fn mul(self, other: i64) -> Vector {
    Vector(self.0 * other, self.1 * other)
  }
}

#[derive(Clone, Copy, Debug)]
enum Instruction {
  Displace(Vector),
  Turn(Turn, i64),
  Forward(i64),
}
impl Instruction {
  fn parse(s: &str) -> Self {
    let (command, argument) = s.split_at(1);
    let argument = argument.parse().unwrap();
    match command {
      "N" => Self::Displace(Vector::NORTH * argument),
      "S" => Self::Displace(Vector::SOUTH * argument),
      "E" => Self::Displace(Vector::EAST * argument),
      "W" => Self::Displace(Vector::WEST * argument),
      "L" => Self::Turn(Turn::Left, argument / 90),
      "R" => Self::Turn(Turn::Right, argument / 90),
      "F" => Self::Forward(argument),
      _ => panic!("bad command"),
    }
  }
}


fn main() {
  use std::io::BufRead;
  let insts = std::io::stdin()
    .lock()
    .lines()
    .map(|line| Instruction::parse(&line.unwrap()))
    .collect::<Vec<_>>();

  let (total_displacement, _) = insts.iter()
    .fold((Vector::ZERO, Vector::EAST), |(position, bearing), inst| match *inst {
      Instruction::Displace(delta) => (position + delta, bearing),
      Instruction::Turn(direction, amount) => (position, bearing.turn(direction, amount)),
      Instruction::Forward(amount) => (position + bearing * amount, bearing),
    });
  println!("{:?}", total_displacement.manhattan());

  let (total_displacement, _) = insts.iter()
    .fold((Vector::ZERO, Vector(10, 1)), |(position, waypoint), inst| match *inst {
      Instruction::Displace(delta) => (position, waypoint + delta),
      Instruction::Turn(direction, amount) => (position, waypoint.turn(direction, amount)),
      Instruction::Forward(amount) => (position + waypoint * amount, waypoint),
    });
  println!("{:?}", total_displacement.manhattan());
}
