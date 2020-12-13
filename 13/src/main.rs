use itertools::Itertools;

fn main() {
  use std::io::BufRead;
  let stdin = std::io::stdin();
  let mut lines = stdin.lock().lines();
  let arrival_time: i64 = lines.next().unwrap().unwrap().parse().unwrap();
  let buses: Vec<Option<i64>> = lines.next().unwrap().unwrap().split(',').map(|s| match s {
    "x" => None,
    s => Some(s.parse().unwrap()),
  }).collect();

  println!("{}", a(arrival_time, &buses));
  println!("{}", b(&buses));
}

fn a(arrival_time: i64, buses: &[Option<i64>]) -> i64 {
  let (next_bus, time_until_departure) = buses.iter()
    .filter_map(Option::clone)
    .map(|bus| {
      let time_since_last_departure = arrival_time % bus;
      let time_until_next_departure = (bus - time_since_last_departure) % bus;
      (bus, time_until_next_departure)
    })
    .min_by_key(|t| t.1)
    .unwrap();
  next_bus * time_until_departure
}

#[test]
fn test_a() {
  assert_eq!(a(939, &[Some(7),Some(13),None,None,Some(59),None,Some(31),Some(19)]), 295);
}

fn b(buses: &[Option<i64>]) -> i64 {
  // https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Search_by_sieving

  // Find x such that x + index ≡ 0 mod bus for all non-None (index, bus).
  buses.iter()
    .enumerate()
    .filter_map(|(index, maybe_bus)| maybe_bus.map(move |bus| (index, bus)))
    // => Find x such that x ≡ a mod n for all (a, n) [where n = bus, a = -index mod n].
    .map(|(index, bus)| ((-(index as i64)).rem_euclid(bus), bus))
    // Works most efficiently if we work in order of decreasing n.
    .sorted_by_key(|(_, n)| -n)
    .fold(None, |accum, (a2, n2)| match accum {
      // First iteration: just put (a, n) into the accumulator and move on.
      None => Some((a2, n2)),
      // Later iterations: accumulator contains (a1, n1) and we just received (a2, n2).
      Some((a1, n1)) => {
        // The solution belongs to the series a1, a1+n1, a1+n1*2, a1+n1*3, ...
        // Find the first element x' of this series for which x' ≡ a2 mod n2.
        // XXX: Extended Euclidean algorithm would be more efficient here, but this
        // implementation is good enough.
        let x = (0..1000).map(|j| a1+n1*j).filter(|x| x.rem_euclid(n2)==a2).next().unwrap();
        // Now, we know x' ≡ a1 mod n1 and x' ≡ a2 mod n2, so the solution must
        // belong to the series x', x'+n1*n2, x'+n1*n2*2, x'+n1*n2*3, ...
        Some((x, n1*n2))
      },
    })
    .unwrap()
    .0 // Throw away second element of fold accumulator (product of all n).
}

#[test]
fn test_b() {
  assert_eq!(b(&[Some(7),Some(13),None,None,Some(59),None,Some(31),Some(19)]), 1068781);
}
