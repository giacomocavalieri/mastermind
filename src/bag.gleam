import gleam/int
import gleam/list
import gleam/map.{Map}

pub opaque type Bag(a) {
  Bag(Map(a, Int))
}

pub fn new() -> Bag(a) {
  Bag(map.new())
}

pub fn from_list(list: List(a)) -> Bag(a) {
  list.group(list, by: fn(x) { x })
  |> map.map_values(fn(_key, value) { list.length(value) })
  |> Bag
}

pub fn intersect(one: Bag(a), with other: Bag(a)) -> Bag(a) {
  let Bag(one) = one
  let Bag(other) = other

  {
    use acc, key, one_value <- map.fold(over: one, from: map.new())
    case map.get(other, key) {
      Ok(other_value) -> map.insert(acc, key, int.min(one_value, other_value))
      Error(_) -> acc
    }
  }
  |> Bag
}

pub fn size(bag: Bag(a)) -> Int {
  let Bag(map) = bag
  int.sum(map.values(map))
}

pub fn intersect_all(bags: List(Bag(a))) -> Bag(a) {
  list.fold(over: bags, from: new(), with: intersect)
}
