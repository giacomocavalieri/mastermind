import gleam/list
import gleam/pair
import gleam/queue.{type Queue}

pub type Focus {
  Focused
  NotFocused
}

pub opaque type Ring(a) {
  Ring(left: Queue(a), focused: a, right: Queue(a))
}

pub fn new(first: a, other: List(a)) -> Ring(a) {
  Ring(queue.new(), first, queue.from_list(other))
}

pub fn focus_next(ring: Ring(a)) -> Ring(a) {
  let Ring(left, focused, right) = ring
  case queue.pop_front(right) {
    Ok(#(new_focused, rest_right)) ->
      // The ring still has items to the right of the focused one, so we move
      // focus to the next item to the right of the current one
      Ring(queue.push_back(left, focused), new_focused, rest_right)
    Error(_) ->
      case queue.pop_front(left) {
        Error(_) ->
          // The ring only has one item, so the focus stays on the current one
          Ring(left, focused, right)
        Ok(#(new_focused, rest_left)) ->
          // The focused item is the last one, so we move focus back to the
          // first item (the leftmost one)
          Ring(queue.new(), new_focused, queue.push_back(rest_left, focused))
      }
  }
}

pub fn focus_previous(ring: Ring(a)) -> Ring(a) {
  let Ring(left, focused, right) = ring
  case queue.pop_back(left) {
    Ok(#(new_focused, rest_left)) ->
      // The ring still has items to the left of the focused one, so we move
      // focus to the previous item to the left of the current one
      Ring(rest_left, new_focused, queue.push_front(right, focused))
    Error(_) ->
      case queue.pop_back(right) {
        Error(_) ->
          // The ring only has one item, so the focus stays on the current one
          Ring(left, focused, right)
        Ok(#(new_focused, rest_right)) ->
          // The focused item is the first one, so we move focus all the way
          // down to the last item (the rightmost one)
          Ring(queue.push_front(rest_right, focused), new_focused, queue.new())
      }
  }
}

pub fn focused_item(ring: Ring(a)) -> a {
  ring.focused
}

pub fn replace_focused(in ring: Ring(a), with item: a) -> Ring(a) {
  Ring(ring.left, item, ring.right)
}

pub fn left_of_focused(ring: Ring(a)) -> List(a) {
  queue.to_list(ring.left)
}

pub fn right_of_focused(ring: Ring(a)) -> List(a) {
  queue.to_list(ring.right)
}

pub fn to_list(ring: Ring(a)) -> List(a) {
  [left_of_focused(ring), [ring.focused], right_of_focused(ring)]
  |> list.concat
}

pub fn to_focus_list(ring: Ring(a)) -> List(#(a, Focus)) {
  [
    list.map(left_of_focused(ring), pair.new(_, NotFocused)),
    [#(ring.focused, Focused)],
    list.map(right_of_focused(ring), pair.new(_, NotFocused)),
  ]
  |> list.concat
}
