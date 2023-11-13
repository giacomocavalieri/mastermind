import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $pair from "../gleam_stdlib/gleam/pair.mjs";
import * as $queue from "../gleam_stdlib/gleam/queue.mjs";
import { toList, CustomType as $CustomType, makeError } from "./gleam.mjs";

export class Focused extends $CustomType {}

export class NotFocused extends $CustomType {}

class Ring extends $CustomType {
  constructor(left, focused, right) {
    super();
    this.left = left;
    this.focused = focused;
    this.right = right;
  }
}

export function new$(first, other) {
  return new Ring($queue.new$(), first, $queue.from_list(other));
}

export function focus_next(ring) {
  if (!(ring instanceof Ring)) {
    throw makeError(
      "assignment_no_match",
      "ring",
      19,
      "focus_next",
      "Assignment pattern did not match",
      { value: ring }
    )
  }
  let left = ring.left;
  let focused = ring.focused;
  let right = ring.right;
  let $ = $queue.pop_front(right);
  if ($.isOk()) {
    let new_focused = $[0][0];
    let rest_right = $[0][1];
    return new Ring($queue.push_back(left, focused), new_focused, rest_right);
  } else if (!$.isOk()) {
    let $1 = $queue.pop_front(left);
    if (!$1.isOk()) {
      return new Ring(left, focused, right);
    } else if ($1.isOk()) {
      let new_focused = $1[0][0];
      let rest_left = $1[0][1];
      return new Ring(
        $queue.new$(),
        new_focused,
        $queue.push_back(rest_left, focused),
      );
    } else {
      throw makeError(
        "case_no_match",
        "ring",
        26,
        "focus_next",
        "No case clause matched",
        { values: [$1] }
      )
    }
  } else {
    throw makeError(
      "case_no_match",
      "ring",
      20,
      "focus_next",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function focus_previous(ring) {
  if (!(ring instanceof Ring)) {
    throw makeError(
      "assignment_no_match",
      "ring",
      39,
      "focus_previous",
      "Assignment pattern did not match",
      { value: ring }
    )
  }
  let left = ring.left;
  let focused = ring.focused;
  let right = ring.right;
  let $ = $queue.pop_back(left);
  if ($.isOk()) {
    let new_focused = $[0][0];
    let rest_left = $[0][1];
    return new Ring(rest_left, new_focused, $queue.push_front(right, focused));
  } else if (!$.isOk()) {
    let $1 = $queue.pop_back(right);
    if (!$1.isOk()) {
      return new Ring(left, focused, right);
    } else if ($1.isOk()) {
      let new_focused = $1[0][0];
      let rest_right = $1[0][1];
      return new Ring(
        $queue.push_front(rest_right, focused),
        new_focused,
        $queue.new$(),
      );
    } else {
      throw makeError(
        "case_no_match",
        "ring",
        46,
        "focus_previous",
        "No case clause matched",
        { values: [$1] }
      )
    }
  } else {
    throw makeError(
      "case_no_match",
      "ring",
      40,
      "focus_previous",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function focused_item(ring) {
  return ring.focused;
}

export function replace_focused(ring, item) {
  return new Ring(ring.left, item, ring.right);
}

export function left_of_focused(ring) {
  return $queue.to_list(ring.left);
}

export function right_of_focused(ring) {
  return $queue.to_list(ring.right);
}

export function to_list(ring) {
  let _pipe = toList([
    left_of_focused(ring),
    toList([ring.focused]),
    right_of_focused(ring),
  ]);
  return $list.concat(_pipe);
}

export function to_focus_list(ring) {
  let _pipe = toList([
    $list.map(
      left_of_focused(ring),
      (_capture) => { return $pair.new$(_capture, new NotFocused()); },
    ),
    toList([[ring.focused, new Focused()]]),
    $list.map(
      right_of_focused(ring),
      (_capture) => { return $pair.new$(_capture, new NotFocused()); },
    ),
  ]);
  return $list.concat(_pipe);
}
