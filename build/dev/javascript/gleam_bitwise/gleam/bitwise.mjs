import {
  and as do_and,
  not as do_not,
  or as do_or,
  exclusive_or as do_exclusive_or,
  shift_left as do_shift_left,
  shift_right as do_shift_right,
} from "../gleam_bitwise.mjs";

export function and(x, y) {
  return do_and(x, y);
}

export function not(x) {
  return do_not(x);
}

export function or(x, y) {
  return do_or(x, y);
}

export function exclusive_or(x, y) {
  return do_exclusive_or(x, y);
}

export function shift_left(x, y) {
  return do_shift_left(x, y);
}

export function shift_right(x, y) {
  return do_shift_right(x, y);
}
