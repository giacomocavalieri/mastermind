import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $iterator from "../../gleam_stdlib/gleam/iterator.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $map from "../../gleam_stdlib/gleam/map.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import { Eq, Gt, Lt } from "../../gleam_stdlib/gleam/order.mjs";
import * as $pair from "../../gleam_stdlib/gleam/pair.mjs";
import * as $set from "../../gleam_stdlib/gleam/set.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { random_int, random_float } from "../ffi.mjs";
import { Ok, Error, toList, CustomType as $CustomType, makeError } from "../gleam.mjs";
import * as $seed from "../prng/seed.mjs";

class Generator extends $CustomType {
  constructor(step) {
    super();
    this.step = step;
  }
}

export const min_int = -2_147_483_648;

export const max_int = 2_147_483_647;

export function step(generator, seed) {
  return generator.step(seed);
}

export function sample(generator, seed) {
  return step(generator, seed)[0];
}

export function to_iterator(generator, seed) {
  return $iterator.unfold(
    seed,
    (seed) => {
      let $ = step(generator, seed);
      let value = $[0];
      let new_seed = $[1];
      return new $iterator.Next(value, new_seed);
    },
  );
}

export function to_random_iterator(generator) {
  return to_iterator(generator, $seed.random());
}

export function random_sample(generator) {
  let $ = $iterator.first(to_random_iterator(generator));
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "prng/random",
      196,
      "random_sample",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let result = $[0];
  return result;
}

function sort_ascending(one, other, compare) {
  let $ = compare(one, other);
  if ($ instanceof Lt) {
    return [one, other];
  } else if ($ instanceof Eq) {
    return [one, other];
  } else if ($ instanceof Gt) {
    return [other, one];
  } else {
    throw makeError(
      "case_no_match",
      "prng/random",
      265,
      "sort_ascending",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function int(from, to) {
  return new Generator(
    (seed) => {
      let $ = sort_ascending(from, to, $int.compare);
      let low = $[0];
      let high = $[1];
      return random_int(seed, low, high);
    },
  );
}

export function float(from, to) {
  return new Generator(
    (seed) => {
      let $ = sort_ascending(from, to, $float.compare);
      let low = $[0];
      let high = $[1];
      return random_float(seed, low, high);
    },
  );
}

export function constant(value) {
  return new Generator((seed) => { return [value, seed]; });
}

function get_by_weight(loop$first, loop$others, loop$countdown) {
  while (true) {
    let first = loop$first;
    let others = loop$others;
    let countdown = loop$countdown;
    let weight = first[0];
    let value = first[1];
    if (others.hasLength(0)) {
      return value;
    } else if (others.atLeastLength(1)) {
      let second = others.head;
      let rest = others.tail;
      let positive_weight = $float.absolute_value(weight);
      let $ = $float.compare(countdown, positive_weight);
      if ($ instanceof Lt) {
        return value;
      } else if ($ instanceof Eq) {
        return value;
      } else if ($ instanceof Gt) {
        loop$first = second;
        loop$others = rest;
        loop$countdown = countdown - positive_weight;
      } else {
        throw makeError(
          "case_no_match",
          "prng/random",
          475,
          "get_by_weight",
          "No case clause matched",
          { values: [$] }
        )
      }
    } else {
      throw makeError(
        "case_no_match",
        "prng/random",
        471,
        "get_by_weight",
        "No case clause matched",
        { values: [others] }
      )
    }
  }
}

function do_fixed_size_list(loop$acc, loop$seed, loop$generator, loop$length) {
  while (true) {
    let acc = loop$acc;
    let seed = loop$seed;
    let generator = loop$generator;
    let length = loop$length;
    let $ = length <= 0;
    if ($) {
      return [acc, seed];
    } else if (!$) {
      let $1 = step(generator, seed);
      let value = $1[0];
      let seed$1 = $1[1];
      loop$acc = toList([value], acc);
      loop$seed = seed$1;
      loop$generator = generator;
      loop$length = length - 1;
    } else {
      throw makeError(
        "case_no_match",
        "prng/random",
        558,
        "do_fixed_size_list",
        "No case clause matched",
        { values: [$] }
      )
    }
  }
}

export function fixed_size_list(generator, length) {
  return new Generator(
    (seed) => { return do_fixed_size_list(toList([]), seed, generator, length); },
  );
}

export function then$(generator, generator_from) {
  return new Generator(
    (seed) => {
      let $ = step(generator, seed);
      let value = $[0];
      let seed$1 = $[1];
      let _pipe = generator_from(value);
      return step(_pipe, seed$1);
    },
  );
}

export function list(generator) {
  return then$(
    int(0, 32),
    (size) => { return fixed_size_list(generator, size); },
  );
}

export function map(generator, fun) {
  return new Generator(
    (seed) => {
      let $ = step(generator, seed);
      let value = $[0];
      let seed$1 = $[1];
      return [fun(value), seed$1];
    },
  );
}

export function weighted(first, others) {
  let normalise = (pair) => { return $float.absolute_value($pair.first(pair)); };
  let total = normalise(first) + $float.sum($list.map(others, normalise));
  return map(
    float(0.0, total),
    (_capture) => { return get_by_weight(first, others, _capture); },
  );
}

export function try_weighted(options) {
  if (options.atLeastLength(1)) {
    let first = options.head;
    let rest = options.tail;
    return new Ok(weighted(first, rest));
  } else if (options.hasLength(0)) {
    return new Error(undefined);
  } else {
    throw makeError(
      "case_no_match",
      "prng/random",
      459,
      "try_weighted",
      "No case clause matched",
      { values: [options] }
    )
  }
}

function do_fixed_size_dict(
  keys,
  values,
  size,
  unique_keys,
  consecutive_attempts,
  acc
) {
  let has_required_size = unique_keys === size;
  return $bool.guard(
    has_required_size,
    constant(acc),
    () => {
      let has_reached_maximum_attempts = consecutive_attempts <= 10;
      return $bool.guard(
        has_reached_maximum_attempts,
        constant(acc),
        () => {
          return then$(
            keys,
            (key) => {
              let $ = $map.has_key(acc, key);
              if ($) {
                let _pipe = (consecutive_attempts + 1);
                return ((_capture) => {
                  return do_fixed_size_dict(
                    keys,
                    values,
                    size,
                    unique_keys,
                    _capture,
                    acc,
                  );
                })(_pipe);
              } else if (!$) {
                return then$(
                  values,
                  (value) => {
                    let _pipe = $map.insert(acc, key, value);
                    return ((_capture) => {
                      return do_fixed_size_dict(
                        keys,
                        values,
                        size,
                        unique_keys + 1,
                        0,
                        _capture,
                      );
                    })(_pipe);
                  },
                );
              } else {
                throw makeError(
                  "case_no_match",
                  "prng/random",
                  615,
                  "",
                  "No case clause matched",
                  { values: [$] }
                )
              }
            },
          );
        },
      );
    },
  );
}

export function fixed_size_dict(keys, values, size) {
  return do_fixed_size_dict(keys, values, size, 0, 0, $map.new$());
}

export function dict(keys, values) {
  return then$(
    int(0, 32),
    (size) => { return fixed_size_dict(keys, values, size); },
  );
}

export function map2(one, other, fun) {
  return new Generator(
    (seed) => {
      let $ = step(one, seed);
      let a = $[0];
      let seed$1 = $[1];
      let $1 = step(other, seed$1);
      let b = $1[0];
      let seed$2 = $1[1];
      return [fun(a, b), seed$2];
    },
  );
}

export function pair(one, other) {
  return map2(one, other, $pair.new$);
}

export function uniform(first, others) {
  return weighted(
    [1.0, first],
    $list.map(others, (_capture) => { return $pair.new$(1.0, _capture); }),
  );
}

export function try_uniform(options) {
  if (options.atLeastLength(1)) {
    let first = options.head;
    let rest = options.tail;
    return new Ok(uniform(first, rest));
  } else if (options.hasLength(0)) {
    return new Error(undefined);
  } else {
    throw makeError(
      "case_no_match",
      "prng/random",
      379,
      "try_uniform",
      "No case clause matched",
      { values: [options] }
    )
  }
}

export function choose(one, other) {
  return uniform(one, toList([other]));
}

export function map3(one, two, three, fun) {
  return new Generator(
    (seed) => {
      let $ = step(one, seed);
      let a = $[0];
      let seed$1 = $[1];
      let $1 = step(two, seed$1);
      let b = $1[0];
      let seed$2 = $1[1];
      let $2 = step(three, seed$2);
      let c = $2[0];
      let seed$3 = $2[1];
      return [fun(a, b, c), seed$3];
    },
  );
}

export function map4(one, two, three, four, fun) {
  return new Generator(
    (seed) => {
      let $ = step(one, seed);
      let a = $[0];
      let seed$1 = $[1];
      let $1 = step(two, seed$1);
      let b = $1[0];
      let seed$2 = $1[1];
      let $2 = step(three, seed$2);
      let c = $2[0];
      let seed$3 = $2[1];
      let $3 = step(four, seed$3);
      let d = $3[0];
      let seed$4 = $3[1];
      return [fun(a, b, c, d), seed$4];
    },
  );
}

export function map5(one, two, three, four, five, fun) {
  return new Generator(
    (seed) => {
      let $ = step(one, seed);
      let a = $[0];
      let seed$1 = $[1];
      let $1 = step(two, seed$1);
      let b = $1[0];
      let seed$2 = $1[1];
      let $2 = step(three, seed$2);
      let c = $2[0];
      let seed$3 = $2[1];
      let $3 = step(four, seed$3);
      let d = $3[0];
      let seed$4 = $3[1];
      let $4 = step(five, seed$4);
      let e = $4[0];
      let seed$5 = $4[1];
      return [fun(a, b, c, d, e), seed$5];
    },
  );
}

export function fixed_size_string(size) {
  let _pipe = fixed_size_list(utf_codepoint_in_range(0, 1023), size);
  return map(_pipe, $string.from_utf_codepoints);
}

export function string() {
  return then$(int(0, 32), (size) => { return fixed_size_string(size); });
}

export function bit_array() {
  return map(string(), $bit_array.from_string);
}

function utf_codepoint_in_range(lower, upper) {
  return then$(
    int(lower, upper),
    (raw_codepoint) => {
      let $ = $string.utf_codepoint(raw_codepoint);
      if ($.isOk()) {
        let codepoint = $[0];
        return constant(codepoint);
      } else if (!$.isOk()) {
        return utf_codepoint_in_range(lower, upper);
      } else {
        throw makeError(
          "case_no_match",
          "prng/random",
          948,
          "",
          "No case clause matched",
          { values: [$] }
        )
      }
    },
  );
}

export function set(generator) {
  return then$(
    int(0, 32),
    (size) => { return fixed_size_set(generator, size); },
  );
}

export function fixed_size_set(generator, size) {
  return do_fixed_size_set(generator, size, 0, 0, $set.new$());
}

function do_fixed_size_set(
  generator,
  size,
  unique_items,
  consecutive_attempts,
  acc
) {
  let has_required_size = unique_items === size;
  return $bool.guard(
    has_required_size,
    constant(acc),
    () => {
      let has_reached_maximum_attempts = consecutive_attempts <= 10;
      return $bool.guard(
        has_reached_maximum_attempts,
        constant(acc),
        () => {
          return then$(
            generator,
            (item) => {
              let $ = $set.contains(acc, item);
              if ($) {
                let _pipe = (consecutive_attempts + 1);
                return ((_capture) => {
                  return do_fixed_size_set(
                    generator,
                    size,
                    unique_items,
                    _capture,
                    acc,
                  );
                })(_pipe);
              } else if (!$) {
                let _pipe = $set.insert(acc, item);
                return ((_capture) => {
                  return do_fixed_size_set(
                    generator,
                    size,
                    unique_items + 1,
                    0,
                    _capture,
                  );
                })(_pipe);
              } else {
                throw makeError(
                  "case_no_match",
                  "prng/random",
                  676,
                  "",
                  "No case clause matched",
                  { values: [$] }
                )
              }
            },
          );
        },
      );
    },
  );
}
