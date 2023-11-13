import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $map from "../../gleam_stdlib/gleam/map.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import { Eq, Gt, Lt } from "../../gleam_stdlib/gleam/order.mjs";
import * as $set from "../../gleam_stdlib/gleam/set.mjs";
import { CustomType as $CustomType, makeError, isEqual } from "../gleam.mjs";

class Bag extends $CustomType {
  constructor(map) {
    super();
    this.map = map;
  }
}

export function from_map(map) {
  return new Bag(map);
}

export function to_map(bag) {
  return bag.map;
}

export function map(bag, fun) {
  return fold(
    bag,
    new$(),
    (acc, item, copies) => { return insert(acc, copies, fun(item, copies)); },
  );
}

export function fold(bag, initial, fun) {
  return $map.fold(bag.map, initial, fun);
}

export function new$() {
  return new Bag($map.new$());
}

export function remove_all(bag, item) {
  return new Bag($map.delete$(bag.map, item));
}

export function copies(bag, item) {
  let $ = $map.get(bag.map, item);
  if ($.isOk()) {
    let copies$1 = $[0];
    return copies$1;
  } else if (!$.isOk() && !$[0]) {
    return 0;
  } else {
    throw makeError(
      "case_no_match",
      "tote/bag",
      251,
      "copies",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function remove(bag, to_remove, item) {
  let to_remove$1 = $int.absolute_value(to_remove);
  let item_copies = copies(bag, item);
  let $ = $int.compare(to_remove$1, item_copies);
  if ($ instanceof Lt) {
    return new Bag($map.insert(bag.map, item, item_copies - to_remove$1));
  } else if ($ instanceof Gt) {
    return remove_all(bag, item);
  } else if ($ instanceof Eq) {
    return remove_all(bag, item);
  } else {
    throw makeError(
      "case_no_match",
      "tote/bag",
      182,
      "remove",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function insert(bag, to_add, item) {
  let $ = $int.compare(to_add, 0);
  if ($ instanceof Lt) {
    return remove(bag, to_add, item);
  } else if ($ instanceof Eq) {
    return bag;
  } else if ($ instanceof Gt) {
    return new Bag(
      $map.update(
        bag.map,
        item,
        (n) => { return $option.unwrap(n, 0) + to_add; },
      ),
    );
  } else {
    throw makeError(
      "case_no_match",
      "tote/bag",
      146,
      "insert",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function from_list(list) {
  return $list.fold(
    list,
    new$(),
    (bag, item) => { return insert(bag, 1, item); },
  );
}

export function update(bag, item, fun) {
  let count = copies(bag, item);
  let new_count = fun(count);
  let $ = $int.compare(new_count, 0);
  if ($ instanceof Lt) {
    return remove_all(bag, item);
  } else if ($ instanceof Eq) {
    return remove_all(bag, item);
  } else if ($ instanceof Gt) {
    let _pipe = remove_all(bag, item);
    return insert(_pipe, new_count, item);
  } else {
    throw makeError(
      "case_no_match",
      "tote/bag",
      231,
      "update",
      "No case clause matched",
      { values: [$] }
    )
  }
}

export function contains(bag, item) {
  return $map.has_key(bag.map, item);
}

export function is_empty(bag) {
  return isEqual(bag.map, $map.new$());
}

export function size(bag) {
  return fold(bag, 0, (sum, _, copies) => { return sum + copies; });
}

export function intersect(one, other) {
  return fold(
    one,
    new$(),
    (acc, item, copies_in_one) => {
      let $ = copies(other, item);
      if ($ === 0) {
        return acc;
      } else {
        let copies_in_other = $;
        return insert(acc, $int.min(copies_in_one, copies_in_other), item);
      }
    },
  );
}

export function merge(one, other) {
  return fold(
    one,
    other,
    (acc, item, copies_in_one) => { return insert(acc, copies_in_one, item); },
  );
}

export function subtract(one, other) {
  return fold(
    other,
    one,
    (acc, item, copies_in_other) => {
      return remove(acc, copies_in_other, item);
    },
  );
}

export function filter(bag, predicate) {
  return fold(
    bag,
    new$(),
    (acc, item, copies) => {
      let $ = predicate(item, copies);
      if ($) {
        return insert(acc, copies, item);
      } else if (!$) {
        return acc;
      } else {
        throw makeError(
          "case_no_match",
          "tote/bag",
          431,
          "",
          "No case clause matched",
          { values: [$] }
        )
      }
    },
  );
}

export function to_list(bag) {
  return $map.to_list(bag.map);
}

export function to_set(bag) {
  return $set.from_list($map.keys(bag.map));
}
