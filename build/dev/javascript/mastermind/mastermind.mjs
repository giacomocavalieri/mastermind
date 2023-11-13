import * as $bool from "../gleam_stdlib/gleam/bool.mjs";
import { guard } from "../gleam_stdlib/gleam/bool.mjs";
import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import { DecodeError } from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $random from "../prng/prng/random.mjs";
import * as $bag from "../tote/tote/bag.mjs";
import { Ok, Error, toList, CustomType as $CustomType, isEqual } from "./gleam.mjs";

export class Blue extends $CustomType {}

export class Green extends $CustomType {}

export class Orange extends $CustomType {}

export class Purple extends $CustomType {}

export class Red extends $CustomType {}

export class Yellow extends $CustomType {}

export class SecretCode extends $CustomType {
  constructor(one, two, three, four) {
    super();
    this.one = one;
    this.two = two;
    this.three = three;
    this.four = four;
  }
}

export class Guess extends $CustomType {
  constructor(one, two, three, four) {
    super();
    this.one = one;
    this.two = two;
    this.three = three;
    this.four = four;
  }
}

export class CorrectColor extends $CustomType {}

export class CorrectPosition extends $CustomType {}

export class Win extends $CustomType {}

export class Lose extends $CustomType {}

export class Continue extends $CustomType {}

export class Game extends $CustomType {
  constructor(secret_code, guesses, max_attempts) {
    super();
    this.secret_code = secret_code;
    this.guesses = guesses;
    this.max_attempts = max_attempts;
  }
}

export class NoMoreGuesses extends $CustomType {}

const winning_hints = toList([
  new CorrectPosition(),
  new CorrectPosition(),
  new CorrectPosition(),
  new CorrectPosition(),
]);

export function new_game(secret_code) {
  return new Game(secret_code, toList([]), 10);
}

export function random_game() {
  let peg = $random.uniform(
    new Blue(),
    toList([new Green(), new Orange(), new Purple(), new Red(), new Yellow()]),
  );
  let secret_code$1 = $random.map4(
    peg,
    peg,
    peg,
    peg,
    (var0, var1, var2, var3) => {
      return new SecretCode(var0, var1, var2, var3);
    },
  );
  return new_game($random.random_sample(secret_code$1));
}

export function attempted_guesses(game) {
  return $list.length(game.guesses);
}

export function are_guesses_over(game) {
  return attempted_guesses(game) >= game.max_attempts;
}

export function remaining_guesses(game) {
  return game.max_attempts - attempted_guesses(game);
}

export function secret_code(game) {
  return game.secret_code;
}

function pairs(secret_code, guess) {
  return toList([
    [secret_code.one, guess.one],
    [secret_code.two, guess.two],
    [secret_code.three, guess.three],
    [secret_code.four, guess.four],
  ]);
}

function correct_positions(secret_code, guess) {
  let are_exact_match = (pair) => { return isEqual(pair[0], pair[1]); };
  let matching_pairs = $list.filter(pairs(secret_code, guess), are_exact_match);
  return $list.length(matching_pairs);
}

function elements_in_common(one, other) {
  let _pipe = $bag.from_list(one);
  let _pipe$1 = $bag.intersect(_pipe, $bag.from_list(other));
  return $bag.size(_pipe$1);
}

function correct_colors(secret_code, guess) {
  let do_not_match = (pair) => { return !isEqual(pair[0], pair[1]); };
  let non_matching_pairs = $list.filter(pairs(secret_code, guess), do_not_match);
  let $ = $list.unzip(non_matching_pairs);
  let secret_pegs = $[0];
  let guess_pegs = $[1];
  return elements_in_common(secret_pegs, guess_pegs);
}

function compare(guess, secret_code) {
  let correct_positions$1 = correct_positions(secret_code, guess);
  let correct_colors$1 = correct_colors(secret_code, guess);
  let _pipe = toList([
    $list.repeat(new CorrectPosition(), correct_positions$1),
    $list.repeat(new CorrectColor(), correct_colors$1),
  ]);
  return $list.concat(_pipe);
}

function add_guess_and_get_hits(guess, game) {
  let hints = compare(guess, game.secret_code);
  let new_guesses = toList([[guess, hints]], game.guesses);
  let new_game$1 = game.withFields({ guesses: new_guesses });
  return [new_game$1, hints];
}

export function make_guess(game, guess) {
  return guard(
    are_guesses_over(game),
    new Error(new NoMoreGuesses()),
    () => {
      let $ = add_guess_and_get_hits(guess, game);
      let new_game$1 = $[0];
      let hints = $[1];
      return guard(
        isEqual(hints, winning_hints),
        new Ok([new Win(), new_game$1]),
        () => {
          return guard(
            are_guesses_over(new_game$1),
            new Ok([new Lose(), new_game$1]),
            () => { return new Ok([new Continue(), new_game$1]); },
          );
        },
      );
    },
  );
}

export function parse_peg(value) {
  let $ = $string.lowercase(value);
  if ($ === "r") {
    return new Ok(new Red());
  } else if ($ === "y") {
    return new Ok(new Yellow());
  } else if ($ === "g") {
    return new Ok(new Green());
  } else if ($ === "b") {
    return new Ok(new Blue());
  } else if ($ === "o") {
    return new Ok(new Orange());
  } else if ($ === "p") {
    return new Ok(new Purple());
  } else {
    let other = $;
    return new Error(
      toList([
        new DecodeError(
          "a peg letter (one of 'r', 'y', 'g', 'b', 'o' or 'p')",
          other,
          toList([]),
        ),
      ]),
    );
  }
}
