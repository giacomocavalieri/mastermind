import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $gleeunit from "../gleeunit/gleeunit.mjs";
import * as $bag from "../tote/tote/bag.mjs";
import { Ok, Error, toList, makeError, isEqual } from "./gleam.mjs";
import * as $mastermind from "./mastermind.mjs";
import {
  Blue,
  Continue,
  CorrectColor,
  CorrectPosition,
  Green,
  Guess,
  Lose,
  NoMoreGuesses,
  Red,
  SecretCode,
  Win,
  Yellow,
} from "./mastermind.mjs";

export function main() {
  return $gleeunit.main();
}

export function guessing_the_solution_results_in_a_win_test() {
  let secret_code = new SecretCode(
    new Red(),
    new Green(),
    new Blue(),
    new Green(),
  );
  let game = $mastermind.new_game(secret_code);
  let guess = new Guess(new Red(), new Green(), new Blue(), new Green());
  let $ = $mastermind.make_guess(game, guess);
  if (!$.isOk() || !($[0][0] instanceof Win)) {
    throw makeError(
      "assignment_no_match",
      "mastermind_test",
      18,
      "guessing_the_solution_results_in_a_win_test",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  return $;
}

function make_guesses(loop$game, loop$guesses) {
  while (true) {
    let game = loop$game;
    let guesses = loop$guesses;
    if (guesses.hasLength(0)) {
      return new Error(undefined);
    } else if (guesses.hasLength(1)) {
      let guess = guesses.head;
      let $ = $mastermind.make_guess(game, guess);
      if ($.isOk()) {
        let result = $[0];
        return new Ok(result);
      } else if (!$.isOk()) {
        return new Error(undefined);
      } else {
        throw makeError(
          "case_no_match",
          "mastermind_test",
          53,
          "make_guesses",
          "No case clause matched",
          { values: [$] }
        )
      }
    } else if (guesses.atLeastLength(1)) {
      let guess = guesses.head;
      let guesses$1 = guesses.tail;
      let $ = $mastermind.make_guess(game, guess);
      if ($.isOk() && $[0][0] instanceof Continue) {
        let new_game = $[0][1];
        loop$game = new_game;
        loop$guesses = guesses$1;
      } else {
        return new Error(undefined);
      }
    } else {
      throw makeError(
        "case_no_match",
        "mastermind_test",
        50,
        "make_guesses",
        "No case clause matched",
        { values: [guesses] }
      )
    }
  }
}

export function making_more_than_ten_guesses_results_in_an_error_test() {
  let secret_code = new SecretCode(
    new Red(),
    new Green(),
    new Blue(),
    new Blue(),
  );
  let game = $mastermind.new_game(secret_code);
  let guess = new Guess(new Red(), new Red(), new Red(), new Red());
  let guesses = $list.repeat(guess, 10);
  let $ = make_guesses(game, guesses);
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "mastermind_test",
      26,
      "making_more_than_ten_guesses_results_in_an_error_test",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let game$1 = $[0][1];
  let $1 = $mastermind.make_guess(game$1, guess);
  if ($1.isOk() || !($1[0] instanceof NoMoreGuesses)) {
    throw makeError(
      "assignment_no_match",
      "mastermind_test",
      27,
      "making_more_than_ten_guesses_results_in_an_error_test",
      "Assignment pattern did not match",
      { value: $1 }
    )
  }
  return $1;
}

export function making_ten_wrong_guesses_results_in_a_lose_test() {
  let secret_code = new SecretCode(
    new Red(),
    new Green(),
    new Blue(),
    new Blue(),
  );
  let game = $mastermind.new_game(secret_code);
  let guess = new Guess(new Yellow(), new Blue(), new Blue(), new Green());
  let $ = make_guesses(game, $list.repeat(guess, 10));
  if (!$.isOk() || !($[0][0] instanceof Lose)) {
    throw makeError(
      "assignment_no_match",
      "mastermind_test",
      43,
      "making_ten_wrong_guesses_results_in_a_lose_test",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  return $;
}

function hints_from_guess(game, guess) {
  let $ = $mastermind.make_guess(game, guess);
  if (!$.isOk()) {
    return new Error(undefined);
  } else if ($.isOk()) {
    let new_game = $[0][1];
    let $1 = new_game.guesses;
    if ($1.atLeastLength(1)) {
      let hints = $1.head[1];
      return new Ok(hints);
    } else if ($1.hasLength(0)) {
      return new Error(undefined);
    } else {
      throw makeError(
        "case_no_match",
        "mastermind_test",
        69,
        "hints_from_guess",
        "No case clause matched",
        { values: [$1] }
      )
    }
  } else {
    throw makeError(
      "case_no_match",
      "mastermind_test",
      66,
      "hints_from_guess",
      "No case clause matched",
      { values: [$] }
    )
  }
}

function same_elements(one, other) {
  return isEqual($bag.from_list(one), $bag.from_list(other));
}

export function guess_is_turned_into_correct_hints_test() {
  let secret_code = new SecretCode(
    new Red(),
    new Green(),
    new Blue(),
    new Blue(),
  );
  let game = $mastermind.new_game(secret_code);
  let guess = new Guess(new Yellow(), new Blue(), new Blue(), new Green());
  let $ = hints_from_guess(game, guess);
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "mastermind_test",
      34,
      "guess_is_turned_into_correct_hints_test",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let hints = $[0];
  let $1 = same_elements(
    hints,
    toList([new CorrectPosition(), new CorrectColor(), new CorrectColor()]),
  );
  if (!$1) {
    throw makeError(
      "assignment_no_match",
      "mastermind_test",
      35,
      "guess_is_turned_into_correct_hints_test",
      "Assignment pattern did not match",
      { value: $1 }
    )
  }
  return $1;
}
