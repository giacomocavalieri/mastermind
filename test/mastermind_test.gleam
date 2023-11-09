import gleam/list
import gleeunit
import tote/bag
import mastermind.{
  type Game, type Guess, type GuessOutcome, type Hint, Blue, Continue,
  CorrectColor, CorrectPosition, Green, Guess, Lose, NoMoreGuesses, Red,
  SecretCode, Win, Yellow,
}

pub fn main() {
  gleeunit.main()
}

pub fn guessing_the_solution_results_in_a_win_test() {
  let secret_code = SecretCode(Red, Green, Blue, Green)
  let game = mastermind.new_game(secret_code)
  let guess = Guess(Red, Green, Blue, Green)
  let assert Ok(#(Win, _)) = mastermind.make_guess(game, guess)
}

pub fn making_more_than_ten_guesses_results_in_an_error_test() {
  let secret_code = SecretCode(Red, Green, Blue, Blue)
  let game = mastermind.new_game(secret_code)
  let guess = Guess(Red, Red, Red, Red)
  let guesses = list.repeat(guess, 10)
  let assert Ok(#(_, game)) = make_guesses(game, guesses)
  let assert Error(NoMoreGuesses) = mastermind.make_guess(game, guess)
}

pub fn guess_is_turned_into_correct_hints_test() {
  let secret_code = SecretCode(Red, Green, Blue, Blue)
  let game = mastermind.new_game(secret_code)
  let guess = Guess(Yellow, Blue, Blue, Green)
  let assert Ok(hints) = hints_from_guess(game, guess)
  let assert True =
    same_elements(hints, [CorrectPosition, CorrectColor, CorrectColor])
}

pub fn making_ten_wrong_guesses_results_in_a_lose_test() {
  let secret_code = SecretCode(Red, Green, Blue, Blue)
  let game = mastermind.new_game(secret_code)
  let guess = Guess(Yellow, Blue, Blue, Green)
  let assert Ok(#(Lose, _)) = make_guesses(game, list.repeat(guess, 10))
}

fn make_guesses(
  game: Game,
  guesses: List(Guess),
) -> Result(#(GuessOutcome, Game), Nil) {
  case guesses {
    [] -> Error(Nil)
    [guess] ->
      case mastermind.make_guess(game, guess) {
        Ok(result) -> Ok(result)
        Error(_) -> Error(Nil)
      }
    [guess, ..guesses] ->
      case mastermind.make_guess(game, guess) {
        Ok(#(Continue, new_game)) -> make_guesses(new_game, guesses)
        _ -> Error(Nil)
      }
  }
}

fn hints_from_guess(game: Game, guess: Guess) -> Result(List(Hint), Nil) {
  case mastermind.make_guess(game, guess) {
    Error(_) -> Error(Nil)
    Ok(#(_, new_game)) ->
      case new_game.guesses {
        [#(_guess, hints), ..] -> Ok(hints)
        [] -> Error(Nil)
      }
  }
}

fn same_elements(one: List(a), other: List(a)) -> Bool {
  bag.from_list(one) == bag.from_list(other)
}
