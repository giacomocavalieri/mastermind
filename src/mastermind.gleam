import gleam/list
import gleam/bool.{guard}
import prng/random
import tote/bag

// GAME TYPES ------------------------------------------------------------------

/// A peg you use to make guesses in Mastermind.
/// 
pub type Peg {
  Blue
  Green
  Orange
  Purple
  Red
  Yellow
}

/// The secret code you're trying to guess in a game of Mastermind.
/// 
pub type SecretCode {
  SecretCode(one: Peg, two: Peg, three: Peg, four: Peg)
}

/// A guess you make to try and win Mastermind.
/// 
pub type Guess {
  Guess(one: Peg, two: Peg, three: Peg, four: Peg)
}

/// A hint you get after makind a guess.
/// 
pub type Hint {
  CorrectColor
  CorrectPosition
}

/// The result you get after making a guess.
/// 
pub type GuessOutcome {
  Win
  Lose
  Continue
}

/// A game of Mastermind.
/// 
pub type Game {
  Game(
    secret_code: SecretCode,
    guesses: List(#(Guess, List(Hint))),
    max_attempts: Int,
  )
}

/// An error that may occur if you try tro break the rules of the game.
/// 
pub type GameError {
  /// This error occurs if you try to make a guess for a game where you've
  /// already reached the maximum number of allowed guesses.
  /// 
  NoMoreGuesses
}

// STARTING A NEW GAME ---------------------------------------------------------

/// Starts a new game of Mastermind where the code to guess is the given one.
/// 
pub fn new_game(secret_code: SecretCode) -> Game {
  Game(secret_code: secret_code, guesses: [], max_attempts: 10)
}

/// Starts a new game of Masterming with a random secret code to guess.
/// 
pub fn random_game() -> Game {
  let peg = random.uniform(Blue, [Green, Orange, Purple, Red, Yellow])
  let secret_code = random.map4(peg, peg, peg, peg, SecretCode)
  new_game(random.random_sample(secret_code))
}

// QUERYING A GAME -------------------------------------------------------------

/// Returns `True` if one can no longer make guesses.
/// 
pub fn are_guesses_over(game: Game) -> Bool {
  attempted_guesses(game) >= game.max_attempts
}

/// Returns the number of guesses made so far.
/// 
pub fn attempted_guesses(game: Game) -> Int {
  list.length(game.guesses)
}

/// The number of guesses remaining before game over.
/// 
pub fn remaining_guesses(game: Game) -> Int {
  game.max_attempts - attempted_guesses(game)
}

// MAKING GUESSES --------------------------------------------------------------

/// The hints corresponding to a winning game.
/// 
const winning_hints = [
  CorrectPosition,
  CorrectPosition,
  CorrectPosition,
  CorrectPosition,
]

/// Makes a guess and returns the new state of the game with the corresponding
/// outcome.
/// 
/// Returns an error if you make a guess in a game that already has reached the
/// maximum number of allowed guesses.
/// 
pub fn make_guess(
  game: Game,
  guess: Guess,
) -> Result(#(GuessOutcome, Game), GameError) {
  use <- guard(when: are_guesses_over(game), return: Error(NoMoreGuesses))
  let #(new_game, hints) = add_guess_and_get_hits(guess, game)
  use <- guard(when: hints == winning_hints, return: Ok(#(Win, new_game)))
  use <- guard(when: are_guesses_over(new_game), return: Ok(#(Lose, new_game)))
  Ok(#(Continue, new_game))
}

fn add_guess_and_get_hits(guess: Guess, to game: Game) -> #(Game, List(Hint)) {
  let hints = compare(guess, with: game.secret_code)
  let new_guesses = [#(guess, hints), ..game.guesses]
  let new_game = Game(..game, guesses: new_guesses)
  #(new_game, hints)
}

/// Compares a guess with the secret code to get a list of hints
/// that can be used for the next move.
/// 
fn compare(guess: Guess, with secret_code: SecretCode) -> List(Hint) {
  let correct_positions = correct_positions(in: secret_code, given: guess)
  let correct_colors = correct_colors(in: secret_code, given: guess)

  [
    list.repeat(CorrectPosition, correct_positions),
    list.repeat(CorrectColor, correct_colors),
  ]
  |> list.concat
  |> list.shuffle
}

/// Given the secret code and a guess, returns the number of pegs
/// in the guess that are in the correct position.
/// 
fn correct_positions(in secret_code: SecretCode, given guess: Guess) -> Int {
  let are_exact_match = fn(pair: #(a, a)) { pair.0 == pair.1 }
  let matching_pairs = list.filter(pairs(secret_code, guess), are_exact_match)
  list.length(matching_pairs)
}

/// Given the secret code and a guess, returns a list of pairs of pegs
/// one for each corresponding position.
///
fn pairs(secret_code: SecretCode, guess: Guess) -> List(#(Peg, Peg)) {
  [
    #(secret_code.one, guess.one),
    #(secret_code.two, guess.two),
    #(secret_code.three, guess.three),
    #(secret_code.four, guess.four),
  ]
}

/// Given the secret code and a guess, returns the number of colors that are
/// correct but not in the correct position.
/// 
fn correct_colors(in secret_code: SecretCode, given guess: Guess) -> Int {
  let do_not_match = fn(pair: #(a, a)) { pair.0 != pair.1 }
  let non_matching_pairs = list.filter(pairs(secret_code, guess), do_not_match)
  let #(secret_pegs, guess_pegs) = list.unzip(non_matching_pairs)
  elements_in_common(secret_pegs, guess_pegs)
}

/// Counts the elements in common between the two list.
/// 
/// ## Example
/// 
/// Here there's only two elements in common: a `1` and a `2`
/// ```gleam
/// intersection_size([1, 1, 2, 3], [1, 2, 2])
/// // -> 2
/// ```
/// 
/// Here there's two elements in common: two `1`s
/// ```gleam
/// intersection_size([1, 1, 1, 2], [1, 1, 3])
/// // -> 2
/// ```
/// 
fn elements_in_common(one: List(a), other: List(a)) -> Int {
  bag.from_list(one)
  |> bag.intersect(with: bag.from_list(other))
  |> bag.size
}
