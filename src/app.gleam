import gleam/bool
import gleam/int
import gleam/list
import gleam/io
import gleam/option.{type Option, None, Some}
import lustre
import lustre/attribute.{class, classes, disabled, id}
import lustre/event.{on_click}
import lustre/element.{type Element, text}
import lustre/element/html.{span}
import lustre/ui
import lustre/ui/button.{button}
import lustre/ui/stack.{stack}
import lustre/ui/styles.{elements, theme}
import lustre/ui/cluster.{cluster}
import mastermind.{
  type Game, type Guess, type GuessOutcome, type Hint, type Peg, Blue, Continue,
  CorrectColor, CorrectPosition, Green, Guess, Lose, NoMoreGuesses, Orange,
  Purple, Red, Win, Yellow,
}

// APPLICATION ENTRY POINT -----------------------------------------------------

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "[data-lustre-app]", Nil)
  Nil
}

// MODEL -----------------------------------------------------------------------

/// The state of the game, keeping track of the outcome of the game and the
/// current colors chosen for each peg.
/// 
type Model {
  Model(pegs: List(#(Option(Peg), Focus)), status: GuessOutcome, game: Game)
}

type Focus {
  Focused
  NotFocused
}

fn init(_: Nil) -> Model {
  Model(
    pegs: [
      #(None, Focused),
      #(None, NotFocused),
      #(None, NotFocused),
      #(None, NotFocused),
    ],
    status: Continue,
    game: mastermind.random_game(),
  )
}

// UPDATE ----------------------------------------------------------------------

type Message {
  NewGame
  FocusPeg(Int)
  SetPeg(Int, Peg)
  TryGuess(Guess)
}

fn update(model: Model, message: Message) -> Model {
  case message {
    NewGame -> init(Nil)

    SetPeg(peg_position, peg_value) ->
      set_peg(model, peg_position, peg_value)
      |> unfocus_pegs
      |> focus_first_empty_peg

    FocusPeg(peg_position) ->
      unfocus_pegs(model)
      |> focus_peg(peg_position)

    TryGuess(guess) ->
      case mastermind.make_guess(model.game, guess) {
        Error(NoMoreGuesses) -> model
        Ok(#(outcome, new_game)) ->
          Model(..model, status: outcome, game: new_game)
          |> reset_pegs
          |> focus_first_empty_peg
          |> io.debug
      }
  }
}

/// Sets the value of a peg in a given position.
/// 
fn set_peg(model: Model, position: Int, value: Peg) -> Model {
  let new_pegs = {
    use index, peg <- list.index_map(model.pegs)
    use <- bool.guard(when: index != position, return: peg)
    #(Some(value), peg.1)
  }
  Model(..model, pegs: new_pegs)
}

/// Focuses the first non-focused empty peg, if all pegs are already full
/// no peg is focused.
/// 
fn focus_first_empty_peg(model: Model) -> Model {
  Model(..model, pegs: do_focus_first_unfocused(model.pegs, []))
}

fn do_focus_first_unfocused(
  values: List(#(Option(a), Focus)),
  acc: List(#(Option(a), Focus)),
) {
  case values {
    [] -> list.reverse(acc)
    [#(Some(_), _) as value, ..rest] | [#(_, Focused) as value, ..rest] ->
      do_focus_first_unfocused(rest, [value, ..acc])
    [#(None, NotFocused), ..rest] ->
      list.reverse(acc)
      |> list.append([#(None, Focused)])
      |> list.append(rest)
  }
}

/// Focuses a peg in a given position.
/// 
fn focus_peg(model: Model, position: Int) -> Model {
  let new_pegs = {
    use index, peg <- list.index_map(model.pegs)
    use <- bool.guard(when: index != position, return: peg)
    #(peg.0, Focused)
  }
  Model(..model, pegs: new_pegs)
}

/// Resets all pegs to their initial state: unfocused and with no value.
/// 
fn reset_pegs(model: Model) -> Model {
  Model(..model, pegs: list.repeat(#(None, NotFocused), 4))
}

/// Removes the focus from all_pegs.
/// 
fn unfocus_pegs(model: Model) -> Model {
  Model(..model, pegs: list.map(model.pegs, fn(peg) { #(peg.0, NotFocused) }))
}

/// Returns true if any of the pegs is in a focused status.
/// 
fn focused_peg_position(model: Model) -> Option(Int) {
  use res, #(_, focus), index <- list.index_fold(over: model.pegs, from: None)
  use <- option.lazy_or(res)
  case focus {
    Focused -> Some(index)
    NotFocused -> None
  }
}

/// If all pegs are set to a value, returns a `Guess` wrapped in `Ok`.
/// Otherwise returns `Nothing`, since a guess must be made of 4 pegs.
/// 
fn guess_from_model_pegs(model: Model) -> Option(Guess) {
  case model.pegs {
    [#(Some(peg1), _), #(Some(peg2), _), #(Some(peg3), _), #(Some(peg4), _)] ->
      Some(Guess(peg1, peg2, peg3, peg4))
    _ -> None
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Message) {
  let message = case model.status {
    Lose -> text("You lost! The solution was TODO")
    Win -> text("You won!")
    Continue ->
      case mastermind.remaining_guesses(model.game) {
        1 -> text("1 remaining guess")
        n -> text(int.to_string(n) <> " remaining guesses")
      }
  }

  let main_content = case model.status {
    Lose | Win -> stack([], [old_guesses(model)])
    Continue -> stack([stack.space("2em")], [old_guesses(model), picker(model)])
  }

  let controls = case model.status {
    Lose | Win -> stack([], [new_game_button(model)])
    Continue -> stack([], [make_a_guess_button(model), new_game_button(model)])
  }

  stack(
    [id("game"), stack.space("3em")],
    [elements(), theme(ui.base()), message, main_content, controls],
  )
}

// VIEW BUTTONS ----------------------------------------------------------------

fn new_game_button(model: Model) -> Element(Message) {
  let attributes = case model.status {
    Lose | Win -> [button.solid(), on_click(NewGame)]
    Continue -> [button.outline(), on_click(NewGame)]
  }
  button(attributes, [text("Start a new game")])
}

fn make_a_guess_button(model: Model) -> Element(Message) {
  let attributes = case model.status, guess_from_model_pegs(model) {
    Continue, Some(guess) -> [
      disabled(False),
      button.solid(),
      on_click(TryGuess(guess)),
    ]
    _, _ -> [disabled(True)]
  }
  button(attributes, [text("Make a guess")])
}

// VIEW OLD GUESSES ------------------------------------------------------------

fn old_guesses(model: Model) -> Element(nothing) {
  list.reverse(model.game.guesses)
  |> list.map(view_guess)
  |> stack([], _)
}

fn view_guess(guess: #(Guess, List(Hint))) -> Element(nothing) {
  let #(guess, hints) = guess

  let pegs =
    [guess.one, guess.two, guess.three, guess.four]
    |> list.map(old_guessed_peg)
    |> cluster([], _)

  let hints = cluster([], list.map(hints, guess_hint))

  cluster([cluster.loose()], [pegs, hints])
}

fn old_guessed_peg(peg: Peg) {
  span(
    [class(peg_to_color_class(Some(peg))), class("peg")],
    [text(peg_to_letter(Some(peg)))],
  )
}

fn guess_hint(hint: Hint) -> Element(nothing) {
  case hint {
    CorrectColor -> span([class("correct-color")], [text("c")])
    CorrectPosition -> span([class("correct-position")], [text("p")])
  }
}

// VIEW PICKER -----------------------------------------------------------------

fn picker(model: Model) -> Element(Message) {
  let guess_row =
    list.index_map(model.pegs, chosen_peg)
    |> cluster([], _)

  let content = case focused_peg_position(model) {
    None -> [guess_row]
    Some(position) -> [
      guess_row,
      [Red, Yellow, Green, Blue, Orange, Purple]
      |> list.map(choice_peg(_, position))
      |> cluster([], _),
    ]
  }

  stack([id("picker")], content)
}

fn chosen_peg(position: Int, peg: #(Option(Peg), Focus)) -> Element(Message) {
  let #(peg_value, peg_focus) = peg
  span(
    [
      class("peg"),
      class("chosen-peg"),
      class(peg_to_color_class(peg_value)),
      classes([#("focused-peg", peg_focus == Focused)]),
      on_click(FocusPeg(position)),
    ],
    [text(peg_to_letter(peg_value))],
  )
}

fn choice_peg(peg: Peg, position: Int) {
  span(
    [
      class("peg"),
      class("choice-peg"),
      class(peg_to_color_class(Some(peg))),
      on_click(SetPeg(position, peg)),
    ],
    [text(peg_to_letter(Some(peg)))],
  )
}

// VIEW GENERIC PEG HELPERS ----------------------------------------------------

fn peg_to_letter(peg: Option(Peg)) -> String {
  case peg {
    None -> "x"
    Some(Red) -> "R"
    Some(Yellow) -> "Y"
    Some(Green) -> "G"
    Some(Blue) -> "B"
    Some(Orange) -> "O"
    Some(Purple) -> "P"
  }
}

fn peg_to_color_class(peg: Option(Peg)) -> String {
  case peg {
    None -> "peg-empty"
    Some(Red) -> "peg-red"
    Some(Yellow) -> "peg-yellow"
    Some(Green) -> "peg-green"
    Some(Blue) -> "peg-blue"
    Some(Orange) -> "peg-orange"
    Some(Purple) -> "peg-purple"
  }
}
