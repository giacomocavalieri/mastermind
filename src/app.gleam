import gleam/bool
import gleam/int
import gleam/list
import gleam/io
import gleam/option.{type Option, None, Some}
import lustre
import lustre/attribute.{class, classes, disabled}
import lustre/event.{on_click}
import lustre/element.{type Element, text}
import lustre/element/html.{button, div, li, ul}
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

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Message) {
  case model.status {
    Win -> winning_view(model)
    Lose -> losing_view(model)
    Continue -> playing_view(model)
  }
}

/// View displayd when the player has lost.
/// 
fn losing_view(model: Model) -> Element(Message) {
  let message = text("You lost! The solution was TODO")
  div([], [message, guesses(model), new_game_button(model)])
}

/// View displayed when the player has won.
/// 
fn winning_view(model: Model) -> Element(Message) {
  let message = text("You won!")
  div([], [message, guesses(model), new_game_button(model)])
}

/// Button to start a new game from scratch.
/// 
fn new_game_button(model: Model) -> Element(Message) {
  let should_be_faded = model.status == Continue
  button(
    [on_click(NewGame), classes([#("slightly-faded", should_be_faded)])],
    [text("Start a new game!")],
  )
}

/// View displayed when the game is still going.
/// 
fn playing_view(model: Model) -> Element(Message) {
  div(
    [class("v-stack")],
    [
      remaining_guesses(model),
      guesses(model),
      peg_row(model),
      ul(
        [class("v-stack")],
        [
          li([class("v-elem")], [guess_button(model)]),
          li([class("v-elem")], [new_game_button(model)]),
        ],
      ),
    ],
  )
}

fn remaining_guesses(model: Model) -> Element(nothing) {
  case mastermind.remaining_guesses(model.game) {
    1 -> text("1 remaining guess")
    n -> text(int.to_string(n) <> " remaining guesses")
  }
}

/// View that displays all the previous guesses taken by the player.
/// 
fn guesses(model: Model) -> Element(nothing) {
  ul(
    [class("guesses"), class("v-stack")],
    list.map(list.reverse(model.game.guesses), view_guess),
  )
}

fn view_guess(guess: #(Guess, List(Hint))) -> Element(nothing) {
  let #(guess, hints) = guess
  let pegs =
    [guess.one, guess.two, guess.three, guess.four]
    |> list.map(fn(peg) {
      div(
        [],
        [
          li(
            [
              class(peg_to_color_class(Some(peg))),
              class("peg"),
              class("v-elem"),
            ],
            [text(peg_to_letter(Some(peg)))],
          ),
        ],
      )
    })
  li(
    [],
    [
      ul([class("pegs"), class("old-guesses"), class("v-stack")], pegs),
      view_hints(hints),
    ],
  )
}

fn view_hints(hints: List(Hint)) -> Element(nothing) {
  let hints =
    list.map(
      hints,
      fn(hint) {
        case hint {
          CorrectColor -> li([class("color-hint")], [text("c")])
          CorrectPosition -> li([class("color-position")], [text("p")])
        }
      },
    )
  ul([class("hints")], hints)
}

/// View that displays the row containing the pegs that can be used to make
/// a guess.
/// 
/// When a peg is focused it displays a picker to choose the peg value.
/// 
fn peg_row(model: Model) -> Element(Message) {
  let pegs = list.index_map(model.pegs, view_peg)
  case focused_peg_position(model) {
    None -> ul([class("pegs"), class("guess-row")], pegs)
    Some(position) ->
      ul([class("pegs")], list.append(pegs, [peg_picker(position)]))
  }
}

fn view_peg(peg_position: Int, peg: #(Option(Peg), Focus)) -> Element(Message) {
  let #(peg_value, peg_focus) = peg
  li(
    [
      class(peg_to_color_class(peg_value)),
      class("peg"),
      classes([#("focused-peg", peg_focus == Focused)]),
      on_click(FocusPeg(peg_position)),
    ],
    [text(peg_to_letter(peg_value))],
  )
}

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

fn peg_picker(position: Int) -> Element(Message) {
  let all_pegs = [Red, Yellow, Green, Blue, Orange, Purple]
  let peg_pickers =
    all_pegs
    |> list.map(fn(peg) {
      li(
        [
          class(peg_to_color_class(Some(peg))),
          class("peg"),
          on_click(SetPeg(position, peg)),
        ],
        [text(peg_to_letter(Some(peg)))],
      )
    })
  ul([class("peg-picker")], peg_pickers)
}

/// Button to submit a new guess.
/// 
fn guess_button(model: Model) -> Element(Message) {
  let attributes = case model.status, guess_from_model_pegs(model) {
    Continue, Some(guess) -> [disabled(False), on_click(TryGuess(guess))]
    _, _ -> [disabled(True)]
  }
  button(attributes, [text("Make a guess!")])
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
