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
  Green, Guess, Lose, NoMoreGuesses, Orange, Purple, Red, Win, Yellow,
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
  Model(
    peg1: #(Option(Peg), Focus),
    peg2: #(Option(Peg), Focus),
    peg3: #(Option(Peg), Focus),
    peg4: #(Option(Peg), Focus),
    status: GuessOutcome,
    game: Game,
  )
}

type Focus {
  Focused
  NotFocused
}

fn init(_: Nil) -> Model {
  Model(
    peg1: #(None, NotFocused),
    peg2: #(None, NotFocused),
    peg3: #(None, NotFocused),
    peg4: #(None, NotFocused),
    status: Continue,
    game: mastermind.random_game(),
  )
}

// UPDATE ----------------------------------------------------------------------

type Message {
  NewGame
  FocusPeg1
  FocusPeg2
  FocusPeg3
  FocusPeg4
  SetPeg1(Peg)
  SetPeg2(Peg)
  SetPeg3(Peg)
  SetPeg4(Peg)
  TryGuess(Guess)
}

fn update(model: Model, message: Message) -> Model {
  case message {
    NewGame -> init(Nil)
    SetPeg1(peg) -> Model(..model, peg1: #(Some(peg), NotFocused))
    SetPeg2(peg) -> Model(..model, peg2: #(Some(peg), NotFocused))
    SetPeg3(peg) -> Model(..model, peg3: #(Some(peg), NotFocused))
    SetPeg4(peg) -> Model(..model, peg4: #(Some(peg), NotFocused))
    FocusPeg1 -> Model(..unfocus_pegs(model), peg1: #(model.peg1.0, Focused))
    FocusPeg2 -> Model(..unfocus_pegs(model), peg2: #(model.peg2.0, Focused))
    FocusPeg3 -> Model(..unfocus_pegs(model), peg3: #(model.peg3.0, Focused))
    FocusPeg4 -> Model(..unfocus_pegs(model), peg4: #(model.peg4.0, Focused))
    TryGuess(guess) ->
      case mastermind.make_guess(model.game, guess) {
        Error(NoMoreGuesses) -> model
        Ok(#(outcome, new_game)) ->
          Model(..model, status: outcome, game: new_game)
          |> reset_pegs
          |> io.debug
      }
  }
}

/// Resets all pegs to their initial state: unfocused and with no value.
/// 
fn reset_pegs(model: Model) -> Model {
  Model(
    ..model,
    peg1: #(None, NotFocused),
    peg2: #(None, NotFocused),
    peg3: #(None, NotFocused),
    peg4: #(None, NotFocused),
  )
}

/// Removes the focus from all_pegs.
/// 
fn unfocus_pegs(model: Model) -> Model {
  Model(
    ..model,
    peg1: #(model.peg1.0, NotFocused),
    peg2: #(model.peg2.0, NotFocused),
    peg3: #(model.peg3.0, NotFocused),
    peg4: #(model.peg4.0, NotFocused),
  )
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
    list.map(
      [guess.one, guess.two, guess.three, guess.four],
      fn(peg) {
        li(
          [class(peg_to_color_class(Some(peg))), class("peg"), class("v-elem")],
          [text(peg_to_letter(Some(peg)))],
        )
      },
    )
  li([], [ul([class("pegs"), class("old-guesses"), class("v-stack")], pegs)])
}

/// View that displays the row containing the pegs that can be used to make
/// a guess.
/// 
/// When a peg is focused it displays a picker to choose the peg value.
/// 
fn peg_row(model: Model) -> Element(Message) {
  let pegs = [
    view_peg(model.peg1, FocusPeg1),
    view_peg(model.peg2, FocusPeg2),
    view_peg(model.peg3, FocusPeg3),
    view_peg(model.peg4, FocusPeg4),
  ]
  case focused_peg(model) {
    None -> ul([class("pegs"), class("guess-row")], pegs)
    Some(on_select) ->
      ul([class("pegs")], list.append(pegs, [peg_picker(on_select)]))
  }
}

fn view_peg(
  peg: #(Option(Peg), Focus),
  click_message: Message,
) -> Element(Message) {
  let #(peg_value, peg_focus) = peg
  li(
    [
      class(peg_to_color_class(peg_value)),
      class("peg"),
      classes([#("focused-peg", peg_focus == Focused)]),
      on_click(click_message),
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

fn focused_peg(model: Model) -> Option(fn(Peg) -> Message) {
  let get = fn(peg: #(Option(Peg), Focus), message) {
    case peg {
      #(_, Focused) -> Some(message)
      _ -> None
    }
  }
  use <- option.lazy_or(get(model.peg1, SetPeg1))
  use <- option.lazy_or(get(model.peg2, SetPeg2))
  use <- option.lazy_or(get(model.peg3, SetPeg3))
  use <- option.lazy_or(get(model.peg4, SetPeg4))
  None
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

fn peg_picker(on_select: fn(Peg) -> Message) -> Element(Message) {
  let all_pegs = [Red, Yellow, Green, Blue, Orange, Purple]
  let peg_pickers =
    list.map(
      all_pegs,
      fn(peg) {
        li(
          [
            class(peg_to_color_class(Some(peg))),
            class("peg"),
            on_click(on_select(peg)),
          ],
          [text(peg_to_letter(Some(peg)))],
        )
      },
    )
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
  use peg1 <- option.then(model.peg1.0)
  use peg2 <- option.then(model.peg2.0)
  use peg3 <- option.then(model.peg3.0)
  use peg4 <- option.then(model.peg4.0)
  Some(Guess(peg1, peg2, peg3, peg4))
}
