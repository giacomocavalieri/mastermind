import gleam/dynamic.{type DecodeError}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lustre
import lustre/attribute.{class, classes, disabled, id}
import lustre/element.{type Element, text}
import lustre/element/html.{span}
import lustre/event.{on_click}
import lustre/ui
import lustre/ui/button.{button}
import lustre/ui/cluster.{cluster}
import lustre/ui/stack.{stack}
import lustre/ui/styles.{elements, theme}
import mastermind.{
  type Game, type Guess, type GuessOutcome, type Hint, type Peg, type SecretCode,
  Blue, Continue, CorrectColor, CorrectPosition, Green, Guess, Lose,
  NoMoreGuesses, Orange, Purple, Red, Win, Yellow,
}
import ring.{type Focus, type Ring, Focused}

// APPLICATION ENTRY POINT -----------------------------------------------------

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(dispatch) = lustre.start(app, "[data-lustre-app]", Nil)
  dispatch
}

// MODEL -----------------------------------------------------------------------

/// The state of the game, keeping track of the outcome of the game and the
/// current colors chosen for each peg.
/// 
type Model {
  Model(pegs: Ring(Option(Peg)), status: GuessOutcome, game: Game)
}

fn init(_: Nil) -> Model {
  Model(
    pegs: ring.new(None, [None, None, None]),
    status: Continue,
    game: mastermind.random_game(),
  )
}

fn init_pegs() -> Ring(Option(Peg)) {
  ring.new(None, [None, None, None])
}

// UPDATE ----------------------------------------------------------------------

pub type Message {
  NewGame
  SetFocusedPeg(Peg)
  FocusNextPeg
  FocusPreviousPeg
  MakeAGuess
}

fn update(model: Model, message: Message) -> Model {
  case message {
    NewGame -> init(Nil)

    SetFocusedPeg(peg_value) -> {
      let new_pegs =
        ring.replace_focused(model.pegs, Some(peg_value))
        |> ring.focus_next
      Model(..model, pegs: new_pegs)
    }

    FocusPreviousPeg -> Model(..model, pegs: ring.focus_previous(model.pegs))
    FocusNextPeg -> Model(..model, pegs: ring.focus_next(model.pegs))

    MakeAGuess ->
      case guess_from_model_pegs(model) {
        None -> model
        Some(guess) ->
          case mastermind.make_guess(model.game, guess) {
            Error(NoMoreGuesses) -> model
            Ok(#(outcome, new_game)) ->
              Model(status: outcome, game: new_game, pegs: init_pegs())
          }
      }
  }
}

/// If all pegs are set to a value, returns a `Guess` wrapped in `Ok`.
/// Otherwise returns `Nothing`, since a guess must be made of 4 pegs.
/// 
fn guess_from_model_pegs(model: Model) -> Option(Guess) {
  case ring.to_list(model.pegs) {
    [Some(peg1), Some(peg2), Some(peg3), Some(peg4)] ->
      Some(Guess(peg1, peg2, peg3, peg4))
    _ -> None
  }
}

// DECODE KEYDOWN MESSAGE ------------------------------------------------------

pub fn key_to_event(value: String) -> Result(Message, List(DecodeError)) {
  case value {
    "Enter" -> Ok(MakeAGuess)
    "ArrowLeft" -> Ok(FocusPreviousPeg)
    "ArrowRight" -> Ok(FocusNextPeg)
    _ -> result.map(mastermind.parse_peg(value), SetFocusedPeg)
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Message) {
  let main_content = case model.status {
    Lose | Win -> stack([], [old_guesses(model)])
    Continue -> stack([stack.space("2em")], [old_guesses(model), picker(model)])
  }

  let message = case model.status {
    Lose -> losing_message(model)
    Win -> text("You won!")
    Continue ->
      case mastermind.remaining_guesses(model.game) {
        1 -> text("1 remaining guess")
        n -> text(int.to_string(n) <> " remaining guesses")
      }
  }

  let controls = case model.status {
    Lose | Win -> stack([], [new_game_button(model)])
    Continue -> stack([], [make_a_guess_button(model), new_game_button(model)])
  }

  stack(
    [id("game"), stack.space("3em")],
    [elements(), theme(ui.base()), main_content, message, controls],
  )
}

fn losing_message(model: Model) -> Element(nothing) {
  let solution =
    mastermind.secret_code(model.game)
    |> secret_code_to_list
    |> list.map(old_guessed_peg)
    |> cluster([], _)

  stack([], [text("You lost! The solution was"), solution])
}

fn secret_code_to_list(code: SecretCode) -> List(Peg) {
  [code.one, code.two, code.three, code.four]
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
    Continue, Some(_) -> [disabled(False), button.solid(), on_click(MakeAGuess)]
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

  cluster([cluster.space("3rem")], [pegs, hints])
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
    list.map(ring.to_focus_list(model.pegs), chosen_peg)
    |> cluster([], _)

  stack(
    [id("picker")],
    [
      guess_row,
      [Red, Yellow, Green, Blue, Orange, Purple]
      |> list.map(choice_peg)
      |> cluster([], _),
    ],
  )
}

fn chosen_peg(peg: #(Option(Peg), Focus)) -> Element(Message) {
  let #(peg_value, peg_focus) = peg
  span(
    [
      class("peg"),
      class("chosen-peg"),
      class(peg_to_color_class(peg_value)),
      classes([#("focused-peg", peg_focus == Focused)]),
    ],
    [text(peg_to_letter(peg_value))],
  )
}

fn choice_peg(peg: Peg) {
  span(
    [
      class("peg"),
      class("choice-peg"),
      class(peg_to_color_class(Some(peg))),
      on_click(SetFocusedPeg(peg)),
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
