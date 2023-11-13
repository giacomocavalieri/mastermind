import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $lustre from "../lustre/lustre.mjs";
import * as $attribute from "../lustre/lustre/attribute.mjs";
import { class$, classes, disabled, id } from "../lustre/lustre/attribute.mjs";
import * as $element from "../lustre/lustre/element.mjs";
import { text } from "../lustre/lustre/element.mjs";
import * as $html from "../lustre/lustre/element/html.mjs";
import { span } from "../lustre/lustre/element/html.mjs";
import * as $event from "../lustre/lustre/event.mjs";
import { on_click } from "../lustre/lustre/event.mjs";
import * as $ui from "../lustre_ui/lustre/ui.mjs";
import * as $button from "../lustre_ui/lustre/ui/button.mjs";
import { button } from "../lustre_ui/lustre/ui/button.mjs";
import * as $cluster from "../lustre_ui/lustre/ui/cluster.mjs";
import { cluster } from "../lustre_ui/lustre/ui/cluster.mjs";
import * as $stack from "../lustre_ui/lustre/ui/stack.mjs";
import { stack } from "../lustre_ui/lustre/ui/stack.mjs";
import * as $styles from "../lustre_ui/lustre/ui/styles.mjs";
import { elements, theme } from "../lustre_ui/lustre/ui/styles.mjs";
import { Ok, toList, CustomType as $CustomType, makeError, isEqual } from "./gleam.mjs";
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
  Orange,
  Purple,
  Red,
  Win,
  Yellow,
} from "./mastermind.mjs";
import * as $ring from "./ring.mjs";
import { Focused } from "./ring.mjs";

class Model extends $CustomType {
  constructor(pegs, status, game) {
    super();
    this.pegs = pegs;
    this.status = status;
    this.game = game;
  }
}

export class NewGame extends $CustomType {}

export class SetFocusedPeg extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class FocusNextPeg extends $CustomType {}

export class FocusPreviousPeg extends $CustomType {}

export class MakeAGuess extends $CustomType {}

function init(_) {
  return new Model(
    $ring.new$(new None(), toList([new None(), new None(), new None()])),
    new Continue(),
    $mastermind.random_game(),
  );
}

function init_pegs() {
  return $ring.new$(new None(), toList([new None(), new None(), new None()]));
}

function guess_from_model_pegs(model) {
  let $ = $ring.to_list(model.pegs);
  if ($.hasLength(4) &&
  $.head instanceof Some &&
  $.tail.head instanceof Some &&
  $.tail.tail.head instanceof Some &&
  $.tail.tail.tail.head instanceof Some) {
    let peg1 = $.head[0];
    let peg2 = $.tail.head[0];
    let peg3 = $.tail.tail.head[0];
    let peg4 = $.tail.tail.tail.head[0];
    return new Some(new Guess(peg1, peg2, peg3, peg4));
  } else {
    return new None();
  }
}

function update(model, message) {
  if (message instanceof NewGame) {
    return init(undefined);
  } else if (message instanceof SetFocusedPeg) {
    let peg_value = message[0];
    let new_pegs = (() => {
      let _pipe = $ring.replace_focused(model.pegs, new Some(peg_value));
      return $ring.focus_next(_pipe);
    })();
    return model.withFields({ pegs: new_pegs });
  } else if (message instanceof FocusPreviousPeg) {
    return model.withFields({ pegs: $ring.focus_previous(model.pegs) });
  } else if (message instanceof FocusNextPeg) {
    return model.withFields({ pegs: $ring.focus_next(model.pegs) });
  } else if (message instanceof MakeAGuess) {
    let $ = guess_from_model_pegs(model);
    if ($ instanceof None) {
      return model;
    } else if ($ instanceof Some) {
      let guess = $[0];
      let $1 = $mastermind.make_guess(model.game, guess);
      if (!$1.isOk() && $1[0] instanceof NoMoreGuesses) {
        return model;
      } else if ($1.isOk()) {
        let outcome = $1[0][0];
        let new_game = $1[0][1];
        return new Model(init_pegs(), outcome, new_game);
      } else {
        throw makeError(
          "case_no_match",
          "app",
          80,
          "update",
          "No case clause matched",
          { values: [$1] }
        )
      }
    } else {
      throw makeError(
        "case_no_match",
        "app",
        77,
        "update",
        "No case clause matched",
        { values: [$] }
      )
    }
  } else {
    throw makeError(
      "case_no_match",
      "app",
      63,
      "update",
      "No case clause matched",
      { values: [message] }
    )
  }
}

export function key_to_event(value) {
  if (value === "Enter") {
    return new Ok(new MakeAGuess());
  } else if (value === "ArrowLeft") {
    return new Ok(new FocusPreviousPeg());
  } else if (value === "ArrowRight") {
    return new Ok(new FocusNextPeg());
  } else {
    return $result.map(
      $mastermind.parse_peg(value),
      (var0) => { return new SetFocusedPeg(var0); },
    );
  }
}

function new_game_button(model) {
  let attributes = (() => {
    let $ = model.status;
    if ($ instanceof Lose) {
      return toList([$button.solid(), on_click(new NewGame())]);
    } else if ($ instanceof Win) {
      return toList([$button.solid(), on_click(new NewGame())]);
    } else if ($ instanceof Continue) {
      return toList([$button.outline(), on_click(new NewGame())]);
    } else {
      throw makeError(
        "case_no_match",
        "app",
        143,
        "new_game_button",
        "No case clause matched",
        { values: [$] }
      )
    }
  })();
  return button(attributes, toList([text("Start a new game")]));
}

function make_a_guess_button(model) {
  let attributes = (() => {
    let $ = model.status;
    let $1 = guess_from_model_pegs(model);
    if ($ instanceof Continue && $1 instanceof Some) {
      return toList([
        disabled(false),
        $button.solid(),
        on_click(new MakeAGuess()),
      ]);
    } else {
      return toList([disabled(true)]);
    }
  })();
  return button(attributes, toList([text("Make a guess")]));
}

function guess_hint(hint) {
  if (hint instanceof CorrectColor) {
    return span(toList([class$("correct-color")]), toList([text("c")]));
  } else if (hint instanceof CorrectPosition) {
    return span(toList([class$("correct-position")]), toList([text("p")]));
  } else {
    throw makeError(
      "case_no_match",
      "app",
      187,
      "guess_hint",
      "No case clause matched",
      { values: [hint] }
    )
  }
}

function peg_to_letter(peg) {
  if (peg instanceof None) {
    return "x";
  } else if (peg instanceof Some && peg[0] instanceof Red) {
    return "R";
  } else if (peg instanceof Some && peg[0] instanceof Yellow) {
    return "Y";
  } else if (peg instanceof Some && peg[0] instanceof Green) {
    return "G";
  } else if (peg instanceof Some && peg[0] instanceof Blue) {
    return "B";
  } else if (peg instanceof Some && peg[0] instanceof Orange) {
    return "O";
  } else if (peg instanceof Some && peg[0] instanceof Purple) {
    return "P";
  } else {
    throw makeError(
      "case_no_match",
      "app",
      239,
      "peg_to_letter",
      "No case clause matched",
      { values: [peg] }
    )
  }
}

function peg_to_color_class(peg) {
  if (peg instanceof None) {
    return "peg-empty";
  } else if (peg instanceof Some && peg[0] instanceof Red) {
    return "peg-red";
  } else if (peg instanceof Some && peg[0] instanceof Yellow) {
    return "peg-yellow";
  } else if (peg instanceof Some && peg[0] instanceof Green) {
    return "peg-green";
  } else if (peg instanceof Some && peg[0] instanceof Blue) {
    return "peg-blue";
  } else if (peg instanceof Some && peg[0] instanceof Orange) {
    return "peg-orange";
  } else if (peg instanceof Some && peg[0] instanceof Purple) {
    return "peg-purple";
  } else {
    throw makeError(
      "case_no_match",
      "app",
      251,
      "peg_to_color_class",
      "No case clause matched",
      { values: [peg] }
    )
  }
}

function old_guessed_peg(peg) {
  return span(
    toList([class$(peg_to_color_class(new Some(peg))), class$("peg")]),
    toList([text(peg_to_letter(new Some(peg)))]),
  );
}

function view_guess(guess) {
  let guess$1 = guess[0];
  let hints = guess[1];
  let pegs = (() => {
    let _pipe = toList([guess$1.one, guess$1.two, guess$1.three, guess$1.four]);
    let _pipe$1 = $list.map(_pipe, old_guessed_peg);
    return ((_capture) => { return cluster(toList([]), _capture); })(_pipe$1);
  })();
  let hints$1 = cluster(toList([]), $list.map(hints, guess_hint));
  return cluster(toList([$cluster.space("3rem")]), toList([pegs, hints$1]));
}

function old_guesses(model) {
  let _pipe = $list.reverse(model.game.guesses);
  let _pipe$1 = $list.map(_pipe, view_guess);
  return ((_capture) => { return stack(toList([]), _capture); })(_pipe$1);
}

function chosen_peg(peg) {
  let peg_value = peg[0];
  let peg_focus = peg[1];
  return span(
    toList([
      class$("peg"),
      class$("chosen-peg"),
      class$(peg_to_color_class(peg_value)),
      classes(toList([["focused-peg", isEqual(peg_focus, new Focused())]])),
    ]),
    toList([text(peg_to_letter(peg_value))]),
  );
}

function choice_peg(peg) {
  return span(
    toList([
      class$("peg"),
      class$("choice-peg"),
      class$(peg_to_color_class(new Some(peg))),
      on_click(new SetFocusedPeg(peg)),
    ]),
    toList([text(peg_to_letter(new Some(peg)))]),
  );
}

function picker(model) {
  let guess_row = (() => {
    let _pipe = $list.map($ring.to_focus_list(model.pegs), chosen_peg);
    return ((_capture) => { return cluster(toList([]), _capture); })(_pipe);
  })();
  return stack(
    toList([id("picker")]),
    toList([
      guess_row,
      (() => {
        let _pipe = toList([
          new Red(),
          new Yellow(),
          new Green(),
          new Blue(),
          new Orange(),
          new Purple(),
        ]);
        let _pipe$1 = $list.map(_pipe, choice_peg);
        return ((_capture) => { return cluster(toList([]), _capture); })(
          _pipe$1,
        );
      })(),
    ]),
  );
}

function view(model) {
  let message = (() => {
    let $ = model.status;
    if ($ instanceof Lose) {
      return text("You lost! The solution was TODO");
    } else if ($ instanceof Win) {
      return text("You won!");
    } else if ($ instanceof Continue) {
      let $1 = $mastermind.remaining_guesses(model.game);
      if ($1 === 1) {
        return text("1 remaining guess");
      } else {
        let n = $1;
        return text($int.to_string(n) + " remaining guesses");
      }
    } else {
      throw makeError(
        "case_no_match",
        "app",
        114,
        "view",
        "No case clause matched",
        { values: [$] }
      )
    }
  })();
  let main_content = (() => {
    let $ = model.status;
    if ($ instanceof Lose) {
      return stack(toList([]), toList([old_guesses(model)]));
    } else if ($ instanceof Win) {
      return stack(toList([]), toList([old_guesses(model)]));
    } else if ($ instanceof Continue) {
      return stack(
        toList([$stack.space("2em")]),
        toList([old_guesses(model), picker(model)]),
      );
    } else {
      throw makeError(
        "case_no_match",
        "app",
        124,
        "view",
        "No case clause matched",
        { values: [$] }
      )
    }
  })();
  let controls = (() => {
    let $ = model.status;
    if ($ instanceof Lose) {
      return stack(toList([]), toList([new_game_button(model)]));
    } else if ($ instanceof Win) {
      return stack(toList([]), toList([new_game_button(model)]));
    } else if ($ instanceof Continue) {
      return stack(
        toList([]),
        toList([make_a_guess_button(model), new_game_button(model)]),
      );
    } else {
      throw makeError(
        "case_no_match",
        "app",
        129,
        "view",
        "No case clause matched",
        { values: [$] }
      )
    }
  })();
  return stack(
    toList([id("game"), $stack.space("3em")]),
    toList([elements(), theme($ui.base()), message, main_content, controls]),
  );
}

export function main() {
  let app = $lustre.simple(init, update, view);
  let $ = $lustre.start(app, "[data-lustre-app]", undefined);
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "app",
      27,
      "main",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let dispatch = $[0];
  return dispatch;
}
