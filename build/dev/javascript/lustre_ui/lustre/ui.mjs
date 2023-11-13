import * as $attribute from "../../lustre/lustre/attribute.mjs";
import { attribute } from "../../lustre/lustre/attribute.mjs";
import { CustomType as $CustomType, makeError } from "../gleam.mjs";
import * as $colour from "../lustre/ui/colour.mjs";

export class Theme extends $CustomType {
  constructor(primary, greyscale, error, warning, success, info) {
    super();
    this.primary = primary;
    this.greyscale = greyscale;
    this.error = error;
    this.warning = warning;
    this.success = success;
    this.info = info;
  }
}

export class Primary extends $CustomType {}

export class Greyscale extends $CustomType {}

export class Error extends $CustomType {}

export class Warning extends $CustomType {}

export class Success extends $CustomType {}

export class Info extends $CustomType {}

export function base() {
  return new Theme(
    $colour.iris(),
    $colour.slate(),
    $colour.red(),
    $colour.yellow(),
    $colour.green(),
    $colour.blue(),
  );
}

export function variant(variant) {
  return attribute(
    "data-variant",
    (() => {
      if (variant instanceof Primary) {
        return "primary";
      } else if (variant instanceof Greyscale) {
        return "greyscale";
      } else if (variant instanceof Error) {
        return "error";
      } else if (variant instanceof Warning) {
        return "warning";
      } else if (variant instanceof Success) {
        return "success";
      } else if (variant instanceof Info) {
        return "info";
      } else {
        throw makeError(
          "case_no_match",
          "lustre/ui",
          66,
          "",
          "No case clause matched",
          { values: [variant] }
        )
      }
    })(),
  );
}
