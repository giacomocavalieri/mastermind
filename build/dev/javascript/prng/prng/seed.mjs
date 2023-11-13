import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import { new_seed as new$ } from "../ffi.mjs";

export { new$ };

export function random() {
  return new$($int.random(0, 4_294_967_296));
}
