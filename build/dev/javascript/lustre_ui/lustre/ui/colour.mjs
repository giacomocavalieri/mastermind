import * as $colour from "../../../gleam_community_colour/gleam_community/colour.mjs";
import { CustomType as $CustomType, makeError } from "../../gleam.mjs";

export class Scale extends $CustomType {
  constructor(app_background, app_background_subtle, app_border, element_background, element_background_hover, element_background_strong, element_border_subtle, element_border_strong, solid_background, solid_background_hover, text_high_contrast, text_low_contrast) {
    super();
    this.app_background = app_background;
    this.app_background_subtle = app_background_subtle;
    this.app_border = app_border;
    this.element_background = element_background;
    this.element_background_hover = element_background_hover;
    this.element_background_strong = element_background_strong;
    this.element_border_subtle = element_border_subtle;
    this.element_border_strong = element_border_strong;
    this.solid_background = solid_background;
    this.solid_background_hover = solid_background_hover;
    this.text_high_contrast = text_high_contrast;
    this.text_low_contrast = text_low_contrast;
  }
}

function from_radix_scale(a, b, c, d, e, f, g, h, i, j, k, l) {
  let $ = $colour.from_rgb_hex(a);
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "lustre/ui/colour",
      56,
      "from_radix_scale",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let app_background = $[0];
  let $1 = $colour.from_rgb_hex(b);
  if (!$1.isOk()) {
    throw makeError(
      "assignment_no_match",
      "lustre/ui/colour",
      57,
      "from_radix_scale",
      "Assignment pattern did not match",
      { value: $1 }
    )
  }
  let app_background_subtle = $1[0];
  let $2 = $colour.from_rgb_hex(c);
  if (!$2.isOk()) {
    throw makeError(
      "assignment_no_match",
      "lustre/ui/colour",
      58,
      "from_radix_scale",
      "Assignment pattern did not match",
      { value: $2 }
    )
  }
  let app_border = $2[0];
  let $3 = $colour.from_rgb_hex(d);
  if (!$3.isOk()) {
    throw makeError(
      "assignment_no_match",
      "lustre/ui/colour",
      59,
      "from_radix_scale",
      "Assignment pattern did not match",
      { value: $3 }
    )
  }
  let element_background = $3[0];
  let $4 = $colour.from_rgb_hex(e);
  if (!$4.isOk()) {
    throw makeError(
      "assignment_no_match",
      "lustre/ui/colour",
      60,
      "from_radix_scale",
      "Assignment pattern did not match",
      { value: $4 }
    )
  }
  let element_background_hover = $4[0];
  let $5 = $colour.from_rgb_hex(f);
  if (!$5.isOk()) {
    throw makeError(
      "assignment_no_match",
      "lustre/ui/colour",
      61,
      "from_radix_scale",
      "Assignment pattern did not match",
      { value: $5 }
    )
  }
  let element_background_strong = $5[0];
  let $6 = $colour.from_rgb_hex(g);
  if (!$6.isOk()) {
    throw makeError(
      "assignment_no_match",
      "lustre/ui/colour",
      62,
      "from_radix_scale",
      "Assignment pattern did not match",
      { value: $6 }
    )
  }
  let element_border_strong = $6[0];
  let $7 = $colour.from_rgb_hex(h);
  if (!$7.isOk()) {
    throw makeError(
      "assignment_no_match",
      "lustre/ui/colour",
      63,
      "from_radix_scale",
      "Assignment pattern did not match",
      { value: $7 }
    )
  }
  let element_border_subtle = $7[0];
  let $8 = $colour.from_rgb_hex(i);
  if (!$8.isOk()) {
    throw makeError(
      "assignment_no_match",
      "lustre/ui/colour",
      64,
      "from_radix_scale",
      "Assignment pattern did not match",
      { value: $8 }
    )
  }
  let solid_background = $8[0];
  let $9 = $colour.from_rgb_hex(j);
  if (!$9.isOk()) {
    throw makeError(
      "assignment_no_match",
      "lustre/ui/colour",
      65,
      "from_radix_scale",
      "Assignment pattern did not match",
      { value: $9 }
    )
  }
  let solid_background_hover = $9[0];
  let $10 = $colour.from_rgb_hex(k);
  if (!$10.isOk()) {
    throw makeError(
      "assignment_no_match",
      "lustre/ui/colour",
      66,
      "from_radix_scale",
      "Assignment pattern did not match",
      { value: $10 }
    )
  }
  let text_high_contrast = $10[0];
  let $11 = $colour.from_rgb_hex(l);
  if (!$11.isOk()) {
    throw makeError(
      "assignment_no_match",
      "lustre/ui/colour",
      67,
      "from_radix_scale",
      "Assignment pattern did not match",
      { value: $11 }
    )
  }
  let text_low_contrast = $11[0];
  return new Scale(
    app_background,
    app_background_subtle,
    app_border,
    element_background,
    element_background_hover,
    element_background_strong,
    element_border_subtle,
    element_border_strong,
    solid_background,
    solid_background_hover,
    text_high_contrast,
    text_low_contrast,
  );
}

export function grey() {
  return from_radix_scale(
    0xFCFCFC,
    0xF9F9F9,
    0xDDDDDD,
    0xF1F1F1,
    0xEBEBEB,
    0xE4E4E4,
    0xBBBBBB,
    0xD4D4D4,
    0x8D8D8D,
    0x808080,
    0x202020,
    0x646464,
  );
}

export function mauve() {
  return from_radix_scale(
    0xFDFCFD,
    0xFAF9FB,
    0xDFDCE3,
    0xF3F1F5,
    0xECEAEF,
    0xE6E3E9,
    0xBCBAC7,
    0xD5D3DB,
    0x8E8C99,
    0x817F8B,
    0x211F26,
    0x65636D,
  );
}

export function slate() {
  return from_radix_scale(
    0xFCFCFD,
    0xF9F9FB,
    0xDDDDE3,
    0xF2F2F5,
    0xEBEBEF,
    0xE4E4E9,
    0xB9BBC6,
    0xD3D4DB,
    0x8B8D98,
    0x7E808A,
    0x1C2024,
    0x60646C,
  );
}

export function sage() {
  return from_radix_scale(
    0xFBFDFC,
    0xF7F9F8,
    0xDCDFDD,
    0xF0F2F1,
    0xE9ECEB,
    0xE3E6E4,
    0xB8BCBA,
    0xD2D5D3,
    0x868E8B,
    0x7A817F,
    0x1A211E,
    0x5F6563,
  );
}

export function olive() {
  return from_radix_scale(
    0xFCFDFC,
    0xF8FAF8,
    0xDBDEDB,
    0xF1F3F1,
    0xEAECEA,
    0xE3E5E3,
    0xB9BCB8,
    0xD2D4D1,
    0x898E87,
    0x7C817B,
    0x1D211C,
    0x60655F,
  );
}

export function sand() {
  return from_radix_scale(
    0xFDFDFC,
    0xF9F9F8,
    0xDDDDDA,
    0xF2F2F0,
    0xEBEBE9,
    0xE4E4E2,
    0xBCBBB5,
    0xD3D2CE,
    0x8D8D86,
    0x80807A,
    0x21201C,
    0x63635E,
  );
}

export function gold() {
  return from_radix_scale(
    0xFDFDFC,
    0xFBF9F2,
    0xDAD1BD,
    0xF5F2E9,
    0xEEEADD,
    0xE5DFD0,
    0xB8A383,
    0xCBBDA4,
    0x978365,
    0x89775C,
    0x3B352B,
    0x71624B,
  );
}

export function bronze() {
  return from_radix_scale(
    0xFDFCFC,
    0xFDF8F6,
    0xE0CEC7,
    0xF8F1EE,
    0xF2E8E4,
    0xEADDD7,
    0xBFA094,
    0xD1B9B0,
    0xA18072,
    0x947467,
    0x43302B,
    0x7D5E54,
  );
}

export function brown() {
  return from_radix_scale(
    0xFEFDFC,
    0xFCF9F6,
    0xE8CDB5,
    0xF8F1EA,
    0xF4E9DD,
    0xEFDDCC,
    0xD09E72,
    0xDDB896,
    0xAD7F58,
    0x9E7352,
    0x3E332E,
    0x815E46,
  );
}

export function yellow() {
  return from_radix_scale(
    0xFDFDF9,
    0xFFFBE0,
    0xECDD85,
    0xFFF8C6,
    0xFCF3AF,
    0xF7EA9B,
    0xC9AA45,
    0xDAC56E,
    0xFBE32D,
    0xF9DA10,
    0x473B1F,
    0x775F28,
  );
}

export function amber() {
  return from_radix_scale(
    0xFEFDFB,
    0xFFF9ED,
    0xF5D08C,
    0xFFF3D0,
    0xFFECB7,
    0xFFE0A1,
    0xD6A35C,
    0xE4BB78,
    0xFFC53D,
    0xFFBA1A,
    0x4F3422,
    0x915930,
  );
}

export function orange() {
  return from_radix_scale(
    0xFEFCFB,
    0xFFF8F4,
    0xFFC291,
    0xFFEDD5,
    0xFFE0BB,
    0xFFD3A4,
    0xED8A5C,
    0xFFAA7D,
    0xF76808,
    0xED5F00,
    0x582D1D,
    0x99543A,
  );
}

export function tomato() {
  return from_radix_scale(
    0xFFFCFC,
    0xFFF8F7,
    0xFAC7BE,
    0xFFF0EE,
    0xFFE6E2,
    0xFDD8D3,
    0xEA9280,
    0xF3B0A2,
    0xE54D2E,
    0xD84224,
    0x5C271F,
    0xC33113,
  );
}

export function red() {
  return from_radix_scale(
    0xFFFCFC,
    0xFFF7F7,
    0xF9C6C6,
    0xFFEFEF,
    0xFFE5E5,
    0xFDD8D8,
    0xEB9091,
    0xF3AEAF,
    0xE5484D,
    0xD93D42,
    0x641723,
    0xC62A2F,
  );
}

export function ruby() {
  return from_radix_scale(
    0xFFFCFD,
    0xFFF7F9,
    0xF5C7D1,
    0xFEEFF3,
    0xFDE5EA,
    0xFAD8E0,
    0xE592A2,
    0xEEAFBC,
    0xE54666,
    0xDA3A5C,
    0x64172B,
    0xCA244D,
  );
}

export function crimson() {
  return from_radix_scale(
    0xFFFCFD,
    0xFFF7FB,
    0xF4C6DB,
    0xFEEFF6,
    0xFCE5F0,
    0xF9D8E7,
    0xE58FB1,
    0xEDADC8,
    0xE93D82,
    0xDC3175,
    0x621639,
    0xCB1D63,
  );
}

export function pink() {
  return from_radix_scale(
    0xFFFCFE,
    0xFFF7FC,
    0xF3C6E2,
    0xFEEEF8,
    0xFCE5F3,
    0xF9D8EC,
    0xE38EC3,
    0xECADD4,
    0xD6409F,
    0xCD3093,
    0x651249,
    0xC41C87,
  );
}

export function plum() {
  return from_radix_scale(
    0xFEFCFF,
    0xFFF8FF,
    0xEBC8ED,
    0xFCEFFC,
    0xF9E5F9,
    0xF3D9F4,
    0xCF91D8,
    0xDFAFE3,
    0xAB4ABA,
    0xA43CB4,
    0x53195D,
    0x9C2BAD,
  );
}

export function purple() {
  return from_radix_scale(
    0xFEFCFE,
    0xFDFAFF,
    0xE3CCF4,
    0xF9F1FE,
    0xF3E7FC,
    0xEDDBF9,
    0xBE93E4,
    0xD3B4ED,
    0x8E4EC6,
    0x8445BC,
    0x402060,
    0x793AAF,
  );
}

export function violet() {
  return from_radix_scale(
    0xFDFCFE,
    0xFBFAFF,
    0xD7CFF9,
    0xF5F2FF,
    0xEDE9FE,
    0xE4DEFC,
    0xAA99EC,
    0xC4B8F3,
    0x6E56CF,
    0x644FC1,
    0x2F265F,
    0x5746AF,
  );
}

export function iris() {
  return from_radix_scale(
    0xFDFDFF,
    0xFAFAFF,
    0xD0D0FA,
    0xF3F3FF,
    0xEBEBFE,
    0xE0E0FD,
    0x9B9EF0,
    0xBABBF5,
    0x5B5BD6,
    0x5353CE,
    0x272962,
    0x4747C2,
  );
}

export function indigo() {
  return from_radix_scale(
    0xFDFDFE,
    0xF8FAFF,
    0xC6D4F9,
    0xF0F4FF,
    0xE6EDFE,
    0xD9E2FC,
    0x8DA4EF,
    0xAEC0F5,
    0x3E63DD,
    0x3A5CCC,
    0x1F2D5C,
    0x3451B2,
  );
}

export function blue() {
  return from_radix_scale(
    0xFBFDFF,
    0xF5FAFF,
    0xB7D9F8,
    0xEDF6FF,
    0xE1F0FF,
    0xCEE7FE,
    0x5EB0EF,
    0x96C7F2,
    0x0091FF,
    0x0880EA,
    0x113264,
    0x0B68CB,
  );
}

export function cyan() {
  return from_radix_scale(
    0xFAFDFE,
    0xF2FCFD,
    0xAADEE6,
    0xE7F9FB,
    0xD8F3F6,
    0xC4EAEF,
    0x3DB9CF,
    0x84CDDA,
    0x05A2C2,
    0x0894B3,
    0x0D3C48,
    0x0C7792,
  );
}

export function teal() {
  return from_radix_scale(
    0xFAFEFD,
    0xF1FCFA,
    0xAFDFD7,
    0xE7F9F5,
    0xD9F3EE,
    0xC7EBE5,
    0x53B9AB,
    0x8DCEC3,
    0x12A594,
    0x0E9888,
    0x0D3D38,
    0x067A6F,
  );
}

export function jade() {
  return from_radix_scale(
    0xFBFEFD,
    0xEFFDF6,
    0xB0E0CC,
    0xE4FAEF,
    0xD7F4E6,
    0xC6ECDB,
    0x56BA9F,
    0x8FCFB9,
    0x29A383,
    0x259678,
    0x1D3B31,
    0x1A7A5E,
  );
}

export function green() {
  return from_radix_scale(
    0xFBFEFC,
    0xF2FCF5,
    0xB4DFC4,
    0xE9F9EE,
    0xDDF3E4,
    0xCCEBD7,
    0x5BB98C,
    0x92CEAC,
    0x30A46C,
    0x299764,
    0x193B2D,
    0x18794E,
  );
}

export function grass() {
  return from_radix_scale(
    0xFBFEFB,
    0xF3FCF3,
    0xB7DFBA,
    0xEBF9EB,
    0xDFF3DF,
    0xCEEBCF,
    0x65BA75,
    0x97CF9C,
    0x46A758,
    0x3D9A50,
    0x203C25,
    0x297C3B,
  );
}

export function lime() {
  return from_radix_scale(
    0xFCFDFA,
    0xF7FCF0,
    0xC6DE99,
    0xEDFADA,
    0xE2F5C4,
    0xD5EDAF,
    0x9AB654,
    0xB2CA7F,
    0xBDEE63,
    0xB0E64C,
    0x37401C,
    0x59682C,
  );
}

export function mint() {
  return from_radix_scale(
    0xF9FEFD,
    0xEFFEFA,
    0xA6E1D3,
    0xDDFBF3,
    0xCCF7EC,
    0xBBEEE2,
    0x51BDA7,
    0x87D0BF,
    0x86EAD4,
    0x7FE1CC,
    0x16433C,
    0x27756A,
  );
}

export function sky() {
  return from_radix_scale(
    0xF9FEFF,
    0xF1FCFF,
    0xA5DCED,
    0xE2F9FF,
    0xD2F4FD,
    0xBFEBF8,
    0x46B8D8,
    0x82CAE0,
    0x7CE2FE,
    0x72DBF8,
    0x19404D,
    0x256E93,
  );
}
