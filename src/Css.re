/* ===== 🎨 CSS Core Types ===== */

module Calc = {
  type t = [ | `add | `sub | `mult | `div];

  let (+) = (a, b) => `calc((`add, a, b));
  let (-) = (a, b) => `calc((`sub, a, b));
  let ( * ) = (a, b) => `calc((`mult, a, b));
  let (/) = (a, b) => `calc((`div, a, b));

  let toString = (x: t) =>
    switch (x) {
    | `add => "+"
    | `sub => "-"
    | `mult => "*"
    | `div => "/"
    };
};

module LengthUnit = {
  type t = [
    | `em(float)
    | `ex(float)
    | `ch(float)
    | `rem(float)
    | `vw(float)
    | `vh(float)
    | `vmin(float)
    | `vmax(float)
    | `cm(float)
    | `mm(float)
    | `q(float)
    | `inch(float)
    | `pc(float)
    | `pt(float)
    | `px(int)
    | `zero
  ];

  let toString = (x: t) =>
    switch (x) {
    | `em(x) => {j|$(x)em|j}
    | `ex(x) => {j|$(x)ex|j}
    | `ch(x) => {j|$(x)ch|j}
    | `rem(x) => {j|$(x)rem|j}
    | `vw(x) => {j|$(x)vw|j}
    | `vh(x) => {j|$(x)vh|j}
    | `vmin(x) => {j|$(x)vmin|j}
    | `vmax(x) => {j|$(x)vmax|j}
    | `cm(x) => {j|$(x)cm|j}
    | `mm(x) => {j|$(x)mm|j}
    | `q(x) => {j|$(x)Q|j}
    | `inch(x) => {j|$(x)in|j}
    | `pc(x) => {j|$(x)pc|j}
    | `pt(x) => {j|$(x)pt|j}
    | `px(x) => {j|$(x)px|j}
    | `zero => "0"
    };
};

module Length = {
  type t = [ LengthUnit.t | `calc(Calc.t, t, t)];

  let rec toString = (x: t) =>
    switch (x) {
    | #LengthUnit.t as x => x->LengthUnit.toString
    | `calc(op, a, b) =>
      let op = op->Calc.toString;
      let a = a->toString;
      let b = b->toString;
      {j|calc($a $op $b)|j};
    };

  let toString2 = (a, b) => {
    let a = a->toString;
    let b = b->toString;
    {j|$a $b|j};
  };
};

module PercentageUnit = {
  type t = [ | `pct(float)];

  let toString = (x: t) =>
    switch (x) {
    | `pct(x) => {j|$(x)%|j}
    };
};

module Percentage = {
  type t = [ PercentageUnit.t | `calc(Calc.t, t, t)];

  let rec toString = (x: t) =>
    switch (x) {
    | #PercentageUnit.t as x => x->PercentageUnit.toString
    | `calc(op, a, b) =>
      let op = op->Calc.toString;
      let a = a->toString;
      let b = b->toString;
      {j|calc($a $op $b)|j};
    };
};

module LengthPercentage = {
  type t = [ LengthUnit.t | PercentageUnit.t | `calc(Calc.t, t, t)];

  let rec toString = (x: t) =>
    switch (x) {
    | #LengthUnit.t as x => x->LengthUnit.toString
    | #PercentageUnit.t as x => x->PercentageUnit.toString
    | `calc(op, a, b) =>
      let op = op->Calc.toString;
      let a = a->toString;
      let b = b->toString;
      {j|calc($a $op $b)|j};
    };

  let toString2 = (a, b) => {
    let a = a->toString;
    let b = b->toString;
    {j|$a $b|j};
  };

  let toString3 = (a, b, c) => {
    let a = a->toString;
    let b = b->toString;
    let c = c->toString;
    {j|$a $b $c|j};
  };

  let toString4 = (a, b, c, d) => {
    let a = a->toString;
    let b = b->toString;
    let c = c->toString;
    let d = d->toString;
    {j|$a $b $c $d|j};
  };
};

module LengthPercentageAuto = {
  type t = [ | `auto | LengthPercentage.t];

  let toString = (x: t) =>
    switch (x) {
    | `auto => "auto"
    | #LengthPercentage.t as x => x->LengthPercentage.toString
    };

  let toString2 = (a, b) => {
    let a = a->toString;
    let b = b->toString;
    {j|$a $b|j};
  };

  let toString3 = (a, b, c) => {
    let a = a->toString;
    let b = b->toString;
    let c = c->toString;
    {j|$a $b $c|j};
  };

  let toString4 = (a, b, c, d) => {
    let a = a->toString;
    let b = b->toString;
    let c = c->toString;
    let d = d->toString;
    {j|$a $b $c $d|j};
  };
};

module LengthPercentageNone = {
  type t = [ | `none | LengthPercentage.t];

  let toString = (x: t) =>
    switch (x) {
    | `none => "none"
    | #LengthPercentage.t as x => x->LengthPercentage.toString
    };
};

module NumberPercentage = {
  type t = [ PercentageUnit.t | `num(float) | `calc(Calc.t, t, t)];

  let rec toString = (x: t) =>
    switch (x) {
    | #PercentageUnit.t as x => x->PercentageUnit.toString
    | `num(x) => {j|$x|j}
    | `calc(op, a, b) =>
      let op = op->Calc.toString;
      let a = a->toString;
      let b = b->toString;
      {j|calc($a $op $b)|j};
    };

  let toString2 = (a, b) => {
    let a = a->toString;
    let b = b->toString;
    {j|$a $b|j};
  };

  let toString3 = (a, b, c) => {
    let a = a->toString;
    let b = b->toString;
    let c = c->toString;
    {j|$a $b $c|j};
  };

  let toString4 = (a, b, c, d) => {
    let a = a->toString;
    let b = b->toString;
    let c = c->toString;
    let d = d->toString;
    {j|$a $b $c $d|j};
  };
};

module Angle = {
  type t = [ | `deg(float) | `rad(float) | `grad(float) | `turn(float)];

  let toString = (x: t) =>
    switch (x) {
    | `deg(x) => {j|$(x)deg|j}
    | `rad(x) => {j|$(x)rad|j}
    | `grad(x) => {j|$(x)grad|j}
    | `turn(x) => {j|$(x)turn|j}
    };
};

module Color = {
  type t = [
    | `rgb(int, int, int)
    | `rgba(int, int, int, float)
    | `hsl(int, int, int)
    | `hsla(int, int, int, float)
    | `hex(string)
    | `transparent
    | `currentColor
  ];

  let toString = (x: t) =>
    switch (x) {
    | `rgb(r, g, b) => {j|rgb($r, $g, $b)|j}
    | `rgba(r, g, b, a) => {j|rgba($r, $g, $b, $a)|j}
    | `hsl(h, s, l) => {j|hsl($h, $s%, $l%)|j}
    | `hsla(h, s, l, a) => {j|hsla($h, $s%, $l%, $a)|j}
    | `hex(x) => "#" ++ x
    | `transparent => "transparent"
    | `currentColor => "currentColor"
    };

  let aliceblue = `hex("f0f8ff");
  let antiquewhite = `hex("faebd7");
  let aqua = `hex("00ffff");
  let aquamarine = `hex("7fffd4");
  let azure = `hex("f0ffff");
  let beige = `hex("f5f5dc");
  let bisque = `hex("ffe4c4");
  let black = `hex("000000");
  let blanchedalmond = `hex("ffebcd");
  let blue = `hex("0000ff");
  let blueviolet = `hex("8a2be2");
  let brown = `hex("a52a2a");
  let burlywood = `hex("deb887");
  let cadetblue = `hex("5f9ea0");
  let chartreuse = `hex("7fff00");
  let chocolate = `hex("d2691e");
  let coral = `hex("ff7f50");
  let cornflowerblue = `hex("6495ed");
  let cornsilk = `hex("fff8dc");
  let crimson = `hex("dc143c");
  let cyan = `hex("00ffff");
  let darkblue = `hex("00008b");
  let darkcyan = `hex("008b8b");
  let darkgoldenrod = `hex("b8860b");
  let darkgray = `hex("a9a9a9");
  let darkgreen = `hex("006400");
  let darkgrey = `hex("a9a9a9");
  let darkkhaki = `hex("bdb76b");
  let darkmagenta = `hex("8b008b");
  let darkolivegreen = `hex("556b2f");
  let darkorange = `hex("ff8c00");
  let darkorchid = `hex("9932cc");
  let darkred = `hex("8b0000");
  let darksalmon = `hex("e9967a");
  let darkseagreen = `hex("8fbc8f");
  let darkslateblue = `hex("483d8b");
  let darkslategray = `hex("2f4f4f");
  let darkslategrey = `hex("2f4f4f");
  let darkturquoise = `hex("00ced1");
  let darkviolet = `hex("9400d3");
  let deeppink = `hex("ff1493");
  let deepskyblue = `hex("00bfff");
  let dimgray = `hex("696969");
  let dimgrey = `hex("696969");
  let dodgerblue = `hex("1e90ff");
  let firebrick = `hex("b22222");
  let floralwhite = `hex("fffaf0");
  let forestgreen = `hex("228b22");
  let fuchsia = `hex("ff00ff");
  let gainsboro = `hex("dcdcdc");
  let ghostwhite = `hex("f8f8ff");
  let gold = `hex("ffd700");
  let goldenrod = `hex("daa520");
  let gray = `hex("808080");
  let green = `hex("008000");
  let greenyellow = `hex("adff2f");
  let grey = `hex("808080");
  let honeydew = `hex("f0fff0");
  let hotpink = `hex("ff69b4");
  let indianred = `hex("cd5c5c");
  let indigo = `hex("4b0082");
  let ivory = `hex("fffff0");
  let khaki = `hex("f0e68c");
  let lavender = `hex("e6e6fa");
  let lavenderblush = `hex("fff0f5");
  let lawngreen = `hex("7cfc00");
  let lemonchiffon = `hex("fffacd");
  let lightblue = `hex("add8e6");
  let lightcoral = `hex("f08080");
  let lightcyan = `hex("e0ffff");
  let lightgoldenrodyellow = `hex("fafad2");
  let lightgray = `hex("d3d3d3");
  let lightgreen = `hex("90ee90");
  let lightgrey = `hex("d3d3d3");
  let lightpink = `hex("ffb6c1");
  let lightsalmon = `hex("ffa07a");
  let lightseagreen = `hex("20b2aa");
  let lightskyblue = `hex("87cefa");
  let lightslategray = `hex("778899");
  let lightslategrey = `hex("778899");
  let lightsteelblue = `hex("b0c4de");
  let lightyellow = `hex("ffffe0");
  let lime = `hex("00ff00");
  let limegreen = `hex("32cd32");
  let linen = `hex("faf0e6");
  let magenta = `hex("ff00ff");
  let maroon = `hex("800000");
  let mediumaquamarine = `hex("66cdaa");
  let mediumblue = `hex("0000cd");
  let mediumorchid = `hex("ba55d3");
  let mediumpurple = `hex("9370db");
  let mediumseagreen = `hex("3cb371");
  let mediumslateblue = `hex("7b68ee");
  let mediumspringgreen = `hex("00fa9a");
  let mediumturquoise = `hex("48d1cc");
  let mediumvioletred = `hex("c71585");
  let midnightblue = `hex("191970");
  let mintcream = `hex("f5fffa");
  let mistyrose = `hex("ffe4e1");
  let moccasin = `hex("ffe4b5");
  let navajowhite = `hex("ffdead");
  let navy = `hex("000080");
  let oldlace = `hex("fdf5e6");
  let olive = `hex("808000");
  let olivedrab = `hex("6b8e23");
  let orange = `hex("ffa500");
  let orangered = `hex("ff4500");
  let orchid = `hex("da70d6");
  let palegoldenrod = `hex("eee8aa");
  let palegreen = `hex("98fb98");
  let paleturquoise = `hex("afeeee");
  let palevioletred = `hex("db7093");
  let papayawhip = `hex("ffefd5");
  let peachpuff = `hex("ffdab9");
  let peru = `hex("cd853f");
  let pink = `hex("ffc0cb");
  let plum = `hex("dda0dd");
  let powderblue = `hex("b0e0e6");
  let purple = `hex("800080");
  let rebeccapurple = `hex("663399");
  let red = `hex("ff0000");
  let rosybrown = `hex("bc8f8f");
  let royalblue = `hex("4169e1");
  let saddlebrown = `hex("8b4513");
  let salmon = `hex("fa8072");
  let sandybrown = `hex("f4a460");
  let seagreen = `hex("2e8b57");
  let seashell = `hex("fff5ee");
  let sienna = `hex("a0522d");
  let silver = `hex("c0c0c0");
  let skyblue = `hex("87ceeb");
  let slateblue = `hex("6a5acd");
  let slategray = `hex("708090");
  let slategrey = `hex("708090");
  let snow = `hex("fffafa");
  let springgreen = `hex("00ff7f");
  let steelblue = `hex("4682b4");
  let tan = `hex("d2b48c");
  let teal = `hex("008080");
  let thistle = `hex("d8bfd8");
  let tomato = `hex("ff6347");
  let turquoise = `hex("40e0d0");
  let violet = `hex("ee82ee");
  let wheat = `hex("f5deb3");
  let white = `hex("ffffff");
  let whitesmoke = `hex("f5f5f5");
  let yellow = `hex("ffff00");
  let yellowgreen = `hex("9acd3");
};

module Gradient = {
  module Stops = {
    type t = list((int, Color.t));

    let toString = (x: t) =>
      x
      ->(
          List.mapU((. (index, color)) => {
            let color = color->Color.toString;
            {j|$color $index%|j};
          })
        )
      ->CssHelpers.joinWith(", ");
  };

  type t = [
    | `linearGradient(Angle.t, Stops.t)
    | `repeatingLinearGradient(Angle.t, Stops.t)
    | `radialGradient(Stops.t)
    | `repeatingRadialGradient(Stops.t)
  ];

  let toString = (x: t) =>
    switch (x) {
    | `linearGradient(angle, stops) =>
      let angle = angle->Angle.toString;
      let stops = stops->Stops.toString;
      {j|linear-gradient($angle, $stops)|j};
    | `repeatingLinearGradient(angle, stops) =>
      let angle = angle->Angle.toString;
      let stops = stops->Stops.toString;
      {j|repeating-linear-gradient($angle, $stops)|j};
    | `radialGradient(stops) =>
      let stops = stops->Stops.toString;
      {j|radial-gradient($stops)|j};
    | `repeatingRadialGradient(stops) =>
      let stops = stops->Stops.toString;
      {j|repeating-radial-gradient($stops)|j};
    };
};

module Url = {
  type t = [ | `url(string)];

  let toString = (x: t) =>
    switch (x) {
    | `url(x) => {j|url($x)|j}
    };
};

module Image = {
  type t = [ Url.t | Gradient.t];

  let toString = (x: t) =>
    switch (x) {
    | #Url.t as x => x->Url.toString
    | #Gradient.t as x => x->Gradient.toString
    };
};

module Display = {
  type t = [
    | `none
    | `inline
    | `listItem
    | `block
    | `inlineBlock
    | `flex
    | `inlineFlex
    | `grid
    | `inlineGrid
    | `table
    | `inlineTable
    | `tableRowGroup
    | `tableHeaderGroup
    | `tableFooterGroup
    | `tableRow
    | `tableColumnGroup
    | `tableColumn
    | `tableCell
    | `tableCaption
  ];

  let toString = (x: t) =>
    switch (x) {
    | `none => "none"
    | `inline => "inline"
    | `listItem => "list-item"
    | `block => "block"
    | `inlineBlock => "inline-block"
    | `flex => "flex"
    | `inlineFlex => "inline-flex"
    | `grid => "grid"
    | `inlineGrid => "inline-grid"
    | `table => "table"
    | `inlineTable => "inline-table"
    | `tableRowGroup => "table-row-group"
    | `tableHeaderGroup => "table-header-group"
    | `tableFooterGroup => "table-footer-group"
    | `tableRow => "table-row"
    | `tableColumnGroup => "table-column-group"
    | `tableColumn => "table-column"
    | `tableCell => "table-cell"
    | `tableCaption => "table-caption"
    };
};

module Position = {
  type t = [ | `absolute | `static | `fixed | `relative | `sticky];

  let toString = (x: t) =>
    switch (x) {
    | `absolute => "absolute"
    | `static => "static"
    | `fixed => "fixed"
    | `relative => "relative"
    | `sticky => "sticky"
    };
};

module BorderStyle = {
  type t = [
    | `none
    | `hidden
    | `dotted
    | `dashed
    | `solid
    | `double
    | `groove
    | `ridge
    | `inset
    | `outset
  ];

  let toString = (x: t) =>
    switch (x) {
    | `none => "none"
    | `hidden => "hidden"
    | `dotted => "dotted"
    | `dashed => "dashed"
    | `solid => "solid"
    | `double => "double"
    | `groove => "groove"
    | `ridge => "ridge"
    | `inset => "inset"
    | `outset => "outset"
    };
};

module BorderWidth = {
  type t = [ | `thin | `medium | `thick | LengthPercentage.t];

  let toString = (x: t) =>
    switch (x) {
    | `thin => "thin"
    | `medium => "medium"
    | `thick => "thick"
    | #LengthPercentage.t as x => x->LengthPercentage.toString
    };
};

module Border = {
  let toString = (width, style, color) => {
    let width = width->BorderWidth.toString;
    let style = style->BorderStyle.toString;
    let color = color->Color.toString;
    {j|$width $style $color|j};
  };
};

module BackgroundImage = {
  type t = [ | `none | Image.t];

  let toString = (x: t) =>
    switch (x) {
    | `none => "none"
    | #Image.t as x => x->Image.toString
    };
};

module BackgroundAttachment = {
  type t = [ | `scroll | `fixed | `local];

  let toString = (x: t) =>
    switch (x) {
    | `scroll => "scroll"
    | `fixed => "fixed"
    | `local => "local"
    };
};

module BackgroundBlendMode = {
  type t = [
    | `normal
    | `multiply
    | `screen
    | `overlay
    | `darken
    | `lighten
    | `colorDodge
    | `colorBurn
    | `hardLight
    | `softLight
    | `difference
    | `exclusion
    | `hue
    | `saturation
    | `color
    | `luminosity
  ];

  let toString = (x: t) =>
    switch (x) {
    | `normal => "normal"
    | `multiply => "multiply"
    | `screen => "screen"
    | `overlay => "overlay"
    | `darken => "darken"
    | `lighten => "lighten"
    | `colorDodge => "color-dodge"
    | `colorBurn => "color-burn"
    | `hardLight => "hard-light"
    | `softLight => "soft-light"
    | `difference => "difference"
    | `exclusion => "exclusion"
    | `hue => "hue"
    | `saturation => "saturation"
    | `color => "color"
    | `luminosity => "luminosit"
    };
};

module BackgroundBox = {
  type t = [ | `borderBox | `contentBox | `paddingBox];

  let toString = (x: t) =>
    switch (x) {
    | `borderBox => "border-box"
    | `contentBox => "content-box"
    | `paddingBox => "padding-box"
    };
};

module BackgroundRepeat = {
  type t = [ | `repeatX | `repeatY | `repeat | `noRepeat | `space | `round];

  let toString = (x: t) =>
    switch (x) {
    | `repeatX => "repeat-x"
    | `repeatY => "repeat-y"
    | `repeat => "repeat"
    | `noRepeat => "no-repeat"
    | `space => "space"
    | `round => "round"
    };
  let toString2 = (a, b) => {
    let a = a->toString;
    let b = b->toString;
    {j|$a $b|j};
  };
};

module BackgroundSize = {
  type t = [
    | `size(LengthPercentage.t, LengthPercentage.t)
    | `auto
    | `cover
    | `contain
  ];

  let toString = (x: t) =>
    switch (x) {
    | `size(x, y) =>
      let x = x->LengthPercentage.toString;
      let y = y->LengthPercentage.toString;
      {j|$x $y|j};
    | `auto => "auto"
    | `cover => "cover"
    | `contain => "contain"
    };
};

module ListStyleType = {
  /* TODO: ListStyleType: incomplete list */
  type t = [
    | `none
    | `disc
    | `circle
    | `square
    | `decimal
    | `decimalLeadingZero
    | `lowerAlpha
    | `upperAlpha
    | `lowerGreek
    | `lowerLatin
    | `upperLatin
    | `lowerRoman
    | `upperRoman
  ];

  let toString = (x: t) =>
    switch (x) {
    | `none => "none"
    | `disc => "disc"
    | `circle => "circle"
    | `square => "square"
    | `decimal => "decimal"
    | `decimalLeadingZero => "decimal-leading-zero"
    | `lowerAlpha => "lower-alpha"
    | `upperAlpha => "upper-alpha"
    | `lowerGreek => "lower-greek"
    | `lowerLatin => "lower-latin"
    | `upperLatin => "upper-latin"
    | `lowerRoman => "lower-roman"
    | `upperRoman => "upper-roman"
    };
};

module ListStylePosition = {
  type t = [ | `inside | `outside];

  let toString = (x: t) =>
    switch (x) {
    | `inside => "inside"
    | `outside => "outside"
    };
};

module ListStyleImage = {
  type t = [ | `none | Url.t];

  let toString = (x: t) =>
    switch (x) {
    | `none => "none"
    | #Url.t as x => x->Url.toString
    };
};

module ListStyle = {
  let toString = (style, position, image) => {
    let style = style->ListStyleType.toString;
    let position = position->ListStylePosition.toString;
    let image = image->ListStyleImage.toString;
    {j|$style $position $image|j};
  };
};

module BoxSizing = {
  type t = [ | `contentBox | `borderBox];

  let toString = (x: t) =>
    switch (x) {
    | `contentBox => "content-box"
    | `borderBox => "border-box"
    };
};

module TableLayout = {
  type t = [ | `auto | `fixed];

  let toString = (x: t) =>
    switch (x) {
    | `auto => "auto"
    | `fixed => "fixed"
    };
};

module BorderCollapse = {
  type t = [ | `collapse | `separate];

  let toString = (x: t) =>
    switch (x) {
    | `collapse => "collapse"
    | `separate => "separate"
    };
};

module Float = {
  type t = [ | `left | `right | `none];

  let toString = (x: t) =>
    switch (x) {
    | `left => "left"
    | `right => "right"
    | `none => "none"
    };
};

module Clear = {
  type t = [ | `left | `right | `both];

  let toString = (x: t) =>
    switch (x) {
    | `left => "left"
    | `right => "right"
    | `both => "both"
    };
};

module Overflow = {
  type t = [ | `auto | `scroll | `visible | `hidden];

  let toString = (x: t) =>
    switch (x) {
    | `auto => "auto"
    | `scroll => "scroll"
    | `visible => "visible"
    | `hidden => "hidden"
    };
};

module FontStyle = {
  type t = [ | `normal | `italic | `oblique];

  let toString = (x: t) =>
    switch (x) {
    | `normal => "normal"
    | `italic => "italic"
    | `oblique => "oblique"
    };
};

module FontVariant = {
  /* TODO: FontVariant: incomplete list */
  type t = [ | `none | `normal | `smallCaps | `allSmallCaps];

  let toString = (x: t) =>
    switch (x) {
    | `none => "none"
    | `normal => "normal"
    | `smallCaps => "small-caps"
    | `allSmallCaps => "all-small-caps"
    };
};

module FontKerning = {
  type t = [ | `auto | `normal | `none];

  let toString = (x: t) =>
    switch (x) {
    | `auto => "auto"
    | `normal => "normal"
    | `none => "none"
    };
};

module FontStretch = {
  type t = [
    | `normal
    | `semiCondensed
    | `condensed
    | `extraCondensed
    | `ultraCondensed
    | `semiExpanded
    | `expanded
    | `extraExpanded
    | `ultraExpanded
  ];

  let toString = (x: t) =>
    switch (x) {
    | `normal => "normal"
    | `semiCondensed => "semi-condensed"
    | `condensed => "condensed"
    | `extraCondensed => "extra-condensed"
    | `ultraCondensed => "ultra-condensed"
    | `semiExpanded => "semi-expanded"
    | `expanded => "expanded"
    | `extraExpanded => "extra-expanded"
    | `ultraExpanded => "ultra-expanded"
    };
};

module FontSrc = {
  type src = [ | `url(string) | `local(string)];
  type format = [ | `woff | `woff2 | `ttf | `eot | `svg];

  let toString = (~format: option(format)=?, src: src) => {
    let src =
      switch (src) {
      | `url(v) => {j|url("$v")|j}
      | `local(v) => {j|local("$v")|j}
      };

    switch (format) {
    | None => src
    | Some(format) =>
      let format =
        switch (format) {
        | `woff => "woff"
        | `woff2 => "woff2"
        | `ttf => "truetype"
        | `eot => "embedded-opentype"
        | `svg => "svg"
        };
      {j|$src $format|j};
    };
  };
};

module LineHeight = {
  type t = [ | `normal | `abs(float) | LengthPercentage.t];

  let toString = (x: t) =>
    switch (x) {
    | `normal => "normal"
    | `abs(x) => {j|$x|j}
    | #LengthPercentage.t as x => x->LengthPercentage.toString
    };
};

module LetterSpacing = {
  type t = [ | `normal | Length.t];

  let toString = (x: t) =>
    switch (x) {
    | `normal => "normal"
    | #Length.t as x => x->Length.toString
    };
};

module Hyphens = {
  type t = [ | `auto | `manual | `none];

  let toString = (x: t) =>
    switch (x) {
    | `auto => "auto"
    | `manual => "manual"
    | `none => "none"
    };
};

module TextAlign = {
  type t = [ | `left | `right | `center | `justify];

  let toString = (x: t) =>
    switch (x) {
    | `left => "left"
    | `right => "right"
    | `center => "center"
    | `justify => "justify"
    };
};

module TextDecorationLine = {
  type t = [ | `none | `underline | `overline | `lineThrough];

  let toString = (x: t) =>
    switch (x) {
    | `none => "none"
    | `underline => "underline"
    | `overline => "overline"
    | `lineThrough => "line-through"
    };
};

module TextDecorationStyle = {
  type t = [ | `wavy | `solid | `double | `dotted | `dashed];

  let toString = (x: t) =>
    switch (x) {
    | `wavy => "wavy"
    | `solid => "solid"
    | `double => "double"
    | `dotted => "dotted"
    | `dashed => "dashed"
    };
};

module TextOverflow = {
  type t = [ | `clip | `ellipsis];

  let toString = (x: t) =>
    switch (x) {
    | `clip => "clip"
    | `ellipsis => "ellipsis"
    };
};

module TextShadow = {
  let toString = (~x, ~y, ~blur, color) => {
    let x = x->Length.toString;
    let y = y->Length.toString;
    let blur = blur->Length.toString;
    let color = color->Color.toString;
    {j|$x $y $blur $color|j};
  };
};

module TextTransform = {
  type t = [ | `uppercase | `lowercase | `capitalize | `none];

  let toString = (x: t) =>
    switch (x) {
    | `uppercase => "uppercase"
    | `lowercase => "lowercase"
    | `capitalize => "capitalize"
    | `none => "none"
    };
};

module UserSelect = {
  type t = [ | `auto | `all | `text | `none];

  let toString = (x: t) =>
    switch (x) {
    | `auto => "auto"
    | `all => "all"
    | `text => "text"
    | `none => "none"
    };
};

module VerticalAlign = {
  type t = [
    | `baseline
    | `sub
    | `super
    | `top
    | `textTop
    | `middle
    | `bottom
    | `textBottom
    | LengthPercentage.t
  ];

  let toString = (x: t) =>
    switch (x) {
    | `baseline => "baseline"
    | `sub => "sub"
    | `super => "super"
    | `top => "top"
    | `textTop => "text-top"
    | `middle => "middle"
    | `bottom => "bottom"
    | `textBottom => "text-bottom"
    | #LengthPercentage.t as x => x->LengthPercentage.toString
    };
};

module WhiteSpace = {
  type t = [ | `normal | `nowrap | `pre | `preLine | `preWrap];

  let toString = (x: t) =>
    switch (x) {
    | `normal => "normal"
    | `nowrap => "nowrap"
    | `pre => "pre"
    | `preLine => "pre-line"
    | `preWrap => "pre-wrap"
    };
};

module WordBreak = {
  type t = [ | `breakAll | `keepAll | `normal];

  let toString = (x: t) =>
    switch (x) {
    | `breakAll => "break-all"
    | `keepAll => "keep-all"
    | `normal => "normal"
    };
};

module WordSpacing = {
  type t = [ | `normal | Length.t];

  let toString = (x: t) =>
    switch (x) {
    | `normal => "normal"
    | #Length.t as x => x->Length.toString
    };
};

module WordWrap = {
  type t = [ | `normal | `breakWord];

  let toString = (x: t) =>
    switch (x) {
    | `normal => "normal"
    | `breakWord => "break-word"
    };
};

module Direction = {
  type t = [ | `ltr | `rtl];

  let toString = (x: t) =>
    switch (x) {
    | `ltr => "ltr"
    | `rtl => "rtl"
    };
};

module OutlineStyle = {
  type t = [ | `auto | BorderStyle.t];

  let toString = (x: t) =>
    switch (x) {
    | `auto => "auto"
    | #BorderStyle.t as x => x->BorderStyle.toString
    };
};

module Outline = {
  let toString = (width, style, color) => {
    let width = width->Length.toString;
    let style = style->OutlineStyle.toString;
    let color = color->Color.toString;
    {j|$width $style $color|j};
  };
};

module BoxShadow = {
  let toString = (~x, ~y, ~blur, ~spread, ~inset, color) => {
    let x = x->Length.toString;
    let y = y->Length.toString;
    let blur = blur->Length.toString;
    let spread = spread->Length.toString;
    let color = color->Color.toString;

    if (inset) {{j|$x $y $blur $spread $color inset|j}} else {
      {j|$x $y $blur $spread $color|j}
    };
  };
};

module Visibility = {
  type t = [ | `visible | `hidden | `collapse];

  let toString = (x: t) =>
    switch (x) {
    | `visible => "visible"
    | `hidden => "hidden"
    | `collapse => "collapse"
    };
};

module Cursor = {
  /* FIXME: List of `url`s */
  type t = [
    | `url(string)
    | `auto
    | `default
    | `none
    | `contextMenu
    | `help
    | `pointer
    | `progress
    | `wait
    | `cell
    | `crosshair
    | `text
    | `verticalText
    | `alias
    | `copy
    | `move
    | `noDrop
    | `notAllowed
    | `grab
    | `grabbing
    | `allScroll
    | `colResize
    | `rowResize
    | `nResize
    | `eResize
    | `sResize
    | `wResize
    | `neResize
    | `nwResize
    | `seResize
    | `swResize
    | `ewResize
    | `nsResize
    | `neswResize
    | `nwseResize
    | `zoomIn
    | `zoomOut
  ];

  let toString = (x: t) =>
    switch (x) {
    | `url(x) => {j|url($x)|j}
    | `auto => "auto"
    | `default => "default"
    | `none => "none"
    | `contextMenu => "context-menu"
    | `help => "help"
    | `pointer => "pointer"
    | `progress => "progress"
    | `wait => "wait"
    | `cell => "cell"
    | `crosshair => "crosshair"
    | `text => "text"
    | `verticalText => "vertical-text"
    | `alias => "alias"
    | `copy => "copy"
    | `move => "move"
    | `noDrop => "no-drop"
    | `notAllowed => "not-allowed"
    | `grab => "grab"
    | `grabbing => "grabbing"
    | `allScroll => "all-scroll"
    | `colResize => "col-resize"
    | `rowResize => "row-resize"
    | `nResize => "n-resize"
    | `eResize => "e-resize"
    | `sResize => "s-resize"
    | `wResize => "w-resize"
    | `neResize => "ne-resize"
    | `nwResize => "nw-resize"
    | `seResize => "se-resize"
    | `swResize => "sw-resize"
    | `ewResize => "ew-resize"
    | `nsResize => "ns-resize"
    | `neswResize => "nesw-resize"
    | `nwseResize => "nwse-resize"
    | `zoomIn => "zoom-in"
    | `zoomOut => "zoom-out"
    };
};

module PointerEvents = {
  /* TODO: SVG PointerEvents */
  type t = [ | `auto | `none];

  let toString = (x: t) =>
    switch (x) {
    | `auto => "auto"
    | `none => "none"
    };
};

module Timing = {
  type t = [ | `s(float) | `ms(int) | `zero];

  let toString = (x: t) =>
    switch (x) {
    | `s(x) => {j|$(x)s|j}
    | `ms(x) => {j|$(x)ms|j}
    | `zero => "0s"
    };
};

module TimingFunction = {
  type t = [
    | `linear
    | `ease
    | `easeIn
    | `easeOut
    | `easeInOut
    | `stepStart
    | `stepEnd
    | `steps(int, [ | `start | `end_])
    | `cubicBezier(float, float, float, float)
  ];

  let toString = (x: t) =>
    switch (x) {
    | `linear => "linear"
    | `ease => "ease"
    | `easeIn => "ease-out"
    | `easeOut => "ease-out"
    | `easeInOut => "ease-in-out"
    | `stepStart => "step-start"
    | `stepEnd => "step-end"
    | `steps(n, `start) => {j|steps($n, start)|j}
    | `steps(n, `end_) => {j|steps($n, end)|j}
    | `cubicBezier(a, b, c, d) => {j|cubic-bezier($a, $b, $c, $d)|j}
    };
};

module TransitionProperty = {
  type t = string;
};

module TransitionDuration = {
  type t = Timing.t;
};

module TransitionDelay = {
  type t = Timing.t;
};

module TransitionTimingFunction = {
  type t = TimingFunction.t;
};

module Transition = {
  let toString =
      (
        ~property: TransitionProperty.t,
        ~duration: TransitionDuration.t,
        ~delay: TransitionDelay.t,
        ~timingFunction: TransitionTimingFunction.t,
      ) => {
    let duration = duration->Timing.toString;
    let delay = delay->Timing.toString;
    let timingFunction = timingFunction->TimingFunction.toString;
    {j|$property $duration $timingFunction $delay|j};
  };
};

module Transform = {
  type t = [
    | `translate(LengthPercentage.t, LengthPercentage.t)
    | `translate3d(LengthPercentage.t, LengthPercentage.t, LengthPercentage.t)
    | `translateX(LengthPercentage.t)
    | `translateY(LengthPercentage.t)
    | `translateZ(LengthPercentage.t)
    | `scale(float)
    | `scaleXY(float, float)
    | `scaleX(float)
    | `scaleY(float)
    | `scaleZ(float)
    | `scale3d(float, float, float)
    | `rotate(Angle.t)
    | `rotate3d(float, float, float, Angle.t)
    | `rotateX(Angle.t)
    | `rotateY(Angle.t)
    | `rotateZ(Angle.t)
    | `skew(Angle.t, Angle.t)
    | `skewX(Angle.t)
    | `skewY(Angle.t)
    | `matrix(float, float, float, float, float, float)
    | `matrix3d(
        float,
        float,
        float,
        float,
        float,
        float,
        float,
        float,
        float,
        float,
        float,
        float,
        float,
        float,
        float,
        float,
      )
    | `perspective(Length.t)
    | `none
  ];

  let toString = (x: t) =>
    switch (x) {
    | `translate(x, y) =>
      let x = x->LengthPercentage.toString;
      let y = y->LengthPercentage.toString;
      {j|translate($x, $y)|j};
    | `translate3d(x, y, z) =>
      let x = x->LengthPercentage.toString;
      let y = y->LengthPercentage.toString;
      let z = z->LengthPercentage.toString;
      {j|translate3d($x, $y, $z)|j};
    | `translateX(x) =>
      let x = x->LengthPercentage.toString;
      {j|translateX($x)|j};
    | `translateY(y) =>
      let y = y->LengthPercentage.toString;
      {j|translateY($y)|j};
    | `translateZ(z) =>
      let z = z->LengthPercentage.toString;
      {j|translateZ($z)|j};
    | `scale(x) => {j|scale($x)|j}
    | `scaleXY(x, y) => {j|scale($x, $y)|j}
    | `scaleX(x) => {j|scaleX($x)|j}
    | `scaleY(y) => {j|scaleY($y)|j}
    | `scaleZ(z) => {j|scaleZ($z)|j}
    | `scale3d(x, y, z) => {j|scale3d($x, $y, $z)|j}
    | `rotate(a) =>
      let a = a->Angle.toString;
      {j|rotate($a)|j};
    | `rotate3d(x, y, z, a) =>
      let a = a->Angle.toString;
      {j|rotate3d($x, $y, $z, $a)|j};
    | `rotateX(a) =>
      let a = a->Angle.toString;
      {j|rotateX($a)|j};
    | `rotateY(a) =>
      let a = a->Angle.toString;
      {j|rotateY($a)|j};
    | `rotateZ(a) =>
      let a = a->Angle.toString;
      {j|rotateZ($a)|j};
    | `skew(x, y) =>
      let x = x->Angle.toString;
      let y = y->Angle.toString;
      {j|skew($x, $y)|j};
    | `skewX(a) =>
      let a = a->Angle.toString;
      {j|skewX($a)|j};
    | `skewY(a) =>
      let a = a->Angle.toString;
      {j|skewY($a)|j};
    | `matrix(a, b, c, d, tx, ty) => {j|matrix($a, $b, $c, $d, $tx, $ty)|j}
    | `matrix3d(
        a1,
        b1,
        c1,
        d1,
        a2,
        b2,
        c2,
        d2,
        a3,
        b3,
        c3,
        d3,
        a4,
        b4,
        c4,
        d4,
      ) => {j|matrix3d($a1, $b1, $c1, $d1, $a2, $b2, $c2, $d2, $a3, $b3, $c3, $d3, $a4, $b4, $c4, $d4)|j}
    | `perspective(x) =>
      let x = x->Length.toString;
      {j|perspective($x)|j};
    | `none => "none"
    };
};

module TransformStyle = {
  type t = [ | `flat | `preserve3d];

  let toString = (x: t) =>
    switch (x) {
    | `flat => "flat"
    | `preserve3d => "preserve-3d"
    };
};

module Perspective = {
  type t = [ | `none | Length.t];

  let toString = (x: t) =>
    switch (x) {
    | `none => "none"
    | #Length.t as x => x->Length.toString
    };
};

module AnimationDirection = {
  type t = [ | `normal | `reverse | `alternate | `alternateReverse];

  let toString = (x: t) =>
    switch (x) {
    | `normal => "normal"
    | `reverse => "reverse"
    | `alternate => "alternate"
    | `alternateReverse => "alternate-reverse"
    };
};

module AnimationFillMode = {
  type t = [ | `none | `forwards | `backwards | `both];

  let toString = (x: t) =>
    switch (x) {
    | `none => "none"
    | `forwards => "forwards"
    | `backwards => "backwards"
    | `both => "both"
    };
};

module AnimationIterationCount = {
  type t = [ | `infinite | `i(int)];

  let toString = (x: t) =>
    switch (x) {
    | `infinite => "infinite"
    | `i(x) => {j|$x|j}
    };
};

module AnimationPlayState = {
  type t = [ | `paused | `running];

  let toString = (x: t) =>
    switch (x) {
    | `paused => "paused"
    | `running => "running"
    };
};

module AnimationName = {
  type t = string;
};

module AnimationDuration = {
  type t = Timing.t;
};

module AnimationDelay = {
  type t = Timing.t;
};

module AnimationTimingFunction = {
  type t = TimingFunction.t;
};

module Animation = {
  let toString =
      (
        ~name: AnimationName.t,
        ~duration: AnimationDuration.t,
        ~delay: AnimationDelay.t,
        ~direction: AnimationDirection.t,
        ~timingFunction: AnimationTimingFunction.t,
        ~fillMode: AnimationFillMode.t,
        ~playState: AnimationPlayState.t,
        ~iterationCount: AnimationIterationCount.t,
      ) => {
    let duration = duration->Timing.toString;
    let delay = delay->Timing.toString;
    let direction = direction->AnimationDirection.toString;
    let timingFunction = timingFunction->TimingFunction.toString;
    let fillMode = fillMode->AnimationFillMode.toString;
    let playState = playState->AnimationPlayState.toString;
    let iterationCount = iterationCount->AnimationIterationCount.toString;
    {j|$name $duration $timingFunction $delay $iterationCount $direction $fillMode $playState|j};
  };
};

module FillRule = {
  type t = [ | `evenodd | `nonzero];

  let toString = (x: t) =>
    switch (x) {
    | `evenodd => "evenodd"
    | `nonzero => "nonzero"
    };
};

module StrokeLinecap = {
  type t = [ | `butt | `round | `square];

  let toString = (x: t) =>
    switch (x) {
    | `butt => "butt"
    | `round => "round"
    | `square => "square"
    };
};

module StrokeLinejoin = {
  type t = [ | `miter | `round | `bevel];

  let toString = (x: t) =>
    switch (x) {
    | `miter => "miter"
    | `round => "round"
    | `bevel => "bevel"
    };
};

module FilterFunction = {
  type t = [
    | `blur(Length.t)
    | `brightness(NumberPercentage.t)
    | `contrast(NumberPercentage.t)
    | `dropShadow(Length.t, Length.t, Length.t, Color.t)
    | `grayscale(NumberPercentage.t)
    | `hueRotate(Angle.t)
    | `invert(NumberPercentage.t)
    | `opacity(NumberPercentage.t)
    | `saturate(NumberPercentage.t)
    | `sepia(NumberPercentage.t)
  ];

  let toString = (x: t) =>
    switch (x) {
    | `blur(x) =>
      let x = x->Length.toString;
      {j|blur($x)|j};
    | `brightness(x) =>
      let x = x->NumberPercentage.toString;
      {j|brightness($x)|j};
    | `contrast(x) =>
      let x = x->NumberPercentage.toString;
      {j|contrast($x)|j};
    | `dropShadow(x, y, blur, color) =>
      let x = x->Length.toString;
      let y = y->Length.toString;
      let blur = blur->Length.toString;
      let color = color->Color.toString;
      {j|drop-shadow($x $y $blur $color)|j};
    | `grayscale(x) =>
      let x = x->NumberPercentage.toString;
      {j|grayscale($x)|j};
    | `hueRotate(x) =>
      let x = x->Angle.toString;
      {j|hue-rotate($x)|j};
    | `invert(x) =>
      let x = x->NumberPercentage.toString;
      {j|invert($x)|j};
    | `opacity(x) =>
      let x = x->NumberPercentage.toString;
      {j|opacity($x)|j};
    | `saturate(x) =>
      let x = x->NumberPercentage.toString;
      {j|saturate($x)|j};
    | `sepia(x) =>
      let x = x->NumberPercentage.toString;
      {j|sepia($x)|j};
    };
};

module Filter = {
  type t = [ Url.t | FilterFunction.t | `none];

  let toString = (x: t) =>
    switch (x) {
    | #Url.t as x => x->Url.toString
    | #FilterFunction.t as x => x->FilterFunction.toString
    | `none => "none"
    };
};

module Appearance = {
  /* NOTE: Appearance: incomplete list. Status: Working Draft */
  type t = [ | `none | `button | `checkbox | `radio];

  let toString = (x: t) =>
    switch (x) {
    | `none => "none"
    | `button => "button"
    | `checkbox => "checkbox"
    | `radio => "radio"
    };
};

module Flex = {
  module Flex = {
    type t = [ | `auto | `none | `some(float, float, LengthPercentageAuto.t)];

    let toString = (x: t) =>
      switch (x) {
      | `auto => "auto"
      | `none => "none"
      | `some(grow, shrink, basis) =>
        let basis = basis->LengthPercentageAuto.toString;
        {j|$grow $shrink $basis|j};
      };
  };

  module Direction = {
    type t = [ | `row | `column | `rowReverse | `columnReverse];

    let toString = (x: t) =>
      switch (x) {
      | `row => "row"
      | `column => "column"
      | `rowReverse => "row-reverse"
      | `columnReverse => "column-reverse"
      };
  };

  module Wrap = {
    type t = [ | `nowrap | `wrap | `wrapReverse];

    let toString = (x: t) =>
      switch (x) {
      | `nowrap => "nowrap"
      | `wrap => "wrap"
      | `wrapReverse => "wrap-reverse"
      };
  };

  module Flow = {
    let toString = (direction, wrap) => {
      let direction = direction->Direction.toString;
      let wrap = wrap->Wrap.toString;
      {j|$direction $wrap|j};
    };
  };

  module Align = {
    type t = [
      | `normal
      | `flexStart
      | `flexEnd
      | `center
      | `baseline
      | `stretch
      | `selfStart
      | `selfEnd
    ];

    let toString = (x: t) =>
      switch (x) {
      | `normal => "normal"
      | `flexStart => "flex-start"
      | `flexEnd => "flex-end"
      | `center => "center"
      | `baseline => "baseline"
      | `stretch => "stretch"
      | `selfStart => "self-start"
      | `selfEnd => "self-end"
      };
  };

  module Justify = {
    type t = [
      | `normal
      | `flexStart
      | `flexEnd
      | `center
      | `spaceAround
      | `spaceBetween
      | `stretch
    ];

    let toString = (x: t) =>
      switch (x) {
      | `normal => "normal"
      | `flexStart => "flex-start"
      | `flexEnd => "flex-end"
      | `center => "center"
      | `spaceAround => "space-around"
      | `spaceBetween => "space-between"
      | `stretch => "stretch"
      };
  };
};

/* TODO: Grid */
/* TODO: Globals: inherit, initial, unset */

/* ===== ⚙️ API Implementation ===== */

type declaration;

module Declaration = {
  type t('a) = list((string, 'a));

  external box: t(_) => declaration = "%identity";
  external unbox: declaration => t(_) = "%identity";
};

module Declarations = {
  external box: list(Declaration.t(_)) => list(declaration) = "%identity";
  external unbox: list(declaration) => list(Declaration.t(_)) = "%identity";

  let toDict: list(declaration) => Js.Dict.t(_) =
    declarations => declarations->unbox->List.flatten->Js.Dict.fromList;
};

module Selector = {
  external box: list((string, Js.Dict.t(_))) => declaration = "%identity";
  external unbox: declaration => list((string, Js.Dict.t(_))) = "%identity";
};

/* ===== 🗂 CSS Properties ===== */

let p = (prop, value) => [(prop, value)]->Declaration.box;

let label = (x: string) => p("label", x);

let display = x => p("display", x->Display.toString);

let boxSizing = x => p("boxSizing", x->BoxSizing.toString);

let position = x => p("position", x->Position.toString);

let top = x => p("top", x->LengthPercentageAuto.toString);
let bottom = x => p("bottom", x->LengthPercentageAuto.toString);
let left = x => p("left", x->LengthPercentageAuto.toString);
let right = x => p("right", x->LengthPercentageAuto.toString);

let width = x => p("width", x->LengthPercentageAuto.toString);
let minWidth = x => p("min-width", x->LengthPercentageAuto.toString);
let maxWidth = x => p("max-width", x->LengthPercentageNone.toString);
let height = x => p("height", x->LengthPercentageAuto.toString);
let minHeight = x => p("min-height", x->LengthPercentageAuto.toString);
let maxHeight = x => p("max-height", x->LengthPercentageNone.toString);

let margin = x => p("margin", x->LengthPercentageAuto.toString);
let margin2 = (v, h) => p("margin", LengthPercentageAuto.toString2(v, h));
let margin3 = (t, h, b) =>
  p("margin", LengthPercentageAuto.toString3(t, h, b));
let margin4 = (t, r, b, l) =>
  p("margin", LengthPercentageAuto.toString4(t, r, b, l));

let marginLeft = x => p("marginLeft", x->LengthPercentageAuto.toString);
let marginRight = x => p("marginRight", x->LengthPercentageAuto.toString);
let marginTop = x => p("marginTop", x->LengthPercentageAuto.toString);
let marginBottom = x => p("marginBottom", x->LengthPercentageAuto.toString);

let padding = x => p("padding", x->LengthPercentage.toString);
let padding2 = (v, h) => p("padding", LengthPercentage.toString2(v, h));
let padding3 = (t, h, b) =>
  p("padding", LengthPercentage.toString3(t, h, b));
let padding4 = (t, r, b, l) =>
  p("padding", LengthPercentage.toString4(t, r, b, l));

let paddingLeft = x => p("paddingLeft", x->LengthPercentage.toString);
let paddingRight = x => p("paddingRight", x->LengthPercentage.toString);
let paddingTop = x => p("paddingTop", x->LengthPercentage.toString);
let paddingBottom = x => p("paddingBottom", x->LengthPercentage.toString);

let border = (width, style, color) =>
  p("border", Border.toString(width, style, color));
let borderWidth = x => p("borderWidth", x->BorderWidth.toString);
let borderStyle = x => p("borderStyle", x->BorderStyle.toString);
let borderColor = x => p("borderColor", x->Color.toString);

let borderLeft = (width, style, color) =>
  p("borderLeft", Border.toString(width, style, color));
let borderLeftWidth = x => p("borderLeftWidth", x->BorderWidth.toString);
let borderLeftStyle = x => p("borderLeftStyle", x->BorderStyle.toString);
let borderLeftColor = x => p("borderLeftColor", x->Color.toString);

let borderRight = (width, style, color) =>
  p("borderRight", Border.toString(width, style, color));
let borderRightWidth = x => p("borderRightWidth", x->BorderWidth.toString);
let borderRightStyle = x => p("borderRightStyle", x->BorderStyle.toString);
let borderRightColor = x => p("borderRightColor", x->Color.toString);

let borderTop = (width, style, color) =>
  p("borderTop", Border.toString(width, style, color));
let borderTopWidth = x => p("borderTopWidth", x->BorderWidth.toString);
let borderTopStyle = x => p("borderTopStyle", x->BorderStyle.toString);
let borderTopColor = x => p("borderTopColor", x->Color.toString);

let borderBottom = (width, style, color) =>
  p("borderBottom", Border.toString(width, style, color));
let borderBottomWidth = x => p("borderBottomWidth", x->BorderWidth.toString);
let borderBottomStyle = x => p("borderBottomStyle", x->BorderStyle.toString);
let borderBottomColor = x => p("borderBottomColor", x->Color.toString);

let borderRadius = x => p("borderRadius", x->LengthPercentage.toString);
let borderTopLeftRadius = x =>
  p("borderTopLeftRadius", x->LengthPercentage.toString);
let borderTopRightRadius = x =>
  p("borderTopRightRadius", x->LengthPercentage.toString);
let borderBottomLeftRadius = x =>
  p("borderBottomLeftRadius", x->LengthPercentage.toString);
let borderBottomRightRadius = x =>
  p("borderBottomRightRadius", x->LengthPercentage.toString);

let borderCollapse = x => p("borderCollapse", x->BorderCollapse.toString);
let borderSpacing = x => p("borderSpacing", x->Length.toString);
let borderSpacing2 = (x, y) => p("borderSpacing", Length.toString2(x, y));

/* NOTE: `background` implementation is redundant due to its complexity */
let backgroundAttachment = x =>
  p("backgroundAttachment", x->BackgroundAttachment.toString);
let backgroundAttachments = x =>
  p(
    "backgroundAttachment",
    x->List.map(BackgroundAttachment.toString)->CssHelpers.joinWith(", "),
  );
let backgroundBlendMode = x =>
  p("backgroundBlendMode", x->BackgroundBlendMode.toString);
let backgroundBlendModes = x =>
  p(
    "backgroundBlendMode",
    x->List.map(BackgroundBlendMode.toString)->CssHelpers.joinWith(", "),
  );
let backgroundClip = x => p("backgroundClip", x->BackgroundBox.toString);
let backgroundColor = x => p("backgroundColor", x->Color.toString);
let backgroundImage = x => p("backgroundImage", x->BackgroundImage.toString);
let backgroundImages = x =>
  p(
    "backgroundImage",
    x->List.map(BackgroundImage.toString)->CssHelpers.joinWith(", "),
  );
let backgroundOrigin = x => p("backgroundOrigin", x->BackgroundBox.toString);
let backgroundPosition = x =>
  p("backgroundPosition", x->LengthPercentage.toString2);
let backgroundRepeat = x =>
  p("backgroundRepeat", x->BackgroundRepeat.toString);
let backgroundRepeat2 = x =>
  p("backgroundRepeat", x->BackgroundRepeat.toString2);
let backgroundSize = x => p("backgroundSize", x->BackgroundSize.toString);
let backgroundSizes = x =>
  p(
    "backgroundSize",
    x->List.map(BackgroundSize.toString)->CssHelpers.joinWith(", "),
  );

let boxShadow =
    (~x=`zero, ~y=`zero, ~blur=`zero, ~spread=`zero, ~inset=false, color) =>
  p("boxShadow", BoxShadow.toString(~x, ~y, ~blur, ~spread, ~inset, color));
let boxShadows = x =>
  p(
    "boxShadow",
    x
    ->List.mapU((. (x, y, blur, spread, color, inset)) =>
        BoxShadow.toString(~x, ~y, ~blur, ~spread, ~inset, color)
      )
    ->CssHelpers.joinWith(", "),
  );

let clipPath = x => p("clipPath", x->Url.toString);

let visibility = x => p("visibility", x->Visibility.toString);
let backfaceVisibility = x => p("backfaceVisibility", x->Visibility.toString);

let color = x => p("color", x->Color.toString);

let fontFamily = (x: string) => p("fontFamily", x);
let fontSize = x => p("fontSize", x->LengthPercentage.toString);
let fontWeight = (x: int) => p("fontWeight", {j|$x|j});
let fontStyle = x => p("fontStyle", x->FontStyle.toString);
let fontVariant = x => p("fontVariant", x->FontVariant.toString);
let fontKerning = x => p("fontKerning", x->FontKerning.toString);
let fontStretch = x => p("fontStretch", x->FontStretch.toString);
let src = (srcs: list((FontSrc.src, option(FontSrc.format)))) =>
  p(
    "src",
    srcs
    ->List.map(((src, format)) => FontSrc.toString(~format?, src))
    ->CssHelpers.joinWith(", "),
  );
let lineHeight = x => p("lineHeight", x->LineHeight.toString);
let letterSpacing = x => p("letterSpacing", x->LetterSpacing.toString);
let hyphens = x => p("hyphens", x->Hyphens.toString);

let textAlign = x => p("textAlign", x->TextAlign.toString);
let textDecoration = x => p("textDecoration", x->TextDecorationLine.toString);
let textDecorationColor = x => p("textDecorationColor", x->Color.toString);
let textDecorationStyle = x =>
  p("textDecorationStyle", x->TextDecorationStyle.toString);
let textIndent = x => p("textIndent", x->LengthPercentage.toString);
let textOverflow = x => p("textOverflow", x->TextOverflow.toString);
let textTransform = x => p("textTransform", x->TextTransform.toString);
let textShadow = (~x=`zero, ~y=`zero, ~blur=`zero, color) =>
  p("textShadow", color |> TextShadow.toString(~x, ~y, ~blur));
let textShadows = xs =>
  p(
    "textShadow",
    xs
    ->List.mapU((. (x, y, blur, color)) =>
        TextShadow.toString(~x, ~y, ~blur, color)
      )
    ->CssHelpers.joinWith(", "),
  );

let float = x => p("float", x->Float.toString);

let clear = x => p("clear", x->Clear.toString);

let overflow = x => p("overflow", x->Overflow.toString);
let overflowX = x => p("overflow-x", x->Overflow.toString);
let overflowY = x => p("overflow-y", x->Overflow.toString);
let overflowWrap = x => p("overflow-wrap", x->WordWrap.toString);

let whiteSpace = x => p("whiteSpace", x->WhiteSpace.toString);

let userSelect = x => p("userSelect", x->UserSelect.toString);

let verticalAlign = x => p("verticalAlign", x->VerticalAlign.toString);

let wordBreak = x => p("wordBreak", x->WordBreak.toString);
let wordSpacing = x => p("wordSpacing", x->WordSpacing.toString);
let wordWrap = x => p("wordWrap", x->WordWrap.toString);

let direction = x => p("direction", x->Direction.toString);

let listStyle = (style, position, image) =>
  p("listStyle", ListStyle.toString(style, position, image));
let listStyleType = x => p("listStyleType", x->ListStyleType.toString);
let listStylePosition = x =>
  p("listStylePosition", x->ListStylePosition.toString);
let listStyleImage = x => p("listStyleImage", x->ListStyleImage.toString);

let opacity = (x: float) => p("opacity", {j|$x|j});

let cursor = x => p("cursor", x->Cursor.toString);

let pointerEvents = x => p("pointerEvents", x->PointerEvents.toString);

let outline = (size, style, color) =>
  p("outline", Outline.toString(size, style, color));

let outlineStyle = x => p("outlineStyle", x->OutlineStyle.toString);
let outlineWidth = x => p("outlineWidth", x->Length.toString);
let outlineColor = x => p("outlineColor", x->Color.toString);
let outlineOffset = x => p("outlineOffset", x->Length.toString);

let tableLayout = x => p("tableLayout", x->TableLayout.toString);

let transition = (property: string, duration, timingFunction, delay) =>
  p(
    "transition",
    Transition.toString(~property, ~duration, ~delay, ~timingFunction),
  );

let transitions = xs =>
  p(
    "transition",
    xs
    ->List.map(((property, duration, timingFunction, delay)) =>
        Transition.toString(~property, ~duration, ~timingFunction, ~delay)
      )
    ->CssHelpers.joinWith(", "),
  );

let transitionProperty = (x: string) => p("transitionProperty", x);
let transitionDuration = x => p("transitionDuration", x->Timing.toString);
let transitionDelay = x => p("transitionDelay", x->Timing.toString);
let transitionTimingFunction = x =>
  p("transitionTimingFunction", x->TimingFunction.toString);

let transform = x => p("transform", x->Transform.toString);
let transforms = xs =>
  p(
    "transform",
    xs->List.map(Transform.toString)->CssHelpers.joinWith(" "),
  );
let transformOrigin = (x, y) =>
  p("transformOrigin", LengthPercentage.toString2(x, y));
let transformOrigin3d = (x, y, z) =>
  p("transformOrigin", LengthPercentage.toString3(x, y, z));
let transformStyle = x => p("transformStyle", x->TransformStyle.toString);

let perspective = x => p("perspective", x->Perspective.toString);

let perspectiveOrigin = (x, y) =>
  p("perspectiveOrigin", LengthPercentage.toString2(x, y));

let fill = x => p("fill", x->Color.toString);
let fillOpacity = (x: float) => p("fillOpacity", {j|$x|j});
let fillRule = x => p("fillRule", x->FillRule.toString);
let stroke = x => p("stroke", x->Color.toString);
let strokeWidth = x => p("strokeWidth", x->LengthPercentage.toString);
let strokeOpacity = (x: float) => p("strokeOpacity", {j|$x|j});
let strokeMiterlimit = (x: float) => p("strokeMiterlimit", {j|$x|j});
let strokeLinecap = x => p("strokeLinecap", x->StrokeLinecap.toString);
let strokeLinejoin = x => p("strokeLinejoin", x->StrokeLinejoin.toString);
let stopColor = x => p("stopColor", x->Color.toString);
let stopOpacity = (x: float) => p("stopOpacity", {j|$x|j});

let animation =
    (
      (
        name,
        duration,
        delay,
        direction,
        timingFunction,
        fillMode,
        playState,
        iterationCount,
      ),
    ) =>
  p(
    "animation",
    Animation.toString(
      ~name,
      ~duration,
      ~delay,
      ~direction,
      ~timingFunction,
      ~fillMode,
      ~playState,
      ~iterationCount,
    ),
  );
let animations = xs =>
  p(
    "animation",
    xs
    ->List.mapU(
        (.
          (
            name,
            duration,
            delay,
            direction,
            timingFunction,
            fillMode,
            playState,
            iterationCount,
          ),
        ) =>
        Animation.toString(
          ~name,
          ~duration,
          ~delay,
          ~direction,
          ~timingFunction,
          ~fillMode,
          ~playState,
          ~iterationCount,
        )
      )
    ->CssHelpers.joinWith(", "),
  );

let animationDelay = x => p("animationDelay", x->Timing.toString);
let animationDelays = x =>
  p(
    "animationDelay",
    x->List.map(Timing.toString)->CssHelpers.joinWith(", "),
  );
let animationDirection = x =>
  p("animationDirection", x->AnimationDirection.toString);
let animationDirections = x =>
  p(
    "animationDirection",
    x->List.map(AnimationDirection.toString)->CssHelpers.joinWith(", "),
  );
let animationDuration = x => p("animationDuration", x->Timing.toString);
let animationDurations = x =>
  p(
    "animationDuration",
    x->List.map(Timing.toString)->CssHelpers.joinWith(", "),
  );
let animationFillMode = x =>
  p("animationFillMode", x->AnimationFillMode.toString);
let animationFillModes = x =>
  p(
    "animationFillMode",
    x->List.map(AnimationFillMode.toString)->CssHelpers.joinWith(", "),
  );
let animationIterationCount = x =>
  p("animationIterationCount", x->AnimationIterationCount.toString);
let animationIterationCounts = x =>
  p(
    "animationIterationCount",
    x->List.map(AnimationIterationCount.toString)->CssHelpers.joinWith(", "),
  );
let animationName = (x: string) => p("animationName", x);
let animationNames = (x: list(string)) =>
  p("animationName", x->CssHelpers.joinWith(", "));
let animationPlayState = x =>
  p("animationPlayState", x->AnimationPlayState.toString);
let animationPlayStates = x =>
  p(
    "animationPlayState",
    x->List.map(AnimationPlayState.toString)->CssHelpers.joinWith(", "),
  );
let animationTimingFunction = x =>
  p("animationTimingFunction", x->TimingFunction.toString);
let animationTimingFunctions = x =>
  p(
    "animationTimingFunction",
    x->List.map(TimingFunction.toString)->CssHelpers.joinWith(", "),
  );

let filter = x => p("filter", x->Filter.toString);
let filters = x =>
  p("filter", x->List.map(Filter.toString)->CssHelpers.joinWith(" "));

let appearance = x => p("appearance", x->Appearance.toString);

let flex = x => p("flex", x->Flex.Flex.toString);
let flexGrow = (x: float) => p("flexGrow", {j|$x|j});
let flexShrink = (x: float) => p("flexShrink", {j|$x|j});
let flexBasis = x => p("flexBasis", x->LengthPercentageAuto.toString);
let flexDirection = x => p("flexDirection", x->Flex.Direction.toString);
let flexWrap = x => p("flexWrap", x->Flex.Wrap.toString);
let flexFlow = (direction, wrap) =>
  p("flex-flow", Flex.Flow.toString(direction, wrap));
let alignSelf = x => p("alignSelf", x->Flex.Align.toString);
let alignItems = x => p("alignItems", x->Flex.Align.toString);
let alignContent = x => p("alignContent", x->Flex.Align.toString);
let justifySelf = x => p("justifySelf", x->Flex.Justify.toString);
let justifyItems = x => p("justifyItems", x->Flex.Justify.toString);
let justifyContent = x => p("justifyContent", x->Flex.Justify.toString);
let order = (x: int) => p("order", {j|$x|j});

let zIndex = (x: int) => p("z-index", {j|$x|j});

let unsafe = p;

/* ===== 🔖 CSS Selectors ===== */

let select = (selector, declarations) =>
  [(selector, declarations->Declarations.toDict)]->Selector.box;

let active = select(":active");
let after = select("::after");
let before = select("::before");
let checked = select(":checked");
let disabled = select(":disabled");
let firstChild = select(":first-child");
let firstOfType = select(":first-of-type");
let focus = select(":focus");
let hover = select(":hover");
let lastChild = select(":last-child");
let lastOfType = select(":last-of-type");
let link = select(":link");
let readOnly = select(":read-only");
let required = select(":required");
let visited = select(":visited");
let enabled = select(":enabled");
let noContent = select(":empty");
let default = select(":default");
let anyLink = select(":any-link");
let onlyChild = select(":only-child");
let onlyOfType = select(":only-of-type");
let optional = select(":optional");
let invalid = select(":invalid");
let outOfRange = select(":out-of-range");
let target = select(":target");
let firstLine = select("::first-line");
let firstLetter = select("::first-letter");
let selection = select("::selection");
let placeholder = select("::placeholder");

/* refmt issue, should be just `not` */
let not_ = (selector: string, declarations) =>
  select({j|:not($selector)|j}, declarations);

let nthChild = (selector: string, declarations) =>
  select({j|:nth-child($selector)|j}, declarations);
let nthLastChild = (selector: string, declarations) =>
  select({j|:nth-last-child($selector)|j}, declarations);
let nthOfType = (selector: string, declarations) =>
  select({j|:nth-of-type($selector)|j}, declarations);
let nthLastOfType = (selector: string, declarations) =>
  select({j|:nth-last-of-type($selector)|j}, declarations);

/* ===== 🗂 @-rules ===== */

let media = (query: string, declarations) =>
  select({j|@media $query|j}, declarations);

let supports = (query: string, declarations) =>
  select({j|@supports $query|j}, declarations);

/* ===== 👩‍🎤 Emotion bindings ===== */

[@bs.module "emotion"] external css: Js.Dict.t(string) => string = "css";
let css = declarations => declarations->Declarations.toDict->css;

[@bs.module "emotion"]
external injectGlobal: Js.Dict.t(Js.Dict.t(string)) => unit = "injectGlobal";
let global = (selector, declarations) => {
  let css = Js.Dict.empty();
  css->(Js.Dict.set(selector, declarations->Declarations.toDict));
  css->injectGlobal;
};

[@bs.module "emotion"]
external makeKeyframes: Js.Dict.t(Js.Dict.t(string)) => string = "keyframes";
let keyframes = (frames: list((int, list(declaration)))) =>
  frames
  ->List.reduce(
      Js.Dict.empty(),
      (acc, item) => {
        let stop = item->fst;
        acc->Js.Dict.set({j|$stop%|j}, item->snd->Declarations.toDict);
        acc;
      },
    )
  ->makeKeyframes;

/* ===== 🌏 Global @-rules ===== */

let fontFace = declarations => global("@font-face", declarations);
let page = (~selectors=?, declarations) =>
  switch (selectors) {
  | Some(selectors) => global("@page " ++ selectors, declarations)
  | None => global("@page", declarations)
  };

/* @charset: impossible with Emotion */
/* @import: impossible with Emotion */
/* @namespace: impossible with Emotion */
