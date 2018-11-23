/* TODO: Globals: inherit, initial, unset */

/* ===== 🛠 Helpers ===== */

module Helpers = {
  let joinWith = (strings, separator) => {
    let rec run = (strings, acc) =>
      switch (strings) {
      | [] => acc
      | [x] => acc ++ x
      | [x, ...xs] => xs->run(acc ++ x ++ separator)
      };
    strings->run("");
  };
};

/* ===== 🎨 CSS Core Types ===== */

module Calc = {
  type op = [ | `add | `sub | `mult | `div];
  type n = [ | `n(float)];
  type t('a) = [ | `calc(op, 'a, 'a)];

  let (+) = (a, b) => `calc((`add, a, b));
  let (-) = (a, b) => `calc((`sub, a, b));
  let ( * ) = (a, b) => `calc((`mult, a, b));
  let (/) = (a, b) => `calc((`div, a, b));

  let opToString = (x: op) =>
    switch (x) {
    | `add => "+"
    | `sub => "-"
    | `mult => "*"
    | `div => "/"
    };

  let numToString = (x: n) =>
    switch (x) {
    | `n(x) => {j|$x|j}
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
  type t = [
    LengthUnit.t
    | Calc.t([ LengthUnit.t | Calc.n | Calc.t('a)] as 'a)
  ];

  let rec operandToString = x =>
    switch (x) {
    | #LengthUnit.t as x => x->LengthUnit.toString
    | #Calc.n as x => x->Calc.numToString
    | `calc(op, a, b) =>
      let op = op->Calc.opToString;
      let a = a->operandToString;
      let b = b->operandToString;
      {j|calc($a $op $b)|j};
    };

  let toString = (x: t) =>
    switch (x) {
    | #LengthUnit.t as x => x->LengthUnit.toString
    | `calc(op, a, b) =>
      let op = op->Calc.opToString;
      let a = a->operandToString;
      let b = b->operandToString;
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
  type t = [
    PercentageUnit.t
    | Calc.t([ PercentageUnit.t | Calc.n | Calc.t('a)] as 'a)
  ];

  let rec operandToString = x =>
    switch (x) {
    | #PercentageUnit.t as x => x->PercentageUnit.toString
    | #Calc.n as x => x->Calc.numToString
    | `calc(op, a, b) =>
      let op = op->Calc.opToString;
      let a = a->operandToString;
      let b = b->operandToString;
      {j|calc($a $op $b)|j};
    };

  let toString = (x: t) =>
    switch (x) {
    | #PercentageUnit.t as x => x->PercentageUnit.toString
    | `calc(op, a, b) =>
      let op = op->Calc.opToString;
      let a = a->operandToString;
      let b = b->operandToString;
      {j|calc($a $op $b)|j};
    };
};

module LengthPercentage = {
  type t = [
    LengthUnit.t
    | PercentageUnit.t
    | Calc.t([ LengthUnit.t | PercentageUnit.t | Calc.n | Calc.t('a)] as 'a)
  ];

  let rec operandToString = x =>
    switch (x) {
    | #LengthUnit.t as x => x->LengthUnit.toString
    | #PercentageUnit.t as x => x->PercentageUnit.toString
    | #Calc.n as x => x->Calc.numToString
    | `calc(op, a, b) =>
      let op = op->Calc.opToString;
      let a = a->operandToString;
      let b = b->operandToString;
      {j|calc($a $op $b)|j};
    };

  let toString = (x: t) =>
    switch (x) {
    | #LengthUnit.t as x => x->LengthUnit.toString
    | #PercentageUnit.t as x => x->PercentageUnit.toString
    | `calc(op, a, b) =>
      let op = op->Calc.opToString;
      let a = a->operandToString;
      let b = b->operandToString;
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
  type t = [ PercentageUnit.t | Calc.n | Calc.t('a)] as 'a;

  let rec toString = (x: t) =>
    switch (x) {
    | #PercentageUnit.t as x => x->PercentageUnit.toString
    | #Calc.n as x => x->Calc.numToString
    | `calc(op, a, b) =>
      let op = op->Calc.opToString;
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
      ->Helpers.joinWith(", ");
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

module BackgroundPosition = {
  module KeywordX = {
    type t = [
      | `left
      | `right
      | `center
      | `leftOffset(LengthPercentage.t)
      | `rightOffset(LengthPercentage.t)
      | `centerOffset(LengthPercentage.t)
    ];

    let toString = (x: t) =>
      switch (x) {
      | `left => "left"
      | `right => "right"
      | `center => "center"
      | `leftOffset(o) =>
        let x = "left";
        let o = o->LengthPercentage.toString;
        {j|$x $o|j};
      | `rightOffset(o) =>
        let x = "right";
        let o = o->LengthPercentage.toString;
        {j|$x $o|j};
      | `centerOffset(o) =>
        let x = "center";
        let o = o->LengthPercentage.toString;
        {j|$x $o|j};
      };
  };
  module KeywordY = {
    type t = [
      | `top
      | `bottom
      | `center
      | `topOffset(LengthPercentage.t)
      | `bottomOffset(LengthPercentage.t)
      | `centerOffset(LengthPercentage.t)
    ];

    let toString = (x: t) =>
      switch (x) {
      | `top => "top"
      | `bottom => "bottom"
      | `center => "center"
      | `topOffset(o) =>
        let x = "top";
        let o = o->LengthPercentage.toString;
        {j|$x $o|j};
      | `bottomOffset(o) =>
        let x = "bottom";
        let o = o->LengthPercentage.toString;
        {j|$x $o|j};
      | `centerOffset(o) =>
        let x = "center";
        let o = o->LengthPercentage.toString;
        {j|$x $o|j};
      };
  };

  type t = [
    | `values(LengthPercentage.t, LengthPercentage.t)
    | `keywords(KeywordX.t, KeywordY.t)
    | `initial
  ];

  let toString = (x: t) =>
    switch (x) {
    | `values(x, y) =>
      let x = x->LengthPercentage.toString;
      let y = y->LengthPercentage.toString;
      {j|$x $y|j};
    | `keywords(x, y) =>
      let x = x->KeywordX.toString;
      let y = y->KeywordY.toString;
      {j|$x $y|j};
    | `initial => "initial"
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
};

module Grid = {
  module Flex = {
    type t = [ | `fr(float)];

    let toString = (x: t) =>
      switch (x) {
      | `fr(x) => {j|$(x)fr|j}
      };
  };

  module MinMax = {
    type t = [ | `minmax(min, max)]
    and min = [ LengthPercentage.t | `minContent | `maxContent | `auto]
    and max = [
      LengthPercentage.t
      | Flex.t
      | `minContent
      | `maxContent
      | `auto
    ];

    let minToString = (x: min) =>
      switch (x) {
      | #LengthPercentage.t as x => x->LengthPercentage.toString
      | `minContent => "min-content"
      | `maxContent => "max-content"
      | `auto => "auto"
      };

    let maxToString = (x: max) =>
      switch (x) {
      | #LengthPercentage.t as x => x->LengthPercentage.toString
      | #Flex.t as x => x->Flex.toString
      | `minContent => "min-content"
      | `maxContent => "max-content"
      | `auto => "auto"
      };

    let toString = (x: t) =>
      switch (x) {
      | `minmax(min, max) =>
        let min = min->minToString;
        let max = max->maxToString;
        {j|minmax($min, $max)|j};
      };
  };

  module FitContent = {
    type t = [ | `fitContent(LengthPercentage.t)];

    let toString = (x: t) =>
      switch (x) {
      | `fitContent(x) =>
        let x = x->LengthPercentage.toString;
        {j|fit-content($x)|j};
      };
  };

  module Repeat = {
    type t = [ | `repeat(value, list(trackList))]
    and value = [ | `n(int) | `autoFill | `autoFit]
    and trackList = [
      LengthPercentage.t
      | Flex.t
      | MinMax.t
      | `minContent
      | `maxContent
      | `auto
    ];

    let valueToString = (x: value) =>
      switch (x) {
      | `n(x) => {j|$x|j}
      | `autoFill => "auto-fill"
      | `autoFit => "auto-fit"
      };
    let trackListToString = (x: trackList) =>
      switch (x) {
      | #LengthPercentage.t as x => x->LengthPercentage.toString
      | #Flex.t as x => x->Flex.toString
      | #MinMax.t as x => x->MinMax.toString
      | `minContent => "min-content"
      | `maxContent => "max-content"
      | `auto => "auto"
      };
    let toString = (x: t) =>
      switch (x) {
      | `repeat(value, trackList) =>
        let value = value->valueToString;
        let trackList =
          trackList->List.map(trackListToString)->Helpers.joinWith(" ");
        {j|repeat($(value), $(trackList))|j};
      };
  };

  module AutoRows = {
    type t = [
      LengthPercentage.t
      | Flex.t
      | MinMax.t
      | `minContent
      | `maxContent
      | `auto
    ];

    let toString = (x: t) =>
      switch (x) {
      | #LengthPercentage.t as x => x->LengthPercentage.toString
      | #Flex.t as x => x->Flex.toString
      | #MinMax.t as x => x->MinMax.toString
      | `minContent => "min-content"
      | `maxContent => "max-content"
      | `auto => "auto"
      };
  };

  module AutoColumns = {
    type t = [
      LengthPercentage.t
      | Flex.t
      | FitContent.t
      | MinMax.t
      | `minContent
      | `maxContent
      | `auto
    ];

    let toString = (x: t) =>
      switch (x) {
      | #LengthPercentage.t as x => x->LengthPercentage.toString
      | #Flex.t as x => x->Flex.toString
      | #FitContent.t as x => x->FitContent.toString
      | #MinMax.t as x => x->MinMax.toString
      | `minContent => "min-content"
      | `maxContent => "max-content"
      | `auto => "auto"
      };
  };

  module Template = {
    type t = [ | `list(list(value)) | `none]
    and value = [
      LengthPercentage.t
      | Flex.t
      | MinMax.t
      | FitContent.t
      | Repeat.t
      | `minContent
      | `maxContent
      | `auto
    ];

    let valueToString = (x: value) =>
      switch (x) {
      | #LengthPercentage.t as x => x->LengthPercentage.toString
      | #Flex.t as x => x->Flex.toString
      | #MinMax.t as x => x->MinMax.toString
      | #FitContent.t as x => x->FitContent.toString
      | #Repeat.t as x => x->Repeat.toString
      | `minContent => "min-content"
      | `maxContent => "max-content"
      | `auto => "auto"
      };

    let toString = (x: t) =>
      switch (x) {
      | `list(xs) =>
        let xs = xs->List.map(valueToString)->Helpers.joinWith(" ");
        {j|$xs|j};
      | `none => "none"
      };
  };

  module Line = {
    type t = [
      | `auto
      | `n(int)
      | `ident(string)
      | `nIdent(int, string)
      | `span([ | `n(int) | `ident(string) | `nIdent(int, string)])
    ];

    let toString = (x: t) =>
      switch (x) {
      | `auto => "auto"
      | `n(x) => {j|$x|j}
      | `ident(x) => x
      | `nIdent(n, ident) => {j|$n $ident|j}
      | `span(`n(x)) => {j|span $x|j}
      | `span(`ident(x)) => {j|span $x|j}
      | `span(`nIdent(n, ident)) => {j|span $n $ident|j}
      };
  };

  module Gap = {
    type t = [ LengthPercentage.t | `normal];

    let toString = (x: t) =>
      switch (x) {
      | #LengthPercentage.t as x => x->LengthPercentage.toString
      | `normal => "normal"
      };
  };

  module TemplateAreas = {
    type t = [ | `areas(list(string)) | `none];

    let toString = (x: t) =>
      switch (x) {
      | `areas(xs) => xs->List.map(x => {j|"$x"|j})->Helpers.joinWith("\n")
      | `none => "none"
      };
  };

  module AutoFlow = {
    type t = [ | `row | `column | `rowDense | `columnDense];

    let toString = (x: t) =>
      switch (x) {
      | `row => "row"
      | `column => "column"
      | `rowDense => "row dense"
      | `columnDense => "column dense"
      };
  };
};

module AlignItems = {
  type t = [
    | `normal
    | `stretch
    | `baseline
    | `firstBaseline
    | `lastBaseline
    | `center
    | `start
    | `end_
    | `selfStart
    | `selfEnd
    | `flexStart
    | `flexEnd
  ];

  let toString = (x: t) =>
    switch (x) {
    | `normal => "normal"
    | `stretch => "stretch"
    | `baseline => "baseline"
    | `firstBaseline => "first baseline"
    | `lastBaseline => "last baseline"
    | `center => "center"
    | `start => "start"
    | `end_ => "end"
    | `selfStart => "self-start"
    | `selfEnd => "self-end"
    | `flexStart => "flex-start"
    | `flexEnd => "flex-end"
    };
};

module AlignSelf = {
  type t = [
    | `auto
    | `normal
    | `stretch
    | `baseline
    | `firstBaseline
    | `lastBaseline
    | `center
    | `start
    | `end_
    | `selfStart
    | `selfEnd
    | `flexStart
    | `flexEnd
  ];

  let toString = (x: t) =>
    switch (x) {
    | `auto => "auto"
    | `normal => "normal"
    | `stretch => "stretch"
    | `baseline => "baseline"
    | `firstBaseline => "first baseline"
    | `lastBaseline => "last baseline"
    | `center => "center"
    | `start => "start"
    | `end_ => "end"
    | `selfStart => "self-start"
    | `selfEnd => "self-end"
    | `flexStart => "flex-start"
    | `flexEnd => "flex-end"
    };
};

module AlignContent = {
  type t = [
    | `normal
    | `baseline
    | `firstBaseline
    | `lastBaseline
    | `spaceBetween
    | `spaceAround
    | `spaceEvenly
    | `stretch
    | `center
    | `start
    | `end_
    | `flexStart
    | `flexEnd
  ];

  let toString = (x: t) =>
    switch (x) {
    | `normal => "normal"
    | `baseline => "baseline"
    | `firstBaseline => "first baseline"
    | `lastBaseline => "last baseline"
    | `spaceBetween => "space-between"
    | `spaceAround => "space-around"
    | `spaceEvenly => "space-evenly"
    | `stretch => "stretch"
    | `center => "center"
    | `start => "start"
    | `end_ => "end"
    | `flexStart => "flex-start"
    | `flexEnd => "flex-end"
    };
};

module JustifyItems = {
  type t = [
    | `normal
    | `stretch
    | `baseline
    | `firstBaseline
    | `lastBaseline
    | `center
    | `start
    | `end_
    | `selfStart
    | `selfEnd
    | `flexStart
    | `flexEnd
    | `left
    | `right
  ];

  let toString = (x: t) =>
    switch (x) {
    | `normal => "normal"
    | `stretch => "stretch"
    | `baseline => "baseline"
    | `firstBaseline => "first baseline"
    | `lastBaseline => "last baseline"
    | `center => "center"
    | `start => "start"
    | `end_ => "end"
    | `selfStart => "self-start"
    | `selfEnd => "self-end"
    | `flexStart => "flex-start"
    | `flexEnd => "flex-end"
    | `left => "left"
    | `right => "right"
    };
};

module JustifySelf = {
  type t = [
    | `auto
    | `normal
    | `stretch
    | `baseline
    | `firstBaseline
    | `lastBaseline
    | `center
    | `start
    | `end_
    | `selfStart
    | `selfEnd
    | `flexStart
    | `flexEnd
    | `left
    | `right
  ];

  let toString = (x: t) =>
    switch (x) {
    | `auto => "auto"
    | `normal => "normal"
    | `stretch => "stretch"
    | `baseline => "baseline"
    | `firstBaseline => "first baseline"
    | `lastBaseline => "last baseline"
    | `center => "center"
    | `start => "start"
    | `end_ => "end"
    | `selfStart => "self-start"
    | `selfEnd => "self-end"
    | `flexStart => "flex-start"
    | `flexEnd => "flex-end"
    | `left => "left"
    | `right => "right"
    };
};

module JustifyContent = {
  type t = [
    | `normal
    | `spaceBetween
    | `spaceAround
    | `spaceEvenly
    | `stretch
    | `center
    | `start
    | `end_
    | `flexStart
    | `flexEnd
    | `left
    | `right
  ];

  let toString = (x: t) =>
    switch (x) {
    | `normal => "normal"
    | `spaceBetween => "space-between"
    | `spaceAround => "space-around"
    | `spaceEvenly => "space-evenly"
    | `stretch => "stretch"
    | `center => "center"
    | `start => "start"
    | `end_ => "end"
    | `flexStart => "flex-start"
    | `flexEnd => "flex-end"
    | `left => "left"
    | `right => "right"
    };
};

module BasicShape = {
  module ShapePosition = {
    type t = [ | `keyword(Position.t) | `value(LengthPercentage.t)];

    let toString = (x: t) =>
      switch (x) {
      | `keyword(x) => x->Position.toString
      | `value(x) => x->LengthPercentage.toString
      };
  };

  module FillRule = {
    type t = [ | `nonzero | `evenodd];

    let toString = (x: t) =>
      switch (x) {
      | `nonzero => "nonzero"
      | `evenodd => "evenodd"
      };
  };

  module ShapeRadius = {
    type t = [ LengthPercentage.t | `closestSide | `farthestSide];

    let toString = (x: t) =>
      switch (x) {
      | #LengthPercentage.t as x => x->LengthPercentage.toString
      | `farthestSide => "farthest-side"
      | `closestSide => "closest-side"
      };
  };

  type t = [
    | `inset(
        LengthPercentage.t,
        LengthPercentage.t,
        LengthPercentage.t,
        LengthPercentage.t,
        option(LengthPercentage.t),
      )
    | `circle(ShapeRadius.t, option((ShapePosition.t, ShapePosition.t)))
    | `ellipse(
        ShapeRadius.t,
        ShapeRadius.t,
        option((ShapePosition.t, ShapePosition.t)),
      )
    | `polygon(
        list((LengthPercentage.t, LengthPercentage.t)),
        option(FillRule.t),
      )
    | `path(string, option(FillRule.t))
  ];

  let toString = (x: t) =>
    switch (x) {
    | `inset(top, right, bottom, left, borderRadius) =>
      let top = top->LengthPercentage.toString;
      let right = right->LengthPercentage.toString;
      let bottom = bottom->LengthPercentage.toString;
      let left = left->LengthPercentage.toString;
      let borderRadius =
        borderRadius
        ->Option.map(borderRadius =>
            " round " ++ borderRadius->LengthPercentage.toString
          )
        ->Option.getWithDefault("");
      {j|$top $right $bottom $left$borderRadius|j};
    | `circle(radius, atPosition) =>
      let radius = radius->ShapeRadius.toString;
      let atPosition =
        atPosition
        ->Option.map(atPosition => {
            let at1 = atPosition->fst->ShapePosition.toString;
            let at2 = atPosition->snd->ShapePosition.toString;
            {j| at $at1 $at2|j};
          })
        ->Option.getWithDefault("");
      {j|$radius$atPosition|j};
    | `ellipse(rX, rY, atPosition) =>
      let rX = rX->ShapeRadius.toString;
      let rY = rY->ShapeRadius.toString;
      let atPosition =
        atPosition
        ->Option.map(atPosition => {
            let at1 = atPosition->fst->ShapePosition.toString;
            let at2 = atPosition->snd->ShapePosition.toString;
            {j| at $at1 $at2|j};
          })
        ->Option.getWithDefault("");
      {j|$rX $rY$atPosition|j};
    | `polygon(shapeArgs, fillRule) =>
      let fillRule =
        fillRule
        ->Option.map(fillRule => fillRule->FillRule.toString ++ ",")
        ->Option.getWithDefault("");
      let args =
        shapeArgs
        ->List.map(arg => {
            let a1 = arg->fst->LengthPercentage.toString;
            let a2 = arg->snd->LengthPercentage.toString;
            {j|$a1 $a2|j};
          })
        ->List.toArray
        ->Js.Array.joinWith(",", _);
      {j|$fillRule$args|j};
    | `path(path, fillRule) =>
      let fillRule =
        fillRule
        ->Option.map(fillRule => fillRule->FillRule.toString ++ ",")
        ->Option.getWithDefault("");
      {j|$fillRule$path|j};
    };
};

module GeometryBox = {
  type t = [
    | `marginBox
    | `borderBox
    | `paddingBox
    | `contentBox
    | `fillBox
    | `strokeBox
    | `viewBox
  ];

  let toString = (x: t) =>
    switch (x) {
    | `marginBox => "margin-box"
    | `borderBox => "border-box"
    | `paddingBox => "padding-box"
    | `contentBox => "content-box"
    | `fillBox => "fill-box"
    | `strokeBox => "stroke-box"
    | `viewBox => "view-box"
    };
};

module ClipPath = {
  type t = [
    | `none
    | `initial
    | `unset
    | `url(Url.t)
    | `box(GeometryBox.t)
    | `shape(BasicShape.t)
    | `boxShape(GeometryBox.t, BasicShape.t)
  ];

  let toString = (x: t) =>
    switch (x) {
    | `none => "none"
    | `initial => "initial"
    | `unset => "unset"
    | `url(url) => url->Url.toString
    | `box(box) => box->GeometryBox.toString
    | `shape(shape) => shape->BasicShape.toString
    | `boxShape(box, shape) =>
      let box = box->GeometryBox.toString;
      let shape = shape->BasicShape.toString;
      {j|$box $shape|j};
    };
};
