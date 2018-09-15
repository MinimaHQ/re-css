# re-css

Typed CSS for ReasonML. Uses [Emotion](https://emotion.sh) runtime.

## Installation
Not published to `npm` yet (I'm not sure about the name of the package).

Pull it from GH repo, for now:

```shell
# yarn
yarn add alexfedoseev/re-css#master
# or npm
npm install --save alexfedoseev/re-css#master
```

Then add it to `bsconfig.json`:

```json
"bs-dependencies": [
  "re-css"
]
```

## Usage

### Defining styles

_Example is in OCaml syntax, but you can use Reason too._

```ocaml
(* ComponentStyles.ml *)

open Css

let container = css [
  (* label hashed classnames for readability *)
  (* I prolly should do ppx to automate that *)
  label "container";

  display `flex;
  flexFlow `column `nowrap;
  alignItems `center;
]

let shape = css [
  label "shape";

  display `flex;
  flexFlow `row `nowrap;
  alignItems `center;
  justifyContent `center;
  transitionProperty "border-radius";
  transitionDuration (`ms 100);
  transitionTimingFunction `easeInOut;
  width (`px 200);
  height (`px 200);
  borderRadius (`px 6);
  backgroundColor (`hex "29d");

  (* :hover selector, same as `select ":hover" [ ... ]` *)
  hover [
    borderRadius (`pct 50.);
    cursor `grab;
  ];
]

(* Dynamic styling *)
let text ~size = css [
  label "text";

  color (`hex "fff");
  fontSize (`px size);
  fontWeight 700;

  (* Transition takes property, duration, timing-function & delay *)
  transition "font-size" (`ms 100) `easeInOut `zero;

  (* You can define multiple transitions by packing them into list of tuples *)
  transitions [
    ("font-size", `ms 100, `easeInOut, `ms 0);
  ];

  (* Complex selector that uses .container class defined above *)
  (* Rendered as: `.container:hover .text {...}` *)
  select {j|.$container:hover &|j} [
    fontSize Calc.((`px size) + (`pct 150.));
  ];

  (* @media quiery with nested selectors *)
  media "(max-width: 900px)" [
    color (`hex "ff69b4");

    select ":hover" [
      color (`hex "fff");
    ];
  ];
]

(* Define keyframes *)
let bounce = keyframes [
  (0,   [ transform (`translateY `zero); ]);
  (50,  [ transform (`translateY (`px (-20))); ]);
  (100, [ transform (`translateY `zero); ]);
]

let animated = css [
  label "animated";

  (* Use generated animation name *)
  animationName bounce;
  animationDuration (`ms 300);
  animationIterationCount (`i 7);
]

(* Compose things *)
let smallText = css [
  label "smallText";

  fontSize (`em 0.8);
]

let note = css ~extend: smallText [
  label "note";

  marginTop (`px 10);
]
```

### Applying styles
```reason
/* Component.re */

module Css = ComponentStyles;

let component = ReasonReact.statelessComponent(__MODULE__);

let make = _ => {
  ...component,
  render: _ => <div className=Css.container> ... </div>,
};
```

#### Cn
This package exposes `Cn` module from [`re-classnames`](https://github.com/alexfedoseev/re-classnames). You can use it to combine classnames together. See `re-classnames` docs for details.

#### Cx
`Cx.merge` is a binding to [Emotion's `cx`](https://emotion.sh/docs/cx) function. See the next section.

#### Caveats
First, what is the difference between `Cn.make` and `Cx.merge`:

```reason
Cn.make([Css.one, Css.two]) /* => "css-<HASH>-one css-<HASH>-two" */
Cx.merge([Css.one, Css.two]) /* => "css-<HASH>-one-two" */
```

If the former simply concatenates two classnames in single string (and as a result 2 css classes are applied) but the latter merges 2 Emotion classes into single unique classname and applies it to a node.

Caveat:

```reason
<div className={Cx.merge([|Css.foo, Css.bar|])}>
  <button className=Css.button />
</div>
```

```ocaml
let foo = css [ ... ]
let bar = css [ ... ]

let button = css [
  ...

  select {j|.$foo:hover &|j} [
    (*
      It won't work due to `.foo` class is being merged
      w/ `.bar` into single unique classname inside component
    *)
  ]
]
```

To make this css work you can use `Cn.make` or rewrite css using functions (for now, I prefer the former):

```reason
<div className={Cn.make([Css.foo, Css.bar])} />
```
