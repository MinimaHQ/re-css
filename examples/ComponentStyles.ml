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
