open Css

let container = css [
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

  (* :hover selector *)
  hover [
    borderRadius (`pct 50.);
    cursor `grab;
  ];
]

let text ~size = css [
  color (`hex "fff");
  fontSize (`px size);
  fontWeight 700;

  (* Transition takes tuple of (property, duration, timing-function, delay) *)
  transition ("font-size", `ms 100, `easeInOut, `ms 0);

  (* You can define multiple transitions as well *)
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
