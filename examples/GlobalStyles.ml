open Css;;

begin
  global "html, body" [
    width (`pct 100.);
    height (`pct 100.);
    fontFamily "Tahoma, sans-serif";
  ];

  global "div" [
    supports "(display: flex)" [
      display `flex;
    ];
  ];

  global "#app" [
    display `flex;
    position `relative;
    flexFlow `row `nowrap;
    alignItems `center;
    justifyContent `center;
    width (`pct 100.);
    height (`pct 100.);
  ];

  fontFace [
    fontFamily "MyFont";
    fontWeight 400;
    fontStyle `normal;
    src [
      (`url "my-font.woff", Some `woff);
      (`url "my-font.woff2", Some `woff2);
    ];
  ];
end
