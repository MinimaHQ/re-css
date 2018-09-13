[@bs.module "emotion"] [@bs.splice]
external make: array(string) => string = "cx";

let ifTrue = (cn, x) => x ? cn : "";

let ifSome = (cn, x) =>
  switch (x) {
  | Some(_) => cn
  | None => ""
  };

let mapSome = (x, fn) =>
  switch (x) {
  | Some(x) => x->fn
  | None => ""
  };
