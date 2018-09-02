module Css = ComponentStyles;

let component = ReasonReact.statelessComponent(__MODULE__);

let make = _ => {
  ...component,
  render: _ =>
    <div className=Css.container>
      <div className={Css.text(~size=30)}>
        {"Hi!" |> ReasonReact.string}
      </div>
    </div>,
};
