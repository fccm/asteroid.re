@get external eventKey : 'a => string = "key";

let withDefault = (fallback, input) =>
  switch input {
  | None => fallback
  | Some(x) => x
  };

type cmd =
  | ShipUp(int)
  | ShipDown(int)
  | ShipLeft(int)
  | ShipRight(int)
  | ShipShoot;

type keyState = {
  mutable left: option<int>,
  mutable right: option<int>,
  mutable up: option<int>,
  mutable down: option<int>,
  mutable space: bool
};

let currentState = {left: None, right: None, up: None, down: None, space: false};

let keydownListener = (evt) => {
  Canvas2d.preventDefault(evt);
  switch (eventKey(evt)) {
  | "ArrowLeft" => currentState.left = Some(withDefault(1, currentState.left))
  | "ArrowRight" => currentState.right = Some(withDefault(1, currentState.right))
  | "ArrowUp" => currentState.up = Some(withDefault(1, currentState.up))
  | "ArrowDown" => currentState.down = Some(withDefault(1, currentState.down))
  | " " => currentState.space = true
  | _ => ()
  }
};

let keyupListener = (evt) =>
  switch (eventKey(evt)) {
  | "ArrowLeft" => currentState.left = None
  | "ArrowRight" => currentState.right = None
  | "ArrowUp" => currentState.up = None
  | "ArrowDown" => currentState.down = None
  | " " => currentState.space = false
  | _ => ()
  };

let bindListeners = () => {
  Canvas2d.addEventListener(Canvas2d.window, "keydown", (evt) => keydownListener(evt), false)
  Canvas2d.addEventListener(Canvas2d.window, "keyup", (evt) => keyupListener(evt), false)
};

let sample = () => {
  switch currentState {
  | {up: Some(n)} => currentState.up = Some(n + 1)
  | {down: Some(n)} => currentState.down = Some(n + 1)
  | _ => ()
  };
  switch currentState {
  | {left: Some(n)} => currentState.left = Some(n + 1)
  | {right: Some(n)} => currentState.right = Some(n + 1)
  | _ => ()
  };
  let spaceList = currentState.space ? list{ShipShoot} : list{};
  let xCmdList =
    switch currentState {
    | {left: Some(n), right: None} => list{ShipLeft(n)}
    | {right: Some(n), left: None} => list{ShipRight(n)}
    | _ => list{}
    };
  let yCmdList =
    switch currentState {
    | {up: Some(n), down: None} => list{ShipUp(n)}
    | {down: Some(n), up: None} => list{ShipDown(n)}
    | _ => list{}
    };
  List.concat(list{xCmdList, yCmdList, spaceList})
};
