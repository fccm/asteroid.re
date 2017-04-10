open Canvas;

let stars = [|
  (100, 100),
  (150, 290),
  (190, 540),
  (350, 210),
  (550, 390),
  (600, 10)
|];

let draw = fun ctx offset::(offsetX, offsetY)=(0,0) ::speed=1 runTime => {
  fillStyle ctx "white";
  Array.iter (fun (baseX, baseY) => {
    let timeFactor = (int_of_float runTime) / 20;
    let x = baseX + offsetX;
    let y = (baseY + offsetY + speed * timeFactor) mod height;
    let size = 2;
    fillRect ctx x y size size;
  }) stars;
};
