module V = Vector;

module Canvas = Canvas2d

let stars = [(100., 100.), (150., 290.), (190., 540.), (350., 210.), (550., 390.), (600., 10.)]

/* 0 < zDepth < 1, with a higher value representing a larger distance from the ship */
let draw = (ctx, ~offset as (offsetX, offsetY)=(0., 0.), ~zDepth=0.2, runTime) => {
  Canvas.save(ctx);
  Canvas.fillStyle(ctx, "rgba(255,255,255," ++ (Js.Float.toString(1.0 -. zDepth) ++ ")"));
  Array.iter(
    ((baseX, baseY)) => {
      let speed = 0.03 *. runTime /. zDepth;
      let x = baseX +. offsetX;
      let y = mod_float(baseY +. offsetY +. speed, Vector.worldHeight);
      let starSize = 2.;
      ctx -> Canvas.fillRect(~x, ~y, ~w=starSize, ~h=starSize)
    },
    stars
  );
  Canvas.restore(ctx)
};
