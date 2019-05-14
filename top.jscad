const switchMinWidth = 13.9;
const switchMaxWidth = 15.5;
const spacing = 3.0;
const thickness = 5.0;
const latch = cube({ size: [switchMaxWidth, 4.9, -0.7], center: [1, 1, 0] });
const center = [1, 1, 0];
const stopper = cube({
  size: [3, 2.9, 3],
  center
});
const cableGuide = difference(
  cube({
    size: [switchMinWidth, 4, 2.9],
    center
  }),
  cylinder({
    r: 3,
    h: 10,
    center
  })
);
const contactSeparator = difference(
  cylinder({
    r: 4,
    h: 2.8,
    center
  }),
  cylinder({
    r: 3,
    h: 2.8,
    center
  }),
  cube({
    size: [8, 4, 2.8],
    center
  }).translate([0, -2, 0])
);
const main = () =>
  union(
    // stopper.translate([(switchMinWidth - 3) / 2, (switchMinWidth - 3) / 2, 0]),
    // stopper.translate([(switchMinWidth - 3) / -2, (switchMinWidth - 3) / 2, 0]),
    // cableGuide.translate([0, (switchMinWidth - 4) / -2, 0]),
    contactSeparator.translate([0, -switchMinWidth / 2, 0]),
    difference(
      union(
        cube({
          size: [
            switchMinWidth + spacing * 2,
            switchMinWidth + spacing * 2,
            thickness
          ],
          center
        }),
        cube({
          size: [
            switchMinWidth + spacing * 2,
            switchMinWidth + spacing * 2,
            thickness / 2
          ],
          center
        })
      ),
      cube({
        size: [switchMinWidth, switchMinWidth, thickness],
        center
      }),
      cube({
        size: [6, 4, 4.8],
        center,
        radius: 2
      }).translate([0, -switchMinWidth / 2, -2]),
      cube({
        size: [9, 0.6, 1.5],
        center,
        radius: 0.3
      }).translate([-5, -switchMinWidth / 2 - 1.5, -0.1]),
      cube({
        size: [5, 2, 3],
        center,
        radius: 1
      }).translate([-5.0, -switchMinWidth / 2 - 1.5, -1]),
      cube({
        size: [1.7, 100, 3],
        center,
        radius: 1.7 / 2
      }).translate([
        -switchMinWidth / 2 - 1.5,
        -switchMinWidth / 2 - 1.5,
        -0.6
      ]),
      latch.translate([0, switchMinWidth / 2 - 3.4, thickness - 1.2]),
      latch.translate([0, -(switchMinWidth / 2) + 3.4, thickness - 1.2])
    )
  );
