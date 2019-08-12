const switchMinWidth = 13.9;
const switchMaxWidth = 15.5;
const spacing = 3.0;
const thickness = 5.0;
const latch = cube({ size: [switchMaxWidth, 4.9, -0.7], center: [1, 1, 0] });
const center = [1, 1, 0];
const boxSize = switchMinWidth + spacing * 2;
const cableHolder = cylinder({
  r: 1,
  h: boxSize * 4,
  center
})
  .rotateX(90)
  .translate([0, boxSize / 2 + 30, 1.5]);

const range = to => [...Array(to).keys()];
const controllerHolder = difference(
  cube({
    size: [22, 2 * boxSize, thickness],
    center,
    radius: [1, 1, 0.3]
  }).translate([0, 2.9, 0]),
  union(
    cube({
      size: [18.4, 33.4, 1.8],
      center,
      radius: 0.3
    }).translate([0, 2.7, 3]),
    cube({
      size: [18.1, 34, 1.6],
      center,
      radius: 0.3
    }).translate([0, 3.8, 4.5]),
    cube({
      size: [7, 30.5, 3],
      center
    }).translate([5.5, 1.75, 0]),
    cube({
      size: [7, 30.5, 3],
      center
    }).translate([-5.5, 1.75, 0])
  ).translate([0, 2.8, 0]),
  cableHolder.rotateZ(90).translate([35, 7, 0]),
  cableHolder.rotateZ(90).translate([35, 9.9, 0]),
  cableHolder.rotateZ(90).translate([35, 12.9, 0]),
  cableHolder.rotateZ(90).translate([35, 15.9, 0]),
  cableHolder.rotateZ(90).translate([35, 18.8, 0]),
  cableHolder.rotateZ(90).translate([35, -1.1, 0]),
  cableHolder.rotateZ(90).translate([35, -4, 0]),
  cableHolder.rotateZ(90).translate([35, -7, 0]),
  cableHolder.rotateZ(90).translate([31, -10, 0])
);

const switchHolder = union(
  difference(
    union(
      cube({
        size: [boxSize, boxSize, thickness],
        center,
        radius: [1, 1, 0.3]
      }),
      cube({
        size: [boxSize, boxSize, thickness / 2],
        center,
        radius: [1, 1, 0.3]
      })
    ),
    cube({
      size: [switchMinWidth, switchMinWidth, thickness],
      center
    }),
    cableHolder,
    cableHolder.translate([3, 0, 0]),
    cableHolder.translate([5.9, 0, 0]),
    cableHolder.translate([-3, 0, 0]),
    cableHolder.translate([-5.9, 0, 0]),
    cableHolder.rotateZ(90),
    cableHolder.rotateZ(90).translate([0, 3, 0]),
    cableHolder.rotateZ(90).translate([0, 5.9, 0]),
    cableHolder.rotateZ(90).translate([0, -3, 0]),
    cableHolder.rotateZ(90).translate([0, -5.9, 0]),
    latch.translate([0, switchMinWidth / 2 - 3.4, thickness - 1.2]),
    latch.translate([0, -(switchMinWidth / 2) + 3.4, thickness - 1.2])
  )
);

const main = () =>
  union(
    ...range(6).map(index =>
      switchHolder.translate([(index % 3) * boxSize, (index >= 3) * boxSize, 0])
    ),
    controllerHolder.translate([-boxSize / 2 - 11, 7, 0]),
    difference(
      cube({
        radius: [1, 1, 0.3],
        size: [3 * boxSize, 2 * boxSize, thickness]
      }),
      union(
        ...range(6).map(index =>
          cube({
            center,
            size: [switchMinWidth, switchMinWidth, thickness]
          }).translate([
            (index % 3) * boxSize + boxSize / 2,
            (index >= 3) * boxSize + boxSize / 2,
            0
          ])
        )
      ),
      cube({
        size: [3 * boxSize - spacing + 1, 2 * boxSize - 2 * spacing, 1]
      }).translate([0, spacing, 3]),
      cube({
        size: [3 * boxSize - spacing, 2 * boxSize - 2 * spacing, 2]
      }).translate([0, spacing, 0.5])
    ).translate([-boxSize / 2, -boxSize / 2, 0])
  );
