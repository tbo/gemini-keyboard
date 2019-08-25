const switchMinWidth = 13.9;
const switchMaxWidth = 15.5;
const spacing = 2.25;
const thickness = 5.1;
const latch = cube({ size: [switchMaxWidth, 4.9, -0.7], center: [1, 1, 0] });
const center = [1, 1, 0];
const boxSize = switchMinWidth + spacing * 2;
const range = to => [...Array(to).keys()];
const flatten = arr => [].concat.apply([], arr);

const f = (a, b) => [].concat(...a.map(d => b.map(e => [].concat(d, e))));
const cartesian = (a, b, ...c) => (b ? cartesian(f(a, b), ...c) : a);

const ROWS = 2;
const COLUMNS = 3;
const SWITCH_COUNT = ROWS * COLUMNS;
const controllerHolder = difference(
  cube({
    size: [22, 2 * boxSize - 3, thickness],
    center,
    radius: [1, 1, 0]
  }).translate([0, 4.4, 0]),
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
    }).translate([-5.5, 1.75, 0]),
    cube({
      size: [7, 26, 3],
      center
    }).translate([0, 1.75, 0])
  ).translate([0, 2.8, 0])
);

const switchHolderCutout = union(
  cube({
    size: [switchMinWidth, switchMinWidth, thickness],
    center
  }),
  cube({
    size: [switchMinWidth + 2.5, switchMinWidth + 2.5, 6],
    radius: 2.2,
    resolution: 30,
    center
  }).translate([0, 0, -3]),
  latch.translate([0, switchMinWidth / 2 - 3.4, thickness - 1.1]),
  latch.translate([0, -(switchMinWidth / 2) + 3.4, thickness - 1.1])
);

const reverse = arr => arr.slice().reverse();

const getPosition = index => {
  const offset = [0, 2, 1, -2];
  const column = index % COLUMNS;
  const row = Math.floor(index / COLUMNS);
  return [column * boxSize, row * (boxSize - 0.5) + offset[column], 0];
};

const getJuncture = (index, connectorId) => {
  const [x, y, z] = getPosition(index);
  const base = switchMinWidth / 2;

  const offsets = [-5.9, -3, 0, 3, 5.9];
  return [
    [...offsets, ...range(5).fill(base), ...reverse(offsets), ...range(5).fill(-base)][connectorId] + x,
    [...range(5).fill(base), ...reverse(offsets), ...range(5).fill(-base), ...offsets][connectorId] + y,
    1.2 + z
  ];
};

const getConnector = (start, end) => scale([1.0, 1.0, 1.2], cylinder({ start, end, center, r: 1 }));

const switches = range(SWITCH_COUNT).map(index => switchHolderCutout.translate(getPosition(index)));
const switches2d = range(SWITCH_COUNT).map(index =>
  square({
    size: [switchMinWidth, switchMinWidth],
    center: true
  }).translate(getPosition(index))
);

const verticalMatches = [[14, 0], [13, 1], [12, 2], [11, 3], [10, 4]];
const verticalConnectors = cartesian(range(SWITCH_COUNT - COLUMNS), verticalMatches).map(([index, from, to]) => [
  [index, from],
  [index + COLUMNS, to]
]);
const horizontalMatches = [[5, 19], [6, 18], [7, 17], [8, 16], [9, 15]];
const horizontalConnectors = cartesian(range(SWITCH_COUNT), horizontalMatches)
  .filter(([index]) => (index + 1) % COLUMNS)
  .map(([index, from, to]) => [[index, from], [index + 1, to]]);

const controllerConnectors = cartesian([0, 3], [19, 18, 17, 16, 15]).map(([index, from]) => {
  const [x, y, z] = getJuncture(index, from);
  return getConnector([x, y, z], [x - 10, y, z]);
});

const connectors = verticalConnectors
  .concat(horizontalConnectors)
  // .concat(controllerConnectors)
  .map(([from, to]) => getConnector(getJuncture(...from), getJuncture(...to)));

const main = () =>
  difference(
    union(
      controllerHolder.translate([-19.5, 4.5, 0]),
      rectangular_extrude(
        hull(...switches2d).toPoints(), // path is an array of 2d coords
        { w: 3.8, h: thickness, closed: true }
      ),
      linear_extrude({ height: thickness }, hull(...switches2d))
    ),
    ...switches,
    ...connectors,
    ...controllerConnectors
  ).center([true, true, false]);
