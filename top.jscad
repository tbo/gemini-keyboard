const switchMinWidth = 13.9;
const switchMaxWidth = 15.5;
const spacing = 5.0;
const thickness = 5.0;
const latch = cube({size: [4.9, switchMaxWidth, -0.7], center: [1, 1, 0]})
function main () {
    return difference(
        union(
            cube({
                size: [switchMinWidth + spacing, switchMinWidth + spacing, thickness], 
                center: [1, 1, 0], 
                radius: 1
            }),
            cube({
                size: [switchMinWidth + spacing, switchMinWidth + spacing, thickness / 2], 
                center: [1, 1, 0], 
                radius: [1, 1, 0]
            })
        ),
        cube({
            size: [switchMinWidth, switchMinWidth, thickness],
            center: [1, 1, 0], 
        }),
        latch.translate([(switchMinWidth / 2) - 3.4, 0, thickness - 1.2]),
        latch.translate([- (switchMinWidth / 2) + 3.4, 0, thickness - 1.2])
    )
}
