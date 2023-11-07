difference() {
  difference() {
    difference() {
      difference() {
        difference() {
          cube([100.0, 65.0, 14.0]);
          translate([0.0,-1.0,0.0]) 
            translate([8.0,0.0,5.0]) 
              polyhedron( points=[ [0.0,0.0,0.0]
              , [85.2,0.0,0.0]
              , [84.2,61.0,0.0]
              , [2.1,61.0,0.0]
              , [0.0,0.0,4.73]
              , [85.2,0.0,4.73]
              , [84.2,61.0,4.2299999999999995]
              , [2.1,61.0,4.2299999999999995] ]
              , faces=[ [0, 1, 2, 3]
              , [4, 5, 1, 0]
              , [7, 6, 5, 4]
              , [5, 6, 2, 1]
              , [6, 7, 3, 2]
              , [7, 4, 0, 3] ]
              , convexity=10 );
        }
        translate([20.5,-1.0,-3.0]) cube([61.0, 61.0, 12.0]);
      }
      translate([39.0,20.0,-1.0]) cube([22.0, 61.0, 7.0]);
    }
    translate([39.0,43.0,-1.0]) cube([22.0, 7.0, 13.0]);
  }
  translate([42.5,19.0,0.0]) 
    union() {
      cylinder(r=2.7, h=30.0);
      cylinder(r=4.7, h=10.0);
      translate([15.0,0.0,0.0]) cylinder(r=2.7, h=30.0);
      translate([15.0,0.0,0.0]) cylinder(r=4.7, h=10.0);
      translate([0.0,36.0,0.0]) cylinder(r=2.7, h=30.0);
      translate([0.0,36.0,0.0]) cylinder(r=4.7, h=10.0);
      translate([15.0,36.0,0.0]) cylinder(r=2.7, h=30.0);
      translate([15.0,36.0,0.0]) cylinder(r=4.7, h=10.0);
    }
}