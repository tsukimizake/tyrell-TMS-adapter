union() {
  difference() {
    difference() {
      difference() {
        difference() {
          difference() {
            translate([0.0,0.0,2.5]) 
              intersection() {
                minkowski() {
                  cube([47.0, 61.0, 10.0]);
                  cylinder(r=1.0, h=1.0);
                }
                translate([0.0,-10.0,0.0]) cube([200.0, 200.0, 200.0]);
              }
            translate([0.0,-1.0,5.0]) 
              polyhedron( points=[ [0.0,0.0,0.0]
              , [42.6,0.0,0.0]
              , [41.0,61.0,0.0]
              , [0.0,61.0,0.0]
              , [0.0,0.0,4.73]
              , [42.6,0.0,4.73]
              , [41.0,61.0,4.23]
              , [0.0,61.0,4.23] ]
              , faces=[ [0, 1, 2, 3]
              , [4, 5, 1, 0]
              , [7, 6, 5, 4]
              , [5, 6, 2, 1]
              , [6, 7, 3, 2]
              , [7, 4, 0, 3] ]
              , convexity=10 );
          }
          translate([0.0,-1.0,-3.0]) cube([31.0, 61.0, 12.0]);
        }
        translate([0.0,20.0,0.0]) cube([11.0, 61.0, 7.0]);
      }
      translate([0.0,43.0,-1.0]) cube([11.0, 7.0, 13.0]);
    }
    translate([7.5,19.0,0.0]) 
      union() {
        cylinder(r=2.8, h=30.0);
        cylinder(r=4.7, h=10.0);
        translate([0.0,36.0,0.0]) cylinder(r=2.8, h=30.0);
        translate([0.0,36.0,0.0]) cylinder(r=4.7, h=10.0);
      }
  }
  mirror([1.0,0.0,0.0]) 
    difference() {
      difference() {
        difference() {
          difference() {
            difference() {
              translate([0.0,0.0,2.5]) 
                intersection() {
                  minkowski() {
                    cube([47.0, 61.0, 10.0]);
                    cylinder(r=1.0, h=1.0);
                  }
                  translate([0.0,-10.0,0.0]) cube([200.0, 200.0, 200.0]);
                }
              translate([0.0,-1.0,5.0]) 
                polyhedron( points=[ [0.0,0.0,0.0]
                , [42.6,0.0,0.0]
                , [41.0,61.0,0.0]
                , [0.0,61.0,0.0]
                , [0.0,0.0,4.73]
                , [42.6,0.0,4.73]
                , [41.0,61.0,4.23]
                , [0.0,61.0,4.23] ]
                , faces=[ [0, 1, 2, 3]
                , [4, 5, 1, 0]
                , [7, 6, 5, 4]
                , [5, 6, 2, 1]
                , [6, 7, 3, 2]
                , [7, 4, 0, 3] ]
                , convexity=10 );
            }
            translate([0.0,-1.0,-3.0]) cube([31.0, 61.0, 12.0]);
          }
          translate([0.0,20.0,0.0]) cube([11.0, 61.0, 7.0]);
        }
        translate([0.0,43.0,-1.0]) cube([11.0, 7.0, 13.0]);
      }
      translate([7.5,19.0,0.0]) 
        union() {
          cylinder(r=2.8, h=30.0);
          cylinder(r=4.7, h=10.0);
          translate([0.0,36.0,0.0]) cylinder(r=2.8, h=30.0);
          translate([0.0,36.0,0.0]) cylinder(r=4.7, h=10.0);
        }
    }
}