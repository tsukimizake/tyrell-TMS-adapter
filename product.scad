difference() {
  difference() {
    difference() {
      difference() {
        difference() {
          cube([100.0, 65.0, 14.0]);
          translate([0.0,-1.0,0.0]) 
            translate([10.0,0.0,5.0]) cube([80.0, 61.0, 4.0]);
        }
        translate([20.0,-1.0,-3.0]) cube([60.0, 61.0, 12.0]);
      }
      translate([40.0,20.0,-1.0]) cube([20.0, 61.0, 6.0]);
    }
    translate([40.0,43.0,-1.0]) cube([20.0, 7.0, 13.0]);
  }
  translate([42.5,4.0,0.0]) 
    union() {
      cylinder(r=2.6, h=30.0);
      translate([15.0,0.0,0.0]) cylinder(r=2.6, h=30.0);
      translate([0.0,36.0,0.0]) cylinder(r=2.6, h=30.0);
      translate([15.0,36.0,0.0]) cylinder(r=2.6, h=30.0);
    }
}