module Main where

import Data.Function ((&))
import OpenSCAD as OS
import Prelude

sample :: OS.Model3d
sample =
  adapterInner

-- OS.diff
--  adapterHull
--  [ adapterInner,
--    adapterWindow,
--    upperLeverWindow,
--    hookReceiver,
--    boltHoles
--  ]

adapterHull :: OS.Model3d
adapterHull = OS.box 100 65 14

adapterInner :: OS.Model3d
adapterInner =
  let a = (0, 0, 0) -- 0
      b = (82, 0, 0) -- 1
      c = (79, 59, 0) -- 2
      d = (0, 59, 0) -- 3
      e = (0, 0, 4.4) -- 4
      f = (83, 0, 4.4) -- 5
      g = (79, 59, 3.9) -- 6
      h = (0, 59, 3.9) -- 7
   in OS.unsafePolyhedron
        10
        [a, b, c, d, e, f, g, h, a, b, f, e, b, c, g, f, c, d, h, g, d, a, e, h]
        (Faces [[0, 1, 2, 3], [4, 5, 1, 0], [7, 6, 5, 4], [5, 6, 2, 1], [6, 7, 3, 2], [7, 4, 0, 3]])
        & OS.translate (10, 0, 5)
        & OS.translate (0, -1, 0)

adapterWindow :: OS.Model3d
adapterWindow =
  OS.box 60 61 12
    & OS.translate (20, -1, -3)

upperLeverWindow :: OS.Model3d
upperLeverWindow =
  OS.box 20 61 6
    & OS.translate (40, 20, -1)

hookReceiver :: OS.Model3d
hookReceiver =
  OS.box 20 7 13
    & OS.translate (40, 43, -1)

-- pitch: 36mm, 15mm 5M bolt
boltHoles :: OS.Model3d
boltHoles =
  OS.union
    [ OS.cylinder 2.6 30 def,
      boltHeadHole,
      OS.cylinder 2.6 30 def & OS.translate (15, 0, 0),
      boltHeadHole & OS.translate (15, 0, 0),
      OS.cylinder 2.6 30 def & OS.translate (0, 36, 0),
      boltHeadHole & OS.translate (0, 36, 0),
      OS.cylinder 2.6 30 def & OS.translate (15, 36, 0),
      boltHeadHole & OS.translate (15, 36, 0)
    ]
    & OS.translate (42.5, 4, 0)

boltHeadHole :: OS.Model3d
boltHeadHole =
  OS.cylinder 4.5 10 def

main :: IO ()
main = do
  sample & OS.render & writeFile "product.scad"
