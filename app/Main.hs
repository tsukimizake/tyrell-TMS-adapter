module Main where

import Data.Function ((&))
import OpenSCAD as OS
import Prelude

sample :: OS.Model3d
sample =
  -- adapterInner

  OS.diff
    adapterHull
    [ adapterInner,
      adapterWindow,
      upperLeverWindow,
      hookReceiver,
      boltHoles
    ]

adapterHull :: OS.Model3d
adapterHull = OS.box 100 65 14

adapterInner :: OS.Model3d
adapterInner =
  let v0 = (0, 0, 0)
      v1 = (82, 0, 0)
      v2 = (79.5, 61, 0)
      v3 = (2, 61, 0)
      v4 = (0, 0, 4.4)
      v5 = (82, 0, 4.4)
      v6 = (79.5, 61, 3.9)
      v7 = (2, 61, 3.9)
   in OS.unsafePolyhedron
        10
        [v0, v1, v2, v3, v4, v5, v6, v7]
        (Faces [[0, 1, 2, 3], [4, 5, 1, 0], [7, 6, 5, 4], [5, 6, 2, 1], [6, 7, 3, 2], [7, 4, 0, 3]])
        & OS.translate (10, 0, 5)
        & OS.translate (0, -1, 0)

adapterWindow :: OS.Model3d
adapterWindow =
  OS.box 61 61 12
    & OS.translate (20.5, -1, -3)

upperLeverWindow :: OS.Model3d
upperLeverWindow =
  OS.box 22 61 7
    & OS.translate (39, 20, -1)

hookReceiver :: OS.Model3d
hookReceiver =
  OS.box 22 7 13
    & OS.translate (39, 43, -1)

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
    & OS.translate (42.5, 19, 0)

boltHeadHole :: OS.Model3d
boltHeadHole =
  OS.cylinder 4.7 10 def

main :: IO ()
main = do
  sample & OS.render & writeFile "product.scad"
