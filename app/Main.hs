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
  let topL = 1.5 + 0.6
      topR = 83 + 1.2
      botL = 0
      botR = 84 + 1.2
      botZ = 4.4 + 0.33
      topZ = 3.9 + 0.33
      v0 = (botL, 0, 0)
      v1 = (botR, 0, 0)
      v2 = (topR, 61, 0)
      v3 = (topL, 61, 0)
      v4 = (botL, 0, botZ)
      v5 = (botR, 0, botZ)
      v6 = (topR, 61, topZ)
      v7 = (topL, 61, topZ)
   in OS.unsafePolyhedron
        10
        [v0, v1, v2, v3, v4, v5, v6, v7]
        (Faces [[0, 1, 2, 3], [4, 5, 1, 0], [7, 6, 5, 4], [5, 6, 2, 1], [6, 7, 3, 2], [7, 4, 0, 3]])
        & OS.translate (8, 0, 5)
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
    [ OS.cylinder 2.7 30 def,
      boltHeadHole,
      OS.cylinder 2.7 30 def & OS.translate (15, 0, 0),
      boltHeadHole & OS.translate (15, 0, 0),
      OS.cylinder 2.7 30 def & OS.translate (0, 36, 0),
      boltHeadHole & OS.translate (0, 36, 0),
      OS.cylinder 2.7 30 def & OS.translate (15, 36, 0),
      boltHeadHole & OS.translate (15, 36, 0)
    ]
    & OS.translate (42.5, 19, 0)

boltHeadHole :: OS.Model3d
boltHeadHole =
  OS.cylinder 4.7 10 def

main :: IO ()
main = do
  sample & OS.render & writeFile "product.scad"
