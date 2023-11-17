module Main where

import Data.Function ((&))
import OpenSCAD as OS
import Prelude

-- botLR = 85.2
-- topLR = 82.1
-- botZ = 4.73
-- topZ = 4.23
-- winW = 61
-- levWinW = 22
sample :: OS.Model3d
sample =
  let inner =
        OS.union
          [ adapterInner,
            adapterWindow,
            upperLeverWindow,
            hookReceiver,
            boltHoles
          ]
   in OS.diff
        adapterHull
        [ inner,
          inner & mirror (1, 0, 0)
        ]

adapterHull :: OS.Model3d
adapterHull =
  OS.intersection
    [ OS.minkowski
        [ OS.box 94 61 10,
          OS.cylinder 1 1 def
        ]
    ]
    & OS.translate (-47, 0, 2.5)

adapterInner :: OS.Model3d
adapterInner =
  let topL = 0
      topR = 41
      botL = 0
      botR = 42.6
      botZ = 4.73
      topZ = 4.23
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
        & OS.translate (0, -1, 5)

adapterWindow :: OS.Model3d
adapterWindow =
  OS.box 31 61 12
    & OS.translate (0, -1, -3)

upperLeverWindow :: OS.Model3d
upperLeverWindow =
  OS.box 11 61 7
    & OS.translate (0, 20, 0)

hookReceiver :: OS.Model3d
hookReceiver =
  OS.box 11 7 13
    & OS.translate (0, 43, -1)

-- pitch: 36mm, 15mm 5M bolt
boltHoles :: OS.Model3d
boltHoles =
  OS.union
    [ OS.cylinder 2.8 30 def,
      boltHeadHole,
      OS.cylinder 2.8 30 def & OS.translate (0, 36, 0),
      boltHeadHole & OS.translate (0, 36, 0)
    ]
    & OS.translate (7.5, 19, 0)

boltHeadHole :: OS.Model3d
boltHeadHole =
  OS.cylinder 4.7 10 def

main :: IO ()
main =
  do
    sample
    & OS.render
    & writeFile "product.scad"
