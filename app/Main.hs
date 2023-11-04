module Main where

import Data.Function ((&))
import OpenSCAD as OS
import Prelude

sample :: OS.Model3d
sample =
  adapterHull
    `difference` adapterInner
    `difference` adapterWindow
    `difference` upperLeverWindow
    `difference` hookReceiver
    `difference` boltHoles

adapterHull :: OS.Model3d
adapterHull = OS.box 100 65 14

adapterInner :: OS.Model3d
adapterInner =
  OS.box 80 61 4
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
      OS.cylinder 2.6 30 def & OS.translate (15, 0, 0),
      OS.cylinder 2.6 30 def & OS.translate (0, 36, 0),
      OS.cylinder 2.6 30 def & OS.translate (15, 36, 0)
    ]
    & OS.translate (42.5, 4, 0)

boltHeadHole :: OS.Model3d
boltHeadHole =
  OS.cylinder 8 10 def
    & OS.translate (0, 0, -1)

main :: IO ()
main = do
  sample & OS.render & writeFile "product.scad"
