module RandomExtras where

import System.Random

instance (Random x, Random y) => Random (x, y) where
  random gen1 =
    let
      (x, gen2) = random gen1
      (y, gen3) = random gen2
    in ((x, y), gen3)
  randomR ((x1, y1), (x2, y2)) gen1 =
    let
      (x, gen2) = randomR (x1, x2) gen1
      (y, gen3) = randomR (y1, y2) gen2
    in ((x, y), gen3)

