module Lib where

import Data.Biapplicative


-- | http://oeis.org/A000111
--
--  Euler or up/down numbers: e.g.f. sec(x) + tan(x).
--  Also for n >= 2, half the number of alternating permutations on n letters (A001250).
--
a000111 :: Num a => Int -> a
a000111 0 = 1
a000111 n = sum $ a008280_row (n - 1)

-- | http://oeis.org/A008280
a008280_row :: Num a => Int -> [a]
a008280_row = (a008280_tabl !!)

a000111s :: (Enum i, Num a) => [(i, a)]
a000111s = zip [toEnum 0..] . fmap sum $ a008280_tabl

a000111s' :: (Enum i, Num a) => [(i, a)]
a000111s' = zip [toEnum 0..] . fmap sum $ a008281_tabl

-- | http://oeis.org/A008280
a008280_tabl :: Num a => [[a]]
a008280_tabl = ox True a008281_tabl
  where
    ox turn (xs:xss) =
      (if turn
         then reverse xs
         else xs) :
      ox (not turn) xss

-- | http://oeis.org/A008281
a008281_tabl :: Num a => [[a]]
a008281_tabl = iterate (scanl (+) 0 . reverse) [1]


-- | Examples:
--
-- @
--  [1]
--  [1]
--  [0, 1]
--  [1, 0]
--  [0, 1, 1]
--  [1, 1, 0]
--  [0, 1, 2, 2]
--  [2, 2, 1, 0]
--  [0, 2, 4, 5, 5]
--  [5, 5, 4, 2, 0]
--  [0, 5, 10, 14, 16, 16]
--
--  λ> take 20.tail.map(!!1).tail$a008281_tabl
--  [1,1,2,5,16,61,272,1385,7936,50521,353792,2702765,22368256,199360981,1903757312,19391512145,209865342976,2404879675441,29088885112832,370371188237525]
--  *Main Data.Biapplicative Control.Monad
--  λ> take 20.map(!!1).tail$a008281_tabl
--  [1,1,1,2,5,16,61,272,1385,7936,50521,353792,2702765,22368256,199360981,1903757312,19391512145,209865342976,2404879675441,29088885112832]
--
--  λ> take 20.map(!!1).tail$a008281_tabl
--  [1,1,1,2,5,16,61,272,1385,7936,50521,353792,2702765,22368256,199360981,1903757312,19391512145,209865342976,2404879675441,29088885112832]
--
--  λ> take 20.tail.map head.iterate diffs.map(!!1).tail$a008281_tabl
--  [0,0,1,0,5,10,61,280,1665,10470,73621,561660,4650425,41441530,395757181,4031082640,43626778785,499925138190,6046986040741,76992601769220]
--
--  liftM2 (zipWith (-)) id (tail.map head.iterate diffs).map(!!1).tail$a008281_tabl
-- @
--
examples :: ()
examples = ()

-- With folds, I believe we could combine many of these layers into one.
-- For example, compute the sum of the outputs of the scanl as a side-effect to give each sum.

