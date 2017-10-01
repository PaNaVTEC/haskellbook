module Chapter7 where

mth x y z = x * y * z

mth' x y = \z -> x * y * z


some = mth' 1 1 1 == mth 1 1 1
