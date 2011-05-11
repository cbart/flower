module Util.Arrow (twice) where


import Control.Arrow


twice :: Arrow a => a b c -> a (b, b) (c, c)
twice f = f *** f
