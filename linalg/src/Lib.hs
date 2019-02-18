module Lib
    ( someFunc
    , module Frac
    ) where

import Frac

someFunc :: IO ()
someFunc = putStrLn "someFunc"


