module Main where

import Control.Monad (when)
import Language.Haskell.HLint3

main :: IO ()
main = do
    hints <- hlint ["src"]
    let unignored = filter ((> Ignore) . ideaSeverity) hints
    when (length unignored /= 0) $ error "Failed lint"
