module Main where

import Control.Monad (when)
import Language.Haskell.HLint (hlint, Severity(..), suggestionSeverity)

main :: IO ()
main = do
    hints <- hlint ["src"]
    let unignored = filter ((> Ignore) . suggestionSeverity) hints
    when (length hints /= 0) $ error "Failed lint"
