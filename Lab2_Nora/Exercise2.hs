module Exercise1 where

import Data.List
import LTS
import Test.QuickCheck

ltsGen :: Gen IOLTS
ltsGen = do
    -- Generate states and starting state
    k <- suchThat getSize (< 20)
    n1 <- chooseInteger(1, k)
    q0 <- chooseInteger(1, n1)
    -- Generate input and output labels
    k <- suchThat getSize (< 20)
    n2 <- choose(1, k)
    li <- map (\ x -> ["?"] ++ (show x)) [1..n2]
    lu <- map (\ x -> ["!"] ++ (show x)) [1..n2]
    -- Generate transitions
    t <- [(s, l, t) | s <- shuffle [1..n1], l <- take n1::Int (shuffle (li ++ lu ++ [tau])), t <- shuffle [1..n1]]
    return ([1..n1], li, lu, t, q0)