-- Study: MSc Software Engineering.
-- This program is intended to
-- Time spent:

module Exercise1 where

import Data.List
import Test.QuickCheck
import Mutation

-- == Mutators in Mutation.hs ==
-- 1. Adds elements to the beginning and/or end of an output list
-- 2. Removes 1 to (length - 1) elements from an output list
-- 3. Any list (return a random list)
-- == Types of ouput which are not yet covered by these mutators ==


-- == Mutators in this exercise ==
-- (a + b) -> (a - b)
-- (a / b) -> (a * b)
-- (a < b) -> (a > b)
-- Random mutation: any list from mutation.hs
-- higher order mutation: combine two or more simple mutations to create a complex mmutation
-- Parallel execution: execute in parallel processors -- this one we should google
-- contol-flow and priority of test
-- constrained mutation
-- evolutionary algorithms
-- model-based mutations
-- minimal mutation:
-- selective mutation: chose mutations that are most likely to break stuff and test these.
-- Source code mutation: compile file for every mutation (low performance)

