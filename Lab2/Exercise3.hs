-- Study: MSc Software Engineering.
-- This program is intended to implement the function straces that returns all suspension traces of a given IOLTS.
-- Time spent: 180 min
-- Additional dependencies:

module Exercise3 where

-- import Exercise2
import Data.List
import LTS

import Test.QuickCheck

-- This function returns True if a state in a list of state transitions is quiescent and False otherwise.
-- This function checks the State has an ouput label. If a state has an output label, this state is not a
-- quiescent and otherwise, this state is a quiescent.
isQuiescent :: [(State, Label)] -> [Label] -> Bool
isQuiescent nt lu = not (any (\lable -> lable `elem` map snd nt) lu)

-- This function finds suspension transitions, adds these transitions in the list of transitions stored in
-- IOLTS and returns the resulting transition list.
-- This function checks every state stored in the IOLTS whether the state is a quiescent.
-- For every state, all related labels of this state is retrieved with the function 'nextTransitions' defined
-- in LTS.hs. Based on these label we determine whether a state is a quiescent using the isQuiescent function.
-- if a state is a quiescent, a transition with delta label will be added to the list of
-- transitions with the quiescent being the input and the output state.
straces' :: IOLTS -> [LabeledTransition]
straces' ([], _, _, t, _) = nub t
straces' (qh:qt, li, lu, t, q0) | isQuiescent (nextTransitions' t qh) lu = straces' (qt, li, lu, t ++ [(qh, delta, qh)], q0)
                                  | otherwise = straces' (qt, li, lu, t, q0)

-- This function returns all suspension traces of a given IOLTS.
-- BY using the traces' function defined in LTS.hs, we retieve all traces of the given IOLTS
-- based on the updated transitions including suspension transitions.
straces :: IOLTS -> [Trace]
straces (q, li, lu, t, q0) = nub $ map snd (traces' (t ++ straces' (q, li, lu ++ [delta], t, q0)) [([q0],[])])


-- This generator randomly generates transitions of the form (State, Label, State).
transitionGen :: Gen LabeledTransition
transitionGen = do
    k <- chooseInteger (2, 20)
    s <- chooseInteger (1, k)
    t <- suchThat (chooseInteger (1, k)) (/= s)
    l <- elements ([tau] ++ (map (\ x -> if mod x 2 == 0 then "?" ++ (show x) else "!" ++ (show x)) [1..k]))
    return (s, l, t)

-- This generator randomly generates valid IOLTS using the createIOLTS function with randomly generated transitions.
ltsGen :: Gen IOLTS
ltsGen = do
    k <- choose (2, 20)
    n <- choose (2, k)
    transitions <- vectorOf n transitionGen
    return (createIOLTS transitions)


tracesGen :: Gen Trace
tracesGen = do
    head . straces <$> ltsGen



main :: IO ()
main = do
    putStrLn "Testing with QuickCheck"
    quickCheck prop_straces_are_susp
    -- quickCheckWith stdArgs { maxSuccess = 20 } prop_no_dup_traces


-- Define a property to check that all generated traces are suspension traces
prop_straces_are_susp :: Property
prop_straces_are_susp = forAll ltsGen $ \tempIOLTS ->
  let susTraces = straces tempIOLTS
  in forAll tracesGen $ \generatedTrace ->
    generatedTrace `elem` susTraces


prop_no_dup_traces :: Property
prop_no_dup_traces = forAll tracesGen $ \generatedTrace ->
  forAll ltsGen $ \sampleIOLTS ->
    let susTraces = straces sampleIOLTS
    in length (filter (== generatedTrace) susTraces) <= 1


{-

[]
["tau"]
["delta"]
["tau","tau"]
["tau","delta"]
["delta","tau"]
["delta","delta"]
["tau","tau","delta"]
["tau","delta","tau"]
["tau","delta","delta"]
["delta","tau","tau"]
["delta","tau","delta"]
["delta","delta","tau"]
["delta","delta","delta"]
["tau","tau","delta","delta"]
["tau","delta","tau","delta"]
["tau","delta","delta","tau"]
["tau","delta","delta","delta"]
["delta","tau","tau","delta"]
["delta","tau","delta","tau"]



Suspension Traces:
[]
["5"]
["16"]
["6"]
["5","2"]
["5","tau"]
["5","delta"]
["16","11"]
["6","delta"]
["5","2","5"]
["5","2","16"]
["5","2","6"]
["5","tau","delta"]
["5","delta","2"]
["5","delta","tau"]
["5","delta","delta"]
["16","11","delta"]
["6","delta","delta"]
["5","2","5","2"]
["5","2","5","tau"]


Suspension Traces:
[]
["1"]
["6"]
["1","3"]
["1","2"]
["6","delta"]
["1","3","delta"]
["1","2","1"]
["1","2","6"]
["6","delta","delta"]
["1","3","delta","delta"]
["1","2","1","3"]
["1","2","1","2"]
["1","2","6","delta"]
["6","delta","delta","delta"]
["1","3","delta","delta","delta"]
["1","2","1","3","delta"]
["1","2","1","2","1"]
["1","2","1","2","6"]
["1","2","6","delta","delta"]


Suspension Traces:
[]
["1"]
["tau"]
["1","6"]
["1","8"]
["1","delta"]
["tau","8"]
["tau","tau"]
["tau","delta"]
["tau","5"]
["1","6","1"]
["1","6","tau"]
["1","8","8"]
["1","8","tau"]
["1","8","delta"]
["1","delta","6"]
["1","delta","8"]
["1","delta","delta"]
["tau","8","delta"]
["tau","tau","1"]


Suspension Traces:
[]
["5"]
["2"]
["5","14"]
["5","delta"]
["2","6"]
["2","tau"]
["2","delta"]
["2","14"]
["5","14","5"]
["5","14","2"]
["5","delta","14"]
["5","delta","delta"]
["2","6","delta"]
["2","tau","5"]
["2","tau","2"]
["2","delta","6"]
["2","delta","tau"]
["2","delta","delta"]
["2","14","5"]

The above are some examples of suspension traces.
They are correct because they provide a list of labels as expected, and every label corresponds with a Label type.
 ===> making the traces consistent relative to the type. 
Also, the labels are valid ("tau", "delta")



-- Trace Generator --

In the above, we have implemented the tracesGen function - which basically generates 
random traces for our IOLTS. 

The most important thing to note about the traceGen is that generates random IOLTS with 
an arbitrary number of transitions and states. Afterwards, the suspension traces are calculated,
later to take the first strace out of the staces and return that as a Gen trace.

This was needed for our quickCheck tests. 

We verify that our staces function is ok with the prop_straces_are_susp prop. 
With this property we check that all the generated traces are indeed suspension traces for
th IOLTS with our  `straces` func. 


forAll ltsGen $ \sampleIOLTS -> here we generate a random IOLTS with the ltsGen generator. and store it in sampleIOLTS
let susTraces = traces sampleIOLTS in .. in this part we calculate the suspension traces for the aforementioned samleIOLTS
Afterwards, with the forAll tracesGen .. we generate random traces with our created generator tracesGen.
the last line implement the main idea of the property - checking if the generatedTrace is an element of the suspension traces.
    We check that the trace generated by our property is a strace given our implemented function   `traces`


The second property checks that the traces function gives unique traces in case we idolize the tracesGen generator. 

All in all, all our test cases ran and we got the result +++ OK, passed 100 tests.

Other ideas for testing the straces function are:
* traces being subsequences of suspended traces
* We can also check that the length of gen traces is kept in the traces
* We can also check that we have unique labels in traces
-}