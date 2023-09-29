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
straces (q, li, lu, t, q0) = map snd (traces' (straces' (q, li, lu, t, q0)) [([q0],[])])


-- TESTING PART --

isInputLabel :: Label -> Bool
isInputLabel label = not (null label) && head label == '?'

isOutputLabel :: Label -> Bool
isOutputLabel label = not (null label) && head label == '!'


prop_no_quiescent_states :: IOLTS -> Property
prop_no_quiescent_states (q, li, lu, t, q0) = not (null (straces (q, li, lu, t, q0))) ==> not (any (hasQuiescent lu) (straces (q, li, lu, t, q0)))
  where
    hasQuiescent :: [Label] -> Trace -> Bool
    hasQuiescent lu = any (\label -> isQuiescent [(0, label)] lu)


-- prop_start_from_initial_state :: IOLTS -> Property
-- prop_start_from_initial_state (q, li, lu, t, q0) = not (null (straces (q, li, lu, t, q0))) ==> all (\(state, _) -> state == q0) (head (straces (q, li, lu, t, q0)))

-- prop_suspended_trace_structure :: IOLTS -> Property
-- prop_suspended_trace_structure (q, li, lu, t, q0) = not (null (straces (q, li, lu, t, q0))) ==> all isTransitionList (straces (q, li, lu, t, q0))
--   where
--     isTransitionList :: Trace -> Bool
--     isTransitionList trace = all isTransitionLabel trace

--     isTransitionLabel :: Label -> Bool
--     isTransitionLabel label = label /= tau && (isInputLabel label || isOutputLabel label)


-- This generator randomly generates transitions of the form (State, Label, State).
transitionGen :: Gen LabeledTransition
transitionGen = do
    k <- chooseInteger(2, 20)
    s <- chooseInteger(1, k)
    t <- suchThat (chooseInteger(1, k)) (/= s)
    l <- elements ([tau] ++ (map (\ x -> if mod x 2 == 0 then "?" ++ (show x) else "!" ++ (show x)) [1..k]))
    return (s, l, t)

-- This generator randomly generates valid IOLTS using the createIOLTS function with randomly generated transitions.
ltsGen :: Gen IOLTS
ltsGen = do
    k <- choose(2, 20)
    n <- choose(2, k)
    transitions <- vectorOf n transitionGen
    return (createIOLTS transitions)



main :: IO ()
main = do
    putStrLn "Property 1: Suspended traces must not contain quiescent states."
    quickCheck $ forAll ltsGen $ prop_no_quiescent_states