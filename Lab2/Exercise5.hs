-- Study: MSc Software Engineering.
-- This program is intended to model a smart door, and to test 8 different smart door implementations
-- based on the model.
-- Time spent: 10 hours

module Exercise5 where

import Data.List
import LTS

-- This function returns a model of a smart door in the form of an IOLTS, based on the behaviour
-- specified in the comment report below.
doorModel :: IOLTS
doorModel = createIOLTS [(0, "?close", 1), (1, "!closed", 2), (2, "?open", 3), (3, "!opened", 0), (2, "?lock", 4), (4, "!locked", 5), (5, "?unlock", 6), (6, "!unlocked", 2)]

-- This function takes an IOLTS model and an implementation, and tests the implementation against the
-- model by creating a test suite of 100 test cases and running the cases.
testLTSAgainstSUT :: IOLTS -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT (q, li, lu, t, q0) impl = do
    let traceList = take 100 (traces (createLTS t))
    let testSuite = map (\ x -> filter (\ y -> not (elem y lu)) x) traceList
    all (\ testCase -> testLoop [q0] 0 testCase t impl) testSuite

-- This function goes through a test case and checks whether the implementation responds correctly to
-- the input labels. If not, an error is thrown specifying the expected response and the given response.
testLoop :: [State] -> State -> [Label]-> [LabeledTransition] -> (State -> Label -> (State, Label)) -> Bool
testLoop _ _ [] _ _ = True
testLoop q s (l:ls) t impl = do
    let (state, lab) = impl s l                                                             -- The next state + output based on the input state + label.
    let next = concatMap (\ x -> nextTransitions' t x) q                                    -- Next possible states + labels based on start state.
    let possibleNext = map (\ v -> fst v) (filter (\ (x, y) -> y == l) next)                -- Next possible states that also correspond with the input label.
    let final = concatMap (\ x -> nextTransitions' t x) possibleNext                        -- One step further states + labels based on next states.
    let possibleFinalStates = map (\ v -> fst v) (filter (\ (x, y) -> y == lab) final)      -- One step further states that also correspond with the output label.
    let resp = map (\ v -> snd v) final                                                     -- The possible output labels based on start state and input label
    if possibleFinalStates /= []                                                            -- If there is states to go to according to the model, there is no issue
    then True && (testLoop possibleFinalStates state ls t impl)                             -- Move to the next input label in the trace
    else error ("Expected response: " ++ (intercalate ", " resp) ++ ". Given response: " ++ lab ++ ".\n")

-- Use this line to test each implementation one by one.
main :: IO ()
main = do
    print (testLTSAgainstSUT doorModel doorImpl1)


{-
--------- LIBRARIES ---------

In this program, the following libraries are used:

- Data.List: To be able to use list specific functions like intercalate
- LTS: To be able to use the IOLTS and related types and functions.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

This program is intended to create a model that represents a door. This model is used to test 8 different
implementations of the door. The model is based on the following behaviors:

1. When the door is closed, it may be opened.
2. When the door is open, it may be closed.
3. When the door is closed and unlocked, it may be locked.
4. When the door is closed and locked, it may be unlocked.

The model is defined in the doorModel function. The test function returns true when this model is
tested against the first implementation, which is known to be correct.

--------- TESTING APPROACH ---------

In order to test the door implementations against the model, the testLTSAgainstSUT function was created.
The first step of testing is the creation of a test suite. This is done by taking 100 possible traces of
the model, and filtering out the output labels, to be left with only input labels. All of these test cases
are tested and the results are conbined. The test suite is only passed if all test cases pass.

The test execution goes as follows:

1. We start with the starting state of all implementations 0, and with the first input label of the test case.
2. We get the implementation output given the state and the input label.
3. We get all possible ending states from the model, given the state and the input label.
4. We get all possible output labels and corresponding ending states from the model, given the states
   gathered in step 3.
5. We filter out the ending states whose output label is not equal to the output label from the
   implementation (step 2). This leaves us with states that can possibly be reached given the input label,
   and the output label given by the implementation.
6. If there are no reachable states, that means that the output given by the implementation, is not one
   of the possible outputs given by the model. The implementation is wrong and an error is thrown.
7. If there are reachable states, that means that the output given by the implementation is in line with
   at least one of the possible outputs given by the model. The implementation is correct so far and we move
   on to the next input label in the test case.

We tested the 8 implementations separately, by replacing the implementation number in the main function
every time and running it again. This was done because every time an error is thrown, the program quits.
So when all tests are run sequentially in one function, and the second test fails, the other 6 will not be
tested. The following test report specifies the implementation tested and the output of the test. We also
explain the bugs that were found in the implementations.

Implementation: doorImpl1 (known to be correct)
Function line: print (testLTSAgainstSUT doorModel doorImpl1)
Test result: True
Bugs: None

Implementation: doorImpl2
Function line: print (testLTSAgainstSUT doorModel doorImpl2)
Test result: Expected response: closed. Given response: opened.
Bugs: When trying to close an open door, the closed response should be given, yet
the opened response is given.

Implementation: doorImpl3
Function line: print (testLTSAgainstSUT doorModel doorImpl3)
Test result: Invalid command and/or state!
Bugs: Difficult to determine from just this error message, but after looking at the
implementation, it seems that when trying to unlock a locked door, the correct response
is sent, but there is no transition to the correct unlocked state.

Implementation: doorImpl4
Function line: print (testLTSAgainstSUT doorModel doorImpl4)
Test result: Invalid command and/or state!
Bugs: Difficult to determine from just this error message, but after looking at the
implementation, it seems that when trying to lock an unlocked door, the unlocked response
is sent instead of the locked response. And when trying to unlock a locked door, the locked
response is sent instead of the unlocked response.

Implementation: doorImpl5
Function line: print (testLTSAgainstSUT doorModel doorImpl5)
Test result: Expected response: opened. Given response: open.
Bugs: When trying to open a closed door, the input label 'open' is used as an output label. The
correct response should be the 'opened' output label.

Implementation: doorImpl6
Function line: print (testLTSAgainstSUT doorModel doorImpl6)
Test result: Door is stuck!
Bugs: Some specific input label sequence leads to the door being stuck. It is difficult to
determmine just from this message what leads to the door being stuck, but after looking at the
implementation, it seems that opening and closing the door three times sequentially leads to a
stuck door.

Implementation: doorImpl7
Function line: print (testLTSAgainstSUT doorModel doorImpl7)
Test result: Incorrect keycode!
Bugs: Some specific input label sequence leads to an incorrect keycode error. It is difficult to
determine that leads to an incorrect keycode. After looking at the implementation, it seems that,
when trying to unlock a locked door after it has been locked, unlocked, and opened 3 times, an
incorrect keycode error is thrown.

Implementation: doorImpl8
Function line: print (testLTSAgainstSUT doorModel doorImpl8)
Test result: Invalid command and/or state!
Bugs: Difficult to determine from just this error message, but after looking at the
implementation, it seems that not all states that represent a closed door, have to option
to move to a locked state with a lock input and back with an unlock input. So when the model
is in such a closed state and tries to lock the door, the implementation does not have the option.

-}
