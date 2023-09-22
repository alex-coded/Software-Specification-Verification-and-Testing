<<<<<<< Updated upstream
=======
-- 3 hours

module Exercise7 where

>>>>>>> Stashed changes
import Test.QuickCheck
import SetOrd
import Lecture2

type Name = Int

n :: Int
n = 1

toList :: Set a -> [a]
toList (Set xs) = xs

data Form = Prop Name
          | Neg Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form
          | Equiv Form Form
          deriving (Eq, Ord)

instance Show Form where
    show (Prop x) = "Prop " ++ show x
    show (Neg f) = "Neg (" ++ show f ++ ")"
    show (Cnj fs) = "Cnj " ++ show fs
    show (Dsj fs) = "Dsj " ++ show fs
    show (Impl f1 f2) = "Impl (" ++ show f1 ++ ") (" ++ show f2 ++ ")"
    show (Equiv f1 f2) = "Equiv (" ++ show f1 ++ ") (" ++ show f2 ++ ")"

-- Define a custom Arbitrary instance for Form
instance Arbitrary Form where
    arbitrary = sized genForm
        where
            genForm 0 = do
                n <- arbitrary
                return (Prop n)
            genForm n = oneof [
                do
                    n <- arbitrary
                    return (Prop n),
                Neg <$> subForm,
                Cnj <$> vectorOf 2 subForm,
                Dsj <$> vectorOf 2 subForm,
                Impl <$> subForm <*> subForm,
                Equiv <$> subForm <*> subForm
                ]
                where subForm = genForm (n `div` 2)

-- Helper function to check if a formula is a subformula of another formula.
isSubformulaOf :: Form -> Form -> Bool
isSubformulaOf sf f = sf `elem` toList (sub f)

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

nsub :: Form -> Int
nsub f = case f of
  Prop _ -> 1
<<<<<<< Updated upstream
  Neg subF -> 1 + nsub subF
  Cnj subFs -> 1 + sum (map nsub subFs) -- Only add 1 for the conjunction itself
  Dsj subFs -> 1 + sum (map nsub subFs) -- Only add 1 for the disjunction itself
=======
  Neg subFormula -> 1 + nsub subFormula
  Cnj subFormulas -> 1 + sum (map nsub subFormulas) -- Only add 1 for the conjunction itself
  Dsj subFormulas -> 1 + sum (map nsub subFormulas) -- Only add 1 for the disjunction itself
>>>>>>> Stashed changes
  Impl f1 f2 -> 1 + nsub f1 + nsub f2
  Equiv f1 f2 -> 1 + nsub f1 + nsub f2


-- Property 1: All subformulas are indeed subformulas of the input.
<<<<<<< Updated upstream
prop_allSubformulasAreSubformulas :: Form -> Property
prop_allSubformulasAreSubformulas f =
=======
prop_checkThatAllSubAreSub :: Form -> Property
prop_checkThatAllSubAreSub f = 
>>>>>>> Stashed changes
  forAll (elements $ toList $ sub f) $ \sf ->
    sf `isSubformulaOf` f

-- Property 2: No subformulas are missing.
<<<<<<< Updated upstream
prop_noMissingSubformulas :: Form -> Property
prop_noMissingSubformulas f =
=======
prop_thereAreNoMissingSubf :: Form -> Property
prop_thereAreNoMissingSubf f =
>>>>>>> Stashed changes
  let allSubformulas = toList $ sub f
  in forAll (elements allSubformulas) $ \sf ->
    sf `elem` allSubformulas

-- Property 3: nsub computes the exact number of subformulas.
<<<<<<< Updated upstream
prop_nsubComputesExactCount :: Form -> Property
prop_nsubComputesExactCount f =
=======
prop_nsubGivesExactCount :: Form -> Property
prop_nsubGivesExactCount f =
>>>>>>> Stashed changes
  nsub f === length (toList $ sub f)

main :: IO ()
main = do
<<<<<<< Updated upstream
  quickCheck prop_allSubformulasAreSubformulas
  quickCheck prop_noMissingSubformulas
  quickCheckWith stdArgs { maxSuccess = n } prop_nsubComputesExactCount
=======
  quickCheck prop_checkThatAllSubAreSub
  quickCheck prop_thereAreNoMissingSubf
  quickCheckWith stdArgs { maxSuccess = n } prop_nsubGivesExactCount


--------- LIBRARIES ---------

-- We didn't use any additional libraries in order to write this code.


--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

-- First, we are given the data type Form which actually represents logical formulae,
-- I used the arbitrary instance for form in order to be able to generate test cases for 
-- QuickCheck. The arbitrary fun gives random formulae (propositions, disjunctions, implications, negations) recursively.
-- With the helper function isSubformulaOf I basically check if subformula sf is included in the formula f, whereas sub calculates 
-- the set of subformulas given a formula f. 


-- I implemented the last property with the function nsub - that calculates recursively the concrete no. of subformulae of a
-- certain formula. It counts everything using pattern matching. It also increments the count for each sf it finds.


--------- TESTING APPROACH ---------
  
-- I used 3 QuickCheck properties to test this code. The first property wants to verify if a subformula
-- is indeed, a valid subformula of the original formula f. It checks the property for many randomly generated 
-- formulae with the help of forAll. We just use the isSubformulaOf function.

-- The second property asserts that no subformulaes are missing from the initial formula. 

-- In short, in the first part of the exercise, we used the above two properties to prove the correctness of the code.

-- With nsub we had two base cases (Prop _) and (Neg f) to check if the formula is a simple proposition (lead node), or if it is 
-- a negation of another formula. Later, we just effectively traverse the entire formula tree, and add to the counter of each formula seen.
>>>>>>> Stashed changes
