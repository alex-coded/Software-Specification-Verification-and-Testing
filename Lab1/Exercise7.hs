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
  Neg subF -> 1 + nsub subF
  Cnj subFs -> 1 + sum (map nsub subFs) -- Only add 1 for the conjunction itself
  Dsj subFs -> 1 + sum (map nsub subFs) -- Only add 1 for the disjunction itself
  Impl f1 f2 -> 1 + nsub f1 + nsub f2
  Equiv f1 f2 -> 1 + nsub f1 + nsub f2


-- Property 1: All subformulas are indeed subformulas of the input.
prop_allSubformulasAreSubformulas :: Form -> Property
prop_allSubformulasAreSubformulas f =
  forAll (elements $ toList $ sub f) $ \sf ->
    sf `isSubformulaOf` f

-- Property 2: No subformulas are missing.
prop_noMissingSubformulas :: Form -> Property
prop_noMissingSubformulas f =
  let allSubformulas = toList $ sub f
  in forAll (elements allSubformulas) $ \sf ->
    sf `elem` allSubformulas

-- Property 3: nsub computes the exact number of subformulas.
prop_nsubComputesExactCount :: Form -> Property
prop_nsubComputesExactCount f =
  nsub f === length (toList $ sub f)

main :: IO ()
main = do
  quickCheck prop_allSubformulasAreSubformulas
  quickCheck prop_noMissingSubformulas
  quickCheckWith stdArgs { maxSuccess = n } prop_nsubComputesExactCount
