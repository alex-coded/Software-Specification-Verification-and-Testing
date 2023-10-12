-- Study: MSc Software Engineering.
-- This program is intended to count  he number of surviving mutants from a total of n mutants.
-- Time spent: 6 hours

module Exercise2 where
import Test.QuickCheck
import MultiplicationTable
import Mutation
import Exercise1

-- This function counts the number of surviving mutants by using vectorOf to repeat the mutate function n times.
countSurvivors :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> IO Int
countSurvivors n prop fun mutator = do
            results <- generate (vectorOf n (mutate' mutator prop fun 3))
            passed <- return (filter (\ x -> (all (\y -> y == True) x) && (x /= [])) results)
            return (length passed)

-- This function counts the number of surviving mutants by using recursion to repeat the mutate function n times.
countSurvivors' :: Int -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> IO Int
countSurvivors' n prop fun mutator = do
            results <- generate (mutate' mutator prop fun 3)
            passed <- if results /= [] then return (all (\ x -> x == True) results) else return (False)
            if n /= 0
            then do
                res <- countSurvivors (n - 1) prop fun mutator
                if passed == True
                then return (1+res)
                else return res
            else
                if passed == True
                then return 1
                else return 0

-- count the survivors for every combination of individual property and mutator.
main :: IO [Int]
main = do
    sequence [ countSurvivors 4000 [x] multiplicationTable y | x <- multiplicationTableProps, y <- ([addElements, removeElements] ++ customMutators)]


{-
--------- LIBRARIES ---------

In this program, the following libraries are used:
- Test.QuickCheck: To be able to make use of mutators, which are of type Gen.
- MultiplicationTable: The function and properties described here are used to test our countSurvivors
  function.
- Mutation: The mutate' function is used in the countSurvivors function, and some standard mutators are
  used for testing.
- Exercise1: In order to access the custom mutators created in Exercise1.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

This program is intended to count survivors given a number of mutants, a set of properties, a function
under testing (fut), and a generator. We will also look at the effect of using different mutators and properties
on the number of survivors. To count the number of survivors, the folllowing steps are followed:

1. Get the output from the fut and an arbitrary input (output = fut input).
2. Mutate the output using the mutator (mutant = mutator output).
3. Get the results of the set of properties applied to the mutant and the input
   (results = [ property mutant input | property <- set of properties ])
4. If the results consist only of True values, the mutant counts as a survivor
   (if all (== True) results then survivorcount += 1)
5. Repeat steps 1 through 4 n times (n = amount of mutants).

The first three steps in this algorithm can be replaced by the mutate' function from the Mutation
module. I ended up implementing the countSurvivors function in two different ways. The first is by
repeating the mutate' function through recursion (countSurvivors'). The second is by immediately
executing the mutate' function n times through vectorOf (countSurvivors). This second version is
neater and has less code. However, I kept the recursive version as well because it is a more direct
translation of the algorithm and it helped me regain understanding of how the algorithm worked when I
got lost in it.

Another thing that we have to take into account is that the mutate' function can return an empty list.
This happens when the mutator had no effect on the output. If the properties accurately describe the fut,
all tests will automatically pass in this case. This would normally be classified as a survivor, but in
this case, it is expected behaviour for the 'mutant'. So in these cases, passing all tests does not imply
a mistake, or incompleteness in the tests. This is why I have classified such cases as killed instead of
survived. This is why any empty list results are filtered out in my implementations.

--------- TESTING APPROACH ---------

In order to see the effect of using different properties and mutators on the number of survivors, the
five properties of the multiplicationTable function, and the 2 standard + 5 custom mutators were used.
The anyList mutator is not used because it can generate empty lists which results in an exception.
Per property, I will describe what the expected number of survivors are for all different mutators, and
I will report the actual results from the countSurvivors function, for 4000 mutants.

The prop_tenElements property states that the output list of the multiplicationTable function should always
consist of 10 elements. Therefore, we expect to have 4000 survivors when using mutators that do not affect
the length of the output list. These are: shuffleList, incrementList, and decrementList. Since the sameElements
mutator generates lists of random length between 1 and 20 over a uniform distribution, we expect around
4000 / 20 = 200 survivors. For all other mutators, we expect 0 survivors, since they will always change the
length of the output list. The output of the countSurvivor function confirms our hypothesis:

- PROPERTY NAME, MUTATOR NAME                                       SURVIVOR COUNT
- prop_tenElements, addElements:                                    0
- prop_tenElements, removeElements:                                 0
- prop_tenElements, shuffleList:                                    4000
- prop_tenElements, subList:                                        0
- prop_tenElements, incrementElements:                              4000
- prop_tenElements, decrementElements:                              4000
- prop_tenElements, sameElements:                                   191

The prop_firstElementIsInput states that the first element of the output list of the multiplicationTable
function should always be equal to the function input. Therefore, we expect 0 survivors when using mutators
that always change all list elements. These are incrementList and decrementList. Since we used the input of 3
(between 1 and 10), and the sameElements mutator chooses an integer between 1 and 10 over a uniform distribution,
we expect around 4000 / 10 = 400 survivors. For shuffleList, 1/10th of all possible permutations keep the first
element at its original place. So here too, we expect around 4000 / 10 = 400 survivors. The removeElements mutator
always takes at least 1 element from the front of the list, so the first element of the output list would remain
unchanged. This would result in 4000 survivors, but removeElements has the chance of not changing the output at all,
which is seen as a killed mutant, so the number of survivors will likely be a bit lower. For the subList mutator, 1/2
of the possible subsequences keep the first element the same, so we expect around 4000 / 2 = 2000 survivors. This
mutator can also return an unchanged output list, but since the empty list is also not a possible output, these two
cancel eachother out, keeping the 1/2 chance of an unchanged first element. I am not sure if there is a way to calculate
the chance of an unchanged first element for addElements, but in any case, we expect survivors between 0 and 4000.
The countSurvivors output confirms our hypothesis:

- PROPERTY NAME, MUTATOR NAME                                       SURVIVOR COUNT
- prop_firstElementIsInput, addElements:                            180
- prop_firstElementIsInput, removeElements:                         3647
- prop_firstElementIsInput, shuffleList:                            390
- prop_firstElementIsInput, subList:                                1991
- prop_firstElementIsInput, incrementElements:                      0
- prop_firstElementIsInput, decrementElements:                      0
- prop_firstElementIsInput, sameElements:                           416

The prop_sumIsTriangleNumberTimesInput states that the sum of the output list of the multiplicationTable
function should be equal to the input times the 10th triangle number. Here, there can only be survivors
when the elements of the output list remain unchanged. So for shuffleList, we expect 4000 survivors, since
the sum of elements is not affected by the order of the elements. For the addElements mutator, there could
be some coincidences where the elements added to the front cancel out with the elements added to the back
of the output list. In that case, the sum also remains the same, but this will probably not happen very often
so we expect a low number of survivors. removeElements and subList also have the possibility of leaving the
elements unchanged, but then the output is not actually mutated and these cases are not seen as survivors.
So both will give 0 survivors. The other mutators always change elements, so these will also have 0 survivors.
The countSurvivors output confirms our hypothesis:

- PROPERTY NAME, MUTATOR NAME                                       SURVIVOR COUNT
- prop_sumIsTriangleNumberTimesInput, addElements:                  19
- prop_sumIsTriangleNumberTimesInput, removeElements:               0
- prop_sumIsTriangleNumberTimesInput, shuffleList:                  4000
- prop_sumIsTriangleNumberTimesInput, subList:                      0
- prop_sumIsTriangleNumberTimesInput, incrementElements:            0
- prop_sumIsTriangleNumberTimesInput, decrementElements:            0
- prop_sumIsTriangleNumberTimesInput, sameElements:                 0

The prop_linear property states that the difference between consecutive elements should always be equal
to the input. For addElements, we expect 0 survivors. There could be a coincidence where addElements results
in a list for which this holds, but this chance is extremely low. For removeElements, this property will always
hold, since the elements themselves and the order of them are not changed. This would result in 4000 survivors,
but again, the cases where the output is not actually mutated do not count as survivors, so the number will be
lower. Since IncrementElements and decrementElements add and subtract the same amount (1) from every element,
the difference between them doesn't change. This should result in 4000 survivors for both mutators. Since
sameElements gives a list with the same element repeated, the difference is always 0, and our input is 3, so
we expect there to be 0 survivors. However, the sameElements has a 1/20 chance in returning a list of 1 element,
which automatically satisfies the property, so we expect 4000 / 20 = 200 survivors. For subList, there will be
some cases where the order of the elements remains unchanged, but I'm not sure how to calculate the chance of that
happening. In any case, we expect survivors between 0 and 4000. The countSurvivors output confirms our hypothesis:

- PROPERTY NAME, MUTATOR NAME                                       SURVIVOR COUNT
- prop_linear, addElements:                                         0
- prop_linear, removeElements:                                      3600
- prop_linear, shuffleList:                                         0
- prop_linear, subList:                                             228
- prop_linear, incrementElements:                                   4000
- prop_linear, decrementElements:                                   4000
- prop_linear, sameElements:                                        188

The prop_moduloIsZero property states that any element of the output list of multiplicationTable is a
multiple of the input. In other words, all elements modulo the input should be 0. For the mutators that
change all elements by 1, we expect 0 survivors. These would only yield survivors if the input was 1.
The shuffleList mutator does not change any elements, so we expect 4000 survivors. For removeElements and
subList, no elements are changed, only a subset is taken. We expect close to 4000 survivors, but a bit less
because of the cases of an unchanged output that are not considered survivors. For sameElements, the element
that fills the list is chosen between 1 and 10 on a uniform distribution. Since we use the input of 3, any
multiple of 3 chosen between 1 and 10 (so 3, 6 and 9) satisfies the property. Since 3/10 possible choices will
satisfy the property, we expect 4000 / 10 * 3 = 1200 survivors. For addElements, there could be a coincidence
where all added elements are a multiple of the input, but this is chance is very low. We expect 0, or close tl 0
survivors. The countSurvivors output confirms our hypothesis.

- PROPERTY NAME, MUTATOR NAME                                       SURVIVOR COUNT
- prop_moduloIsZero, addElements:                                   2
- prop_moduloIsZero, removeElements:                                3630
- prop_moduloIsZero, shuffleList:                                   4000
- prop_moduloIsZero, subList:                                       3996
- prop_moduloIsZero, incrementElements:                             0
- prop_moduloIsZero, decrementElements:                             0
- prop_moduloIsZero, sameElements:                                  1216

-}
