-- Study: MSc Software Engineering.
-- This program is intended to implement a solution for a crime scene investigation
-- Time spend: 4 hours

module Exercise5

where

data Boy = Matthew | Peter | Jack | Arnold | Carl
  deriving (Eq,Show)
--deriving clause -> specifies that we want the compiler to
--automatically generate instances of the Eq and Show classes for our Boy type
boys = [Matthew, Peter, Jack, Arnold, Carl]

--Encodes whether a boy accuses another boy
accuses :: Boy -> Boy -> Bool
accuses Matthew b = not (b == Carl || b == Matthew)
accuses Peter b = b == Matthew || b == Jack
accuses Jack b = not (accuses Matthew b) && not (accuses Peter b)
accuses Arnold b = (accuses Matthew b && not (accuses Peter b)) || (accuses Peter b && not (accuses Matthew b))
accuses Carl b = not (accuses Arnold b)

{-

--------- CONVERTING NATURAL LANGUAGE INTO SPECIFICATIONS ---------

In the accuses function, we have written clauses corresponding to each of
boys' testimonies.

In general, we have drawn the following translations from english to logic(ish):
"Boy did it" -> p
"Boy didn't do it" -> not p
"Boy is telling the truth" -> accuses Boy
"Boy is lying" -> not (accuses Boy)

For Matthew, testimony is of form << not (Carl OR Matthew) >>
For Peter, testimony is of form << Matthew OR Jack >>
For Jack, it is of form << not (accuses Matthew) AND not (accuses Peter) >>
For Arnord, << (accuses Matthew AND not (accuses Peter)) OR (accuses Peter AND not (accuses Matthew)) >>
For Carl, << not (accuses Arnold)

-}

--Gives the list of accusers of each boy
accusers :: Boy -> [Boy]
accusers b = [x | x <- boys, accuses x b] --list comprehension

--Gives the list of guilty boys and list of boys who made true statements
guilty, honest :: [Boy]
guilty = [x | x <- boys, length(accusers x) == 3] -- assuming what the teacher says is true
honest = accusers (head guilty) --assuming guilty is a singleton list

--If the puzzle is well-designed, guilty should return a singleton list

{-

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

The purpose is to solve this thruth-tellers and liars problem.
The approach is to first translate natural language testimonies into specifications inside accuses function.
The we write accusers function as a list comprehension with accuses function as a condition.
Finally, in the guilty function WE ASSUME that since 3 boys are telling the truth (what the teacher says is true)
length (guilty) == 3, and this gives us the answer - the name of the accuser. There should only be one boy for whom
length (guilty) == 3, otherwise assumption does not hold.
It is important to point out that honest function assumes that guilty is a singleton list.
If guilty function is faulty, output of honest is not valid.

-}
