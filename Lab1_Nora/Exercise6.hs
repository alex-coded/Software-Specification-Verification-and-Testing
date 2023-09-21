module Exercise6 where

import Lecture3

-- This function takes a Boolean proposition and converts it into CNF form. This is done
-- using the following algorithm:
-- 1. Remove arrows (implications and equivalences)
-- 2. Convert to Negation Normal Form (NNF)
-- 3. Repeatedly apply distributive laws until CNF is reached.
cnf :: Form -> Form
cnf (Prop x) = Prop x                                                                               -- Literal
cnf (Neg (Prop x)) = Neg (Prop x)                                                                   -- Literal
cnf (Neg (Neg f)) = cnf f                                                                           -- Double negation law
cnf (Neg (Cnj fs)) = Dsj (map (\ z -> cnf (Neg (cnf z))) fs)                                        -- De Morgan's law
cnf (Neg (Dsj fs)) = Cnj (map (\ z -> cnf (Neg (cnf z))) fs)                                        -- De Morgan's law
cnf (Neg f) = cnf (Neg (cnf f))                                                                     -- Negation of Impl or Equiv
cnf (Cnj fs) = Cnj (map cnf fs)                                                                     -- Conjuctions should stay
cnf (Dsj [x, Cnj [y, z]]) = Cnj [Dsj [cnf x, cnf y], Dsj [cnf x, cnf z]]                            -- Distributive law
cnf (Dsj [Cnj [x, y], z]) = Cnj [Dsj [cnf x, cnf z], Dsj [cnf y, cnf z]]                            -- Distributive law
cnf (Dsj fs) = Dsj (map cnf fs)                                                                     -- Non-distributable disjunctions should stay
cnf (Impl f1 f2) = Dsj [cnf (Neg (cnf f1)), cnf f2]                                                 -- Conditional law
cnf (Equiv f1 f2) = cnf (Cnj [Dsj [cnf (Neg (cnf f1)), cnf f2], Dsj [cnf f1, cnf (Neg (cnf f2))]])  -- Biconditional law


{-
--------- LIBRARIES ---------

For this program, the Form and Name types, the show function for the Form type are
important from Lecture3.hs.

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

The point of this program is to convert a given Boolean proposition into CNF form.
This is done by using the following algorithm:

1. Remove arrows (implications and equivalences) using the following laws:
   Conditional law:       P -> Q = ¬p v q
   Biconditional law:     P <-> Q = (¬p v q) ^ (p v ¬q)
2. Convert to Negation Normal Form (NNF) using the following laws:
   Double negation law: ¬¬p = p
   De Morgan's law: ¬(P ^ Q) = ¬p v ¬q
   De Morgan's law: ¬(P v Q) = ¬p ^ ¬q
3. Repeatedly apply the distributive law until CNF is reached:
   Distributive law: (P ^ Q) v R = (p v r) ^ (q v r)
   Distributive law: P v (Q ^ R) = (p v q) ^ (p v r)

--------- TESTING APPROACH ---------

No specific property testing was required for this exercise. To ckeck my implementation
of the cnf function, I did some manual testing with some example propositions of which I
know the CNF form. For each test, p = Prop 1 and q = Prop 2, and all known CNF outcomes are
calculated using the algorithm described above, not via truth tables. The following cases
were checked:

Proposition: ¬¬¬p
Known CNF: ¬p
Function input: Neg (Neg (Neg p))
Function output: -1
Rewritten output: ¬p
Output is correct: True

Proposition: ¬(p v ¬q)
Known CNF: ¬p ^ q
Function input: Neg (Dsj [p, Neg q])
Function output: *(-1 2)
Rewritten output: ¬p ^ q
Output is correct: True

Proposition: ¬(¬p ^ ¬q)
Known CNF: p v q
Function input: Neg (Cnj [Neg p, Neg q])
Function output: +(1 2)
Rewritten output: p v q
Output is correct: True

Proposition: (p -> q) <-> (¬q -> ¬p)
Known CNF: (q v ¬p v p) ^ (q v ¬p v ¬q) ^ (¬p v q v ¬q) ^ (¬p v q v p)
Function input: Equiv (Impl p q) (Impl (Neg q) (Neg p))
Function output: *(*(+(1 +(2 -1)) +(-2 +(2 -1))) *(+(+(-1 2) -2) +(+(-1 2) 1)))
Rewritten output: ((p v (q v ¬p)) * (¬q v (q v ¬p))) ^ (((¬p v q) v ¬q) * ((¬p v q) v p))
Output is correct: True

Proposition: ¬(¬p -> q)
Known CNF: ¬p ^ ¬q
Function input: Neg (Impl (Neg p) q)
Function output: *(-1 -2)
Rewritten output: ¬p ^ ¬q
Output is correct: True

As expected, all the cnf function yielded the correct output for these 5 test cases.
These are just 5 test cases, and it would obviously be better to test more cases, but
due to time constraints I decided to keep it at this. These tests do not prove that my
implementation of the cnf function is correct, but it does increase my confidence.

-}