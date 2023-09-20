module Exercise6 where

type Name = Int

data Form = Prop Name
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form
          | Equiv Form Form
          deriving (Eq,Ord)

instance Show Form where
  show (Prop x)   = show x
  show (Neg f)    = '-' : show f
  show (Cnj fs)     = "*(" ++ showLst fs ++ ")"
  show (Dsj fs)     = "+(" ++ showLst fs ++ ")"
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>"
                           ++ show f2 ++ ")"
  show (Equiv f1 f2)  = "(" ++ show f1 ++ "<=>"
                           ++ show f2 ++ ")"

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs

cnf :: Form -> Form
cnf (Prop x) = Prop x                                                             -- Literal
cnf (Neg (Prop x)) = Neg (Prop x)                                                 -- Literal
cnf (Neg (Neg f)) = cnf f                                                         -- Double negation law
cnf (Neg (Cnj fs)) = Dsj (map (\ z -> cnf (Neg z)) fs)                            -- DeMorgan's law
cnf (Neg (Dsj fs)) = Cnj (map (\ z -> cnf (Neg z)) fs)                            -- DeMorgan's law
cnf (Neg f) = Neg (cnf f)                                                         -- Negation of Impl or Equiv
cnf (Cnj fs) = Cnj (map cnf fs)                                                   -- Conjuctions should stay
cnf (Dsj [x, Cnj [y, z]]) = Cnj [Dsj [cnf x, cnf y], Dsj [cnf x, cnf z]]          -- Distributive law
cnf (Impl f1 f2) = Dsj [cnf (Neg f1), cnf f2]                                     -- Conditional law
cnf (Equiv f1 f2) = Cnj [Dsj [cnf (Neg f1), cnf f2], Dsj [cnf f1, cnf (Neg f2)]]  -- Biconditional law


{-
--------- LIBRARIES ---------



--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------



--------- TESTING APPROACH ---------



-}