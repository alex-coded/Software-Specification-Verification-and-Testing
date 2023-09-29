-- tau is a lable (same as epsilon), can be used in transition
-- after -> states after a lable. depth first searching, starting at the output
-- delta is recursive arrow
-- ! output
-- ? input
-- list of transitions and find the path
-- How to go from one state to another
-- subfunctions to trace the states.

-- Study: MSc Software Engineering.
-- This program is intended to
-- Time spent: 180 min
-- Additional dependencies
module Exercise4 where

import Data.List
import LTS
import Test.QuickCheck

after :: IOLTS -> Trace -> [State]
after =



