-- import System.Posix.Fcntl (Advice(AdviceRandom))
import Data.List (permutations)
data Boy = Matthew | Peter | Jack | Arnold | Carl
    deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]


accuses :: Boy -> Boy -> Bool
accuses Matthew b = not (b == Carl || b == Matthew)
accuses Peter b = b == Matthew || b == Jack
accuses Jack b = not (accuses Matthew b) && not (accuses Peter b)
accuses Arnold b = (accuses Matthew b && not (accuses Peter b)) || (accuses Peter b && not (accuses Matthew b))
accuses Carl b = not (accuses Arnold b)

accusers :: Boy -> [Boy]
accusers b = [x | x <- boys, accuses x b]

-- accusers Matthew = [Peter, Arnold]
-- accusers Peter = [Matthew, Arnold]
-- accusers Jack = [Matthew, Peter, Carl]
-- accusers Arnold = [Matthew, Arnold]
-- accusers Carl = [Jack, Carl]

main :: IO ()
main = do
    let res = filter (\x -> length x == 3) $ map accusers boys
    print res



-- if the puzzle is well-designed, 
-- then guilty should give a singleton list.


{-

--------- LIBRARIES ---------

We used libr

--------- CODE DESCRIPTION (PURPOSE, FEATURES, ARCHITECTURE) ---------

--------- TESTING APPROACH ---------

-}