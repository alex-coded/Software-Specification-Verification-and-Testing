data Boy = Matthew | Peter | Jack | Arnold | Carl
    deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

-- main :: IO ()
-- main = do
--     undefined
--     -- print Boy

accuses :: Boy -> Boy -> Bool
accuses Matthew Carl = False
accuses Matthew Matthew = False
accuses Peter b = b == Matthew || 
-- accuses Jack Peter = True
-- accuses Jack Matthew = True
-- accuses Carl Arnold = True

accusers :: Boy -> [Boy]
accusers Matthew = [Peter, Jack, Arnold]
accusers Peter = []
accusers Jack = []
accusers Arnold = []
accusers Carl = []





-- guilty, honest :: [Boy]
-- guilty 






-- if the puzzle is well-designed, 
-- then guilty should give a singleton list.