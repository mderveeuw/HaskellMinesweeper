-- Data type for a cell on a field
-- Has an Int having values 1 - 8 indicating the count 
-- of surrounding mines or 9 if cell contains mine
-- Has a Bool which is True if cell is shown
-- Has a Bool which is True if cell is marked as mine 
data Cell = Cell Int Bool Bool deriving (Show)

-- Data type for a field
-- Has an Int equal to field width
-- Has an Int equal to field height
-- Has an Int equal to amount of mines in field
-- Has a 2-dimensional list of cells representing field
-- TODO: safe constructor?
data Field = Field Int Int Int [[Cell]] deriving (Show)

testfield :: Field
testfield =  Field 4 4 3 
            [[Cell 0 False False, Cell 0 False False, Cell 0 False False, Cell 0 False False],
             [Cell 1 False False, Cell 1 False False, Cell 2 False False, Cell 1 False False],
             [Cell 1 False False, Cell 9 False False, Cell 3 False False, Cell 9 False False],
             [Cell 1 False False, Cell 1 False False, Cell 3 False False, Cell 9 False False]]

replace :: Int -> a -> [a] -> [a]
replace n e l = take n l ++ [e] ++ drop (n + 1) l

update :: Int -> Int -> Cell -> Field -> Field
update x y cell (Field w h m f) = 
    Field w h m (replace y (replace x cell (f !! y)) f)

show :: Int -> Int -> Field -> Field
show x y field = undefined

mark :: Int -> Int -> Field -> Field
mark x y field@(Field w h m f) = update x y markedCell field
                    where Cell mineCount shown _ = f !! y !! x
                          markedCell = Cell mineCount shown True
                          