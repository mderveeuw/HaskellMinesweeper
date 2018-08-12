-- Data type for a cell on a field
-- Has an Int having values 1 - 8 indicating the count 
-- of surrounding mines or 9 if cell contains mine
-- Has a Bool which is True if cell is marked as mine 
-- and False if it is not marked as mine
data Cell = Cell Int Bool deriving (Show)

-- Type for a field
-- A field is represented a 2d list of cells
type Field = [[Cell]]

testfield :: Field
testfield = [[Cell 0 False, Cell 0 False, Cell 0 False, Cell 0 False],
             [Cell 1 False, Cell 1 False, Cell 2 False, Cell 1 False],
             [Cell 1 False, Cell 9 False, Cell 3 False, Cell 9 False],
             [Cell 1 False, Cell 1 False, Cell 3 False, Cell 9 False]]
