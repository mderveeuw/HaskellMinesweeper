-- Data type for a Tile on a field
-- Has an Int having values 1 - 8 indicating the count 
-- of surrounding mines or 9 if Tile contains mine
-- Has a Bool which is True if Tile is revealed
-- Has a Bool which is True if Tile is marked as mine 
data Tile = Tile Int Bool Bool deriving (Show)

-- Data type for a field
-- Has an Int equal to field width
-- Has an Int equal to field height
-- Has an Int equal to amount of mines in field
-- Has a 2-dimensional list of Tiles representing field
-- TODO: safe constructor?
data Field = Field Int Int Int [[Tile]] deriving (Show)

testfield :: Field
testfield =  Field 4 4 3 
            [[Tile 0 False False, Tile 0 False False, Tile 0 False False, Tile 0 False False],
             [Tile 1 False False, Tile 1 False False, Tile 2 False False, Tile 1 False False],
             [Tile 1 False False, Tile 9 False False, Tile 3 False False, Tile 9 False False],
             [Tile 1 False False, Tile 1 False False, Tile 3 False False, Tile 9 False False]]

replace :: Int -> a -> [a] -> [a]
replace n e l = take n l ++ [e] ++ drop (n + 1) l

update :: Int -> Int -> Tile -> Field -> Field
update x y tile (Field w h m f) = 
    Field w h m (replace y (replace x tile (f !! y)) f)

getTile :: Int -> Int -> Field -> Tile
getTile x y (Field _ _ _ f) = f !! y !! x

reveal :: Int -> Int -> Field -> Field
reveal x y field@(Field w h m f) = if inBounds x y field && not revealed then 
                                     if mineCount == 0 then
                                         fieldWest
                                     else
                                         field'
                                 else
                                     field
                                 where inBounds x y (Field w h _ _) 
                                            = 0 <= x && x < w && 0 <= y && y < h
                                       Tile mineCount revealed marked 
                                            = getTile x y field
                                       field' 
                                            = update x y 
                                              (Tile mineCount True marked) field
                                       fieldNorth 
                                            = reveal x (y - 1) field'
                                       fieldEast 
                                            = reveal (x + 1) y fieldNorth
                                       fieldSouth 
                                            = reveal x (y + 1) fieldEast
                                       fieldWest 
                                            = reveal (x - 1) y fieldSouth

mark :: Int -> Int -> Field -> Field
mark x y field@(Field w h m f) = update x y markedTile field
                    where Tile mineCount revealed _ = getTile x y field
                          markedTile = Tile mineCount revealed True

chord :: Int -> Int -> Field -> Field
chord = undefined
