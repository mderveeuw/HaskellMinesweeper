-- Data type for a Tile on a field
-- Has an Int having values 1 - 8 indicating the count 
-- of surrounding mines or 9 if Tile contains mine
-- Has a Bool which is True if Tile is revealed
-- Has a Bool which is True if Tile is marked as mine 
data Tile = Tile { count :: Int
                 , revealed :: Bool
                 , marked :: Bool
                 } deriving (Show)

-- Data type for a field
-- Has an Int equal to field width
-- Has an Int equal to field height
-- Has an Int equal to amount of mines in field
-- Has a 2-dimensional list of Tiles representing field
-- TODO: safe constructor?
data Field = Field { width :: Int
                   , height :: Int
                   , mines :: Int
                   , field :: [[Tile]]
                   } deriving (Show)

type Coord = (Int, Int)

testfield :: Field
testfield =  Field 4 4 3 
            [[Tile 0 False False, Tile 0 False False, Tile 0 False False, Tile 0 False False],
             [Tile 1 False False, Tile 1 False False, Tile 2 False False, Tile 1 False False],
             [Tile 1 False False, Tile 9 False False, Tile 3 False False, Tile 9 False False],
             [Tile 1 False False, Tile 1 False False, Tile 3 False False, Tile 9 False False]]

replace :: Int -> a -> [a] -> [a]
replace n e l = take n l ++ [e] ++ drop (n + 1) l

update :: Coord -> Tile -> Field -> Field
update (x, y) tile (Field w h m f) = 
    Field w h m (replace y (replace x tile (f !! y)) f)

inBounds :: Coord -> Field -> Bool
inBounds (x, y) (Field w h _ _) = 0 <= x && x < w && 0 <= y && y < h

getTile :: Coord -> Field -> Tile
getTile (x, y) (Field _ _ _ f) = f !! y !! x

reveal :: Coord -> Field -> Field
reveal (x, y) field = if inBounds (x, y) field 
                                                && not revealed then 
                                          if mineCount == 0 then
                                              fieldWest
                                          else
                                              field'
                                      else
                                          field
                                 where tile@(Tile mineCount revealed marked) 
                                            = getTile (x, y) field
                                       revealedTile = if revealed || marked then
                                                          tile
                                                      else Tile mineCount 
                                                                    True marked
                                       field' = update (x, y) revealedTile field
                                       fieldNorth 
                                            = reveal (x, y - 1) field'
                                       fieldEast 
                                            = reveal (x + 1, y) fieldNorth
                                       fieldSouth 
                                            = reveal (x, y + 1) fieldEast
                                       fieldWest 
                                            = reveal (x - 1, y) fieldSouth

mark :: Coord -> Field -> Field
mark (x, y) field = update (x, y) markedTile field
                    where tile = getTile (x, y) field
                          markedTile = Tile (count tile) (revealed tile) True

chord :: Coord -> Field -> Field
chord (x, y) field = undefined
