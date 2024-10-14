module Problems5 (module Problems5) where

{-------------------------------------------------------------------------------

CS:3820 Fall 2024 Problem Set 5
===============================

This problem set contains exercises on list comprehension and type synonyms.
In this problem set we will model a very simple "game". 

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

 The game has four regions.

 Note: by using "deriving Eq" we make it possible to use == on this type.

-------------------------------------------------------------------------------}
data Region = Forest | Desert | Plains | Swamp
    deriving (Show, Eq)

{-------------------------------------------------------------------------------

 Part 1: Generating Game Maps
 ---------------------------------

 The goal of this part is to generate game maps.  We first have to describe
 their structure.

 Our game has four types of rooms:

 The empty room contains nothing.
 The arrow shop sells arrows for a specific price.
 The treasure room contains a specific number of coins.
 The monster room contains a monster.  To win against the monster, the player
 must use a specific number of arrows.  Then the monster drops a specified
 number of coins.

-------------------------------------------------------------------------------}
data Room = Empty         |
            ArrowShop Int | -- Number of coins one arrow costs
            Treasure Int  | -- Number of coins in the treasure
            Monster Int Int -- Number of arrows needed to win against the monster, and number of coins dropped 
    deriving (Show, Eq)

{- A local consists of a region and a room -}
type Local  = (Region, Room)

{- A map is a list of locals -}
type Map    = [Local]


{-------------------------------------------------------------------------------
1. Make a list of all rooms.

To keep the list manageable, enforce that the arrows at a shop can cost one
or two coins, a treasure contains at least one and at most four coins,
and that a monster needs one or two arrows, and drops one or two coins.

A useful builtin function is (++).  This function concatenates two lists.

>> [1,2] ++ [2,3]
[1,2,2,3]

-------------------------------------------------------------------------------}

listOfRooms :: [Room]
listOfRooms = Empty : (shops ++ treasures ++ monsters)
    where
        shops     = [ArrowShop i | i <- [1, 2]]
        treasures = [Treasure i  | i <- [1..4]]
        monsters  = [Monster i j | i <- [1, 2], j <- [1, 2]]

{-------------------------------------------------------------------------------
2. Make a list of all locations.

A location is a pair of a room and a region.  Any room can appear in any region.
-------------------------------------------------------------------------------}

listOfLocals :: [Local]
listOfLocals = [(region, room) | region <- [Forest, Desert, Plains, Swamp],
                                 room <- listOfRooms]

{- Recall the Cartesian product (cp) function from the lecture.  This function
   Takes a list of lists and returns a list of all lists that can be built by
   taking elements from the input lists.

    >> cp [[1,2], [3], [4, 5]]
    [[1, 3, 4], [1, 3, 5], [2, 3, 4], [2, 3, 5]]
-}
   
   
cp :: [[a]] -> [[a]]
cp [] = [[]] 
cp (xs:xss) = [ x:ys | x <- xs, ys <- yss ]
    where yss = cp xss 

{-------------------------------------------------------------------------------
3. Create maps.

Write a function that lists all maps of a certain size.  Recall that a map
is a list of locales.

You can use the builtin function `replicate`

>> replicate 3 1
[1,1,1]

-------------------------------------------------------------------------------}
allMaps :: Int -> [Map]
allMaps n = cp (replicate n listOfLocals)

{-------------------------------------------------------------------------------
4. Detect repeated regions.

Write a function that returns true if a map contains two consecutive locations
with the same region.

-------------------------------------------------------------------------------}

regionRepeats :: Map -> Bool
regionRepeats [] = False
regionRepeats ((r,_):xs) = regionRepeatsRec r xs
    where regionRepeatsRec r1 ((r2,_):xs)
                | r1 == r2  = True
                | otherwise = regionRepeatsRec r2 xs 
          regionRepeatsRec _ [] = False

{-------------------------------------------------------------------------------
5. Winnable maps.

Consider the following simple way to "play" a map: starting at the first location
(the first element of the map), go through the rooms in order.  Whenever you
are in a store, buy as many arrows as you can with the coins you have.

Whenever you are in a most room, if you carry enough arrows for the monster,
you get the coins the monster carries, otherwise you loose the game.

Write a function that determines whether a map is winnable (the player makes
it through the last room without loosing).  Assume the player starts with
no arrows and no coins.

Hint: write a updatePlayer function that updates the number of arrows and
coins as the player goes through a room.

-------------------------------------------------------------------------------}
type Player = (Int, Int) -- Arrows and coins 

updatePlayer :: Player -> Room -> Player
updatePlayer p      Empty           = p
updatePlayer (a, c) (ArrowShop p)   = (a + div c p, mod c p)
updatePlayer (a, c) (Treasure ct)   = (a, c + ct)
updatePlayer (a, c) (Monster an ct) = (a - an, c + ct)

containsMonster :: Map -> Bool
containsMonster = any isMonster
    where
         isMonster (_, Monster _ _) = True
         isMonster (_, _)           = False

winnable :: Map -> Bool
winnable = winnableRec (0, 0)
    where
        winnableRec _ []                  = True
        winnableRec p ((_, r):rs)
            | fst (updatePlayer p r) >= 0 = winnableRec (updatePlayer p r) rs
            | otherwise                   = False

{-------------------------------------------------------------------------------
6. Write a function that lists all maps of a given length that are winnable
   and that contain at least one monster.
-------------------------------------------------------------------------------}

winnableMaps :: Int -> [Map]
winnableMaps n = (filter winnable . filter containsMonster) (allMaps n)
