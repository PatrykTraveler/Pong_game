module Types where

-- * Data that changes during the game

data PongGame = Game{
    -- | Stores ball position
    ballLoc :: (Float, Float),
    -- | Stores ball velocity vector
    ballVel :: (Float, Float),
    -- | Stores player 1 Y - position
    player1 :: Float,
    -- | Stores player 2 Y - position
    player2 :: Float,

    -- | Stores boolean value that stores if up-button is pressed
    stateUpClick :: !Bool,
    
    -- | Stores boolean value that stores if down-button is pressed
    stateDownClick :: !Bool,

    -- | Seed used to randomize ball position
    seed :: !Int
}deriving Show

-- | Stores width of game field 
width :: Int
-- | Stores height of game field
height :: Int

width = 300 
height = 300 

-- * Variables that can't be changed after start of the game

-- | Stores width of the paddle 
playerWidth :: Int 
-- | Stores height of the paddle 
playerHeight ::Int
-- | Stores the move offset that tells how far the paddle moves during time between frames
moveOffset :: Int

playerWidth = 10 
playerHeight = 80 
moveOffset = 3 

-- | Stores the number of frames per second
fps :: Int
fps = 60