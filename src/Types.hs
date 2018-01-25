module Types where

data PongGame = Game{
    ballLoc :: (Float, Float),
    ballVel :: (Float, Float),
    player1 :: Float,
    player2 :: Float,

    stateUpClick :: !Bool,
    stateDownClick :: !Bool,

    seed :: !Int
}deriving Show

width, height :: Int
width = 300
height = 300

playerWidth, playerHeight, moveOffset :: Int
playerWidth = 10
playerHeight = 80
moveOffset = 3

fps :: Int
fps = 60