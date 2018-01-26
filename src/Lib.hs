{- | 
    Main game module
 -}

module Lib where
    import Graphics.Gloss
    import Graphics.Gloss.Data.ViewPort
    import Graphics.Gloss.Interface.Pure.Game
    import Types


    -- | Function that sets game seed
    updateSeed :: PongGame -> PongGame
    updateSeed game = game {seed = seed'}
        where 
            prev = seed game
            seed' = (1103515245 * prev + 12345) `mod` (2^32) 
    
    -- | Game window declaration, uses values of 'Types.width' and 'Types.height' from 'Types' module
    window :: Display
    window = InWindow "Pong" (width, height) (10,10)
    
    -- | Sets the background color
    background :: Color
    background = white
    
    -- | Renders current view of the game 
    render :: PongGame -> Picture 
    render game =
        pictures [ball, mkPaddle black (fromIntegral (width `div` 2 - playerWidth `div` 2)) $ player1 game, 
        mkPaddle red (fromIntegral (-width `div` 2 + playerWidth `div` 2)) $ player2 game]
        where
            ball = uncurry translate (ballLoc game) $ color ballColor $ rectangleSolid 10 10
            ballColor = black
    
            mkPaddle :: Color -> Float -> Float -> Picture
            mkPaddle col x y = translate x y $ color col $ rectangleSolid (fromIntegral playerWidth) (fromIntegral playerHeight)
    
    -- | Sets the game to starting values
    initialState :: PongGame
    initialState = Game{
        ballLoc = (-30, -60),
        ballVel = (60, 60),
        player1 = 40,
        player2 = 80,
        stateUpClick = False,
        stateDownClick = False,
        seed = 149327498
    }
    
    -- | Function that draws the ball during the game
    moveBall :: Float -> PongGame -> PongGame
    moveBall seconds game = game { ballLoc = (x', y')}
        where
            (x, y) = ballLoc game
            (vx, vy) = ballVel game
    
            x' = x + vx * seconds
            y' = y + vy * seconds
    -- | Type used to store ball radius
    type Radius = Float

    -- | Type used to store position
    type Position = (Float, Float)
    
    -- | Function that checks if ball collides with wall
    wallCollision :: Position -> Radius -> Bool
    wallCollision (_, y) radius = topCollision || bottomCollision
        where
            topCollision = (y - radius) <= (-fromIntegral width/2)
            bottomCollision = (y + radius) >= (fromIntegral width/2)
    

    -- | Function that checks if ball collides with paddle      
    paddleCollision :: Position -> Float -> Float -> Radius -> Bool
    paddleCollision (x, y) p1 p2 radius = p1Collision || p2Collision
        where
            p1Collision = (x + radius) >= (fromIntegral(width `div` 2 - playerWidth)) && (y + radius) <= (p1 + fromIntegral playerHeight/2) && (y - radius) >= (p1 - fromIntegral playerHeight/2)
    
            p2Collision = (x - radius) <= (fromIntegral(-width `div` 2 + playerWidth)) && (y + radius) <= (p2 + fromIntegral playerHeight/2) && (y - radius) >= (p2 - fromIntegral playerHeight/2)
    
    -- | Function that switches the ball direction after collision
    ballBounce :: PongGame -> PongGame
    ballBounce game = game { ballVel = (vx', vy') }
        where
            radius = 5
            (vx, vy) = ballVel game
            vy' = if wallCollision (ballLoc game) radius
                then
                    -vy
                else
                    vy
            vx' = if paddleCollision (ballLoc game) (player1 game) (player2 game) radius
                then 
                    -vx
                else
                    vx
    
    -- | Function that gets player move 
    playerMovement :: PongGame -> Int -> PongGame
    playerMovement game offset = game { player1 = y'}
        where
            y = player1 game
            y' = if abs(y + fromIntegral offset + fromIntegral (signum offset * playerHeight `div` 2)) < fromIntegral height/2
                    then y + fromIntegral offset
                else 
                    y
    
    -- | Function that calculate computer move               
    computerMovement :: PongGame -> PongGame
    computerMovement game = game { player2 = y'}
        where 
            y = player2 game
            (vx, vy) = ballVel game 
            (px, py) = ballLoc game
            y' = 
                if (not $ inrange py y)
                    then if y > py
                         then y - fromIntegral moveOffset
                         else
                            y + fromIntegral moveOffset
                else
                     y

            inrange :: Float -> Float -> Bool
            inrange y py = (y + radius) <= (py + fromIntegral playerHeight/2) && (y - radius) >= (py - fromIntegral playerHeight/2)
                where 
                    radius = 5
    
    -- | Function that moves player
    movePlayer :: PongGame -> PongGame
    movePlayer game 
        | stateUpClick game = playerMovement game moveOffset
        | stateDownClick game = playerMovement game (-moveOffset)
        | otherwise = game
    
    -- | Function that resets ball position after collision with right or left wall
    resetBall :: PongGame -> PongGame
    resetBall game = game { ballLoc = (x', y')}
        where
            randomX = (seed game `mod` 300) - 150
            randomY = (seed game `mod` 300) - 150
            (x, y) = ballLoc game
            x' = if abs x > fromIntegral width/2 then fromIntegral randomX else x
            y' = if abs y > fromIntegral height/2 then fromIntegral randomY else y
                
    -- | Function that updates view
    update :: Float -> PongGame -> PongGame
    update seconds = movePlayer . computerMovement . moveBall seconds . ballBounce . resetBall
    
    -- | Function that takes player's input
    handleKeys :: Event -> PongGame -> PongGame
    handleKeys event game 
        | (EventKey (SpecialKey KeyUp) Down _ _) <- event 
        = game {stateUpClick = True}
        | (EventKey (SpecialKey KeyUp) Up _ _) <- event 
        = game {stateUpClick = False}
        | (EventKey (SpecialKey KeyDown) Down _ _) <- event 
        = game {stateDownClick = True}
        | (EventKey (SpecialKey KeyDown) Up _ _) <- event 
        = game {stateDownClick = False}
        | otherwise = game
    
    -- | Function that runs the game
    doPong :: IO ()
    doPong = play window background fps initialState render handleKeys update
