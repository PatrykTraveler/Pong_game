module Main where
    import Test.QuickCheck
    import Test.HUnit
    import Lib
    import Types

    prop_wallCollision :: Position -> Radius -> Bool
    prop_wallCollision (x, y) radius = 
        if (y - radius) <= (-fromIntegral width/2) || (y + radius) >= (fromIntegral width/2) 
        then True == (wallCollision (x,y) radius)
        else False == (wallCollision (x,y) radius)

    test1 :: Test
    test1 = TestCase (assertEqual "For (1,1) 1, no collision schould be detected" False (wallCollision (1,1) 1) )

    test2 :: Test
    test2 = TestCase (assertEqual "For (1,1) 1, bottomCollision schould be detected" True (wallCollision (1,150) 1))

    test3 :: Test
    test3 = TestCase (assertEqual "For (1,1) 1, topCollision schould be detected" True (wallCollision (1,-150) 1))

    tests :: Test
    tests = TestList [TestLabel "wallcollision: No collision" test1,
                      TestLabel "wallcollision: bottomCollision" test2,
                      TestLabel "wallcollision: bottomCollision" test3]

    main :: IO ()
    main = do 
        putStrLn ""
        putStrLn "HUnit:"
        return () <$> runTestTT tests
        putStrLn ""
        putStrLn "QuickCheck:"
        quickCheck prop_wallCollision
            
        
    
