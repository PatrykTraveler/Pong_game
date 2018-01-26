module Param where
    import Test.QuickCheck
    import Lib
    import Types

    test4 = TestCase (quickCheck wallCollision)

    main :: IO ()
    main = quickCheck wallCollision

    
