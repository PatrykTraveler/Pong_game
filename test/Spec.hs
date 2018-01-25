import Test.QuickCheck
import Lib
import Types

main :: IO ()
main = quickCheck wallCollision
