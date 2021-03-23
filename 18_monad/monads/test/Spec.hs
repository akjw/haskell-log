import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Lib ( CountMe
           , Nope
           , BahEither
           , Identity
           , List
           )

main :: IO ()
main = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  quickBatch (monad (undefined :: Nope (Int, Int, Int)))
  quickBatch (monad (undefined :: BahEither Int (Int, Int, Int)))
  quickBatch (monad (undefined :: Identity (Int, Int, Int)))
  quickBatch (monad (undefined :: List (Int, Int, Int)))

