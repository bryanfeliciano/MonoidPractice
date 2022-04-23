import Data.Semigroup

-- class Monoid a where
--     mempty :: [a] -> a
--     mappend :: a -> a -> a
--     mconcat :: [a] -> a

-- Practical Monoids - Probability tables --

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where
      totalProbs = sum probs
      normalizedProbs = map (\x -> x/totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|" , show prob , "\n"]

instance Show PTable where
    show (PTable events probs) = mconcat pairs
      where 
          pairs = zipWith showPair events probs

-- Apply the cartesian product  --

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
      nToAdd = length l2
      repeatedL1 = map (take nToAdd . repeat) l1
      newL1 = mconcat repeatedL1
      cycledL2 = cycle l2 

-- with this code segment you now made PTable an instance of semigroup --

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
  where
      combiner = (\x y->mconcat [x,"-",y])

combinedProbs :: Probs -> Probs -> Probs
combinedProbs p1 p2 = cartCombine (*) p1 p2