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

-- combine events and probs --

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
  where
      combiner = (\x y->mconcat [x,"-",y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

-- with this code segment you now made PTable an instance of semigroup --

instance Semigroup PTable where
    (<>) ptable1 (PTable [] []) = ptable1
    (<>) (PTable [] []) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
      where
          newEvents = combineEvents e1 e2
          newProbs = combineProbs p1 p2

-- Make PTable an  instance of monoid --
-- this gives you the utility of mconcat for free --

instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>)

-- Create a function for coin flip and spin spinner --

coin :: PTable
coin = createPTable ["heads","tails"] [0.5,0.5]

spinner :: PTable
spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]

-- To find out the probability of a certain outcome --
-- GHCI> coin <> spinner