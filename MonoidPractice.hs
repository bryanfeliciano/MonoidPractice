import Data.Semigroup

class Monoid a where
    mempty :: [a] -> a
    mappend :: a -> a -> a
    mconcat :: [a] -> a

-- Practical Monoids - Probability tables --

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where
      totalProbs = sum probs
      normalizedProbs = map (\x -> x/totalProbs) probs