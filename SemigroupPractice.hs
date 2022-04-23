import Data.Semigroup 

-- Combining like types: Semigroups --
-- the <> operator can be thought of as an op for combining instances of the same type --
-- (<>) :: Semigroup a => a -> a -> a 

instance Semigroup Integer where
    (<>) x y = x + y

data Color = Red|
             Yellow|
             Blue|
             Green|
             Purple|
             Orange|
             Brown deriving (Show,Eq)

-- instance Semigroup Color where
--     (<>) Red Blue = Purple
--     (<>) Blue Red = Purple
--     (<>) Yellow Blue = Green
--     (<>) Blue Yellow = Green
--     (<>) Yellow Red = Orange
--     (<>) Red Yellow = Orange
--     (<>) a b = if a == b 
--                then a
--                else Brown

-- Implement the code above with support for associativity --

instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b | a == b = a
             | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
             | all (`elem` [Blue,Yellow,Green]) [a,b] = Green
             | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange
             | otherwise = Brown