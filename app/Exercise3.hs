module Exercise3 where

import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all


stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool 
stronger xs p q = forall xs (\x -> (p x --> q x))
weaker xs p q = stronger xs q p



evenOrGreaterThanThree :: Int -> Bool
evenOrGreaterThanThree x = even x || x > 3

evenAndGreaterThanThree:: Int -> Bool
evenAndGreaterThanThree x = even x && x > 3

evenAndGreatherThanThree_OrEven:: Int -> Bool
evenAndGreatherThanThree_OrEven x = even x && x>3 || even x

data NameableFunction = NameableFunction {
    name:: String,
    prop:: Int -> Bool
}



-- function name extractor
getName :: NameableFunction -> String
getName (NameableFunction n _) = n

getNames:: [NameableFunction] -> [String]
getNames ps = [getName a | a <- ps ]

-- property extractor
getProp:: NameableFunction -> (Int -> Bool)
getProp (NameableFunction _ prop) = prop

strongerButNotEq :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
strongerButNotEq xs p q = let pq = stronger xs p q 
                              qp = stronger xs q p 
                          in 
                  if pq && not (pq && qp)  then True
                  else  False
                 
               

-- props :: [(Int -> Bool)] 
-- props = [even, evenOrGreaterThanThree, evenAndGreatherThanThree_OrEven, evenAndGreatherThanThree_OrEven] 

funsToSort :: [NameableFunction]
funsToSort = [
    (NameableFunction {name = "even", prop = even}),
    (NameableFunction {name = "evenOrGreaterThanThree", prop = evenOrGreaterThanThree}),
    (NameableFunction {name = "evenAndGreaterThanThree", prop= evenAndGreaterThanThree}),
    (NameableFunction {name = "evenAndGreatherThanThree_OrEven", prop = evenAndGreatherThanThree_OrEven})
    ]

-- descendentSort:: [(Int -> Bool)] -> [(Int -> Bool)] 
-- descendentSort  (p:ps) =  [compar p descendentSort ps]
   
quicksortProps :: [NameableFunction] -> [NameableFunction]  
quicksortProps [] = []  
quicksortProps (x:xs) = 
   quicksortProps [  a | a <-  xs, strongerButNotEq [-10..10] (getProp a) (getProp x) ]  
   ++ [x]
   ++ quicksortProps [ a | a <-  xs, stronger [-10..10] (getProp x) (getProp a) ]





