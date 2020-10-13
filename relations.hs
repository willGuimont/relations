import Data.Set
import Data.Tuple

-- Data types
type Domain = [Int]
type Relation = Set (Int, Int)

-- Functions
inverse :: Relation -> Relation
inverse = Data.Set.map swap

identity :: Domain -> Relation
identity domain = fromList [(x, x) | x <- domain]

compose :: Relation -> Relation -> Relation
compose xs ys = fromList [(a, d) | (a, b) <- xl, (c, d) <- yl, b == c]
    where 
        xl = toList xs
        yl = toList ys

reflective :: Relation -> Domain -> Bool
reflective xs domain = intersection  i xs == i
    where
        i = identity domain

irreflective :: Relation -> Domain -> Bool
irreflective xs domain = intersection i xs == empty
    where
        i = identity domain

symetric :: Relation -> Bool
symetric xs = inverse xs == xs

asymetric :: Relation -> Bool
asymetric xs = intersection (inverse xs) xs == empty 

antisymetric :: Relation -> Domain -> Bool
antisymetric xs domain = a `isSubsetOf` i
    where 
        a = intersection (inverse xs) xs
        i = identity domain

transitive :: Relation -> Bool
transitive xs = compose xs xs `isSubsetOf` xs

-- Test function
main :: IO()
main = do
    let domain = [0..50]
    let relation = fromList [(x, y) | x <- domain, y <- domain, x `mod` 5 == y `mod` 5]
    print relation
    putStrLn $ "Reflective: " ++ show (reflective relation domain)
    putStrLn $ "Irreflective: " ++ show (irreflective relation domain)
    putStrLn $ "Symetric: " ++ show (symetric relation)
    putStrLn $ "Asymetric: " ++ show (asymetric relation)
    putStrLn $ "Antisymetric: " ++ show (antisymetric relation domain)
    putStrLn $ "Transitive: " ++ show (transitive relation)
