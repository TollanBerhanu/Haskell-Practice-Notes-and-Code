-- ** Functors, Applicative functors and Monoids
-- Functors are things that can be mapped over (like lists, Maybes, trees). The typeclass Functor, has only one
-- typeclass method, namely 'fmap' ... fmap :: (a -> b) -> f a -> f b
-- If we want to make a type constructor an instance of Functor, it has to have a kind of a -> a (it has to take only
-- one concrete type as a type parameter) .. we can't write 'instance Functor Either where', but we can write 
-- 'instance Functor (Either a) where'

import Data.Char
import Data.List
import Control.Applicative -- ZipList is not in Prelude but all are instances of Control.Applicative
import Data.Monoid
import qualified Data.Foldable as F

--  Two addidtional instances of functor are: IO and (->) r ... infix form: (r ->), but this syntax isn't allowed

-- When we fmap a function over an I/O action, we want to get back an I/O action that does the same thing,
-- but has our function applied over its result value.
{- instance Functor IO where  
    fmap f action = do  
        result <- action  
        return (f result)   -}
-- To illustrate
reverseLine :: IO String
reverseLine = do
        putStrLn "Say something..."
        line <- fmap reverse getLine -- getLine :: IO String (fmap takes IO and returns IO after applying f to its contents)
        putStrLn $ "You said "++ line ++ " backwards. Now say sth else..."
        line2 <- fmap (intersperse '-' . map toUpper . reverse) getLine
        return $ "Formatted: " ++ line2

{- instance Functor ((->) r) where  
    fmap f g = (\x -> f (g x))  -- or fmap = (.)  -}
-- That means fmap :: (a -> b) -> (r -> a) -> (r -> b) ... which is exactly function composition
funcComposition1 = fmap (*3) (+100) 1 :: Int
funcComposition2 = (*3) . (+100) $ 1 :: Int
funcComposition3 = (*3) `fmap` (+100) $ 1 :: Int

-- fmap (f) is a function that takes a functor and returns a functor (list, Maybe, Either a, (->)r )
fmapExample1 = fmap (replicate 3) [1,2,3] -- [[1,1,1],[2,2,2],[3,3,3]]
fmapExample2 = fmap (replicate 3) (Just [1,2,3]) -- Just [[1,2,3],[1,2,3],[1,2,3]]
fmapExample3 = fmap (replicate 3) Nothing -- Nothing
fmapExample5 = fmap (replicate 3) (Right "foo") -- Right ["foo","foo","foo"]
fmapExample4 = fmap (replicate 3) (Left "foo") -- Left "foo" -- (Either a) is a functor (because they take only one type)

-- There are basically two laws of functors
-- The first law states: if we map the 'id' function over a functor, we get back the same fucntor as the original functor
identityFunctor = fmap id [1,2,3,4,5] -- [1,2,3,4,5] ... basically 'id' = (\x -> x)

-- The second law states: composing two functions and then mapping the resulting function over a functor is the same as
-- first mapping one function over the functor and then mapping the other one ... fmap (f . g) = fmap f . fmap g

data CMaybe a = CNothing | CJust Int a deriving (Show, Eq) 
instance Functor CMaybe where  
    fmap f CNothing = CNothing  
    fmap f (CJust counter x) = CJust (counter+1) (f x)
newFunctorInstance = (fmap id $ CJust 10 [1,2]) == (CJust 10 [1,2]) -- CJust 11 [1,2] /= CJust 10 [1,2]
-- For example, this instance does not obey the first functor law ... so 'CMaybe' it's not considered to be a functor

-- ***** Applicative functors
-- What happens if we map a double parameter function to a functor
listOfFunctions1 = fmap (*) [1,2,3] -- [(*1),(*2),(*3)] :: (Num a) => [a -> a]
listOfFunctions2 = fmap compare "ABCDEFG" -- [(`compare` A),..] :: [Char -> Ordering] ... because it is a list of Chars
listOfFunctions3 = fmap (\x y z -> x + y / z) [3,4,5,6]  -- :: (Fractional a) => [a -> a -> a]

listOfResults1 = fmap (\f -> f 9) listOfFunctions1 -- [9,18,27]
-- What if we have a functor 'Just (3 *)' and a functor 'Just 5' and we want to take the function from Just (3 *) and
-- map it over Just 5? We could use pattern matching to extract the function but there's a more general way for all functors.

-- The Applicative typeclass provides two methods: pure and <*>
{- class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b -}
-- from the class definition, we can dedude if a type constructor is part of the Applicative typeclass, it's also in
-- Functor, so we can use fmap on it

-- ** 'pure' takes a value and puts it in a default context / minimal context. But it has to have a value (can't be: Nothing, [])
-- ** '<*>' takes the content of a functor and maps it onto another functor

-- Applicative instance implementation for Maybe
{- instance Applicative Maybe where  -- notice Maybe is also an instance of Functor
    pure = Just  -- we could have done pure x = Just x
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something   -}
pureMaybe = pure "Hey" :: Maybe String -- Just "Hey" 
applicativeMaybe1 = Just (+3) <*> Just 10  -- Just 13
applicativeMaybe2 = pure (+3) <*> Just 10  -- Just 13 ... use pure if you are using <*>
applicativeMaybe3 = Just (++"foo") <*> Nothing -- Nothing
applicativeMaybe4 = Nothing <*> Just "woot" -- Nothing
-- Applicative functors allow you to operate on several functors with a single function ( pure f <*> x <*> y <*> ... )
-- Also, <*> is left associative (the left side is computed first)
applicativeMaybe5 = pure (+) <*> Just 3 <*> Just 5 -- Just (+3) <*> Just 5 ... Just 8 
applicativeMaybe6 = pure (+) <*> Just 3 <*> Nothing -- Nothing
applicativeMaybe7 = pure (+) <*> Nothing <*> Just 5 -- Nothing

--  pure f <*> x <*> y <*> ... is the same as  fmap f x <*> y <*> ...
-- Control.Applicative exports a function called <$>, which is just fmap as an infix operator.
{- (<$>) :: (Functor f) => (a -> b) -> f a -> f b  
f <$> x = fmap f x  -}

-- Let's say we have values: Just "foo" and Just "bar" and we join them into one String inside a Maybe functor like this:
fooBar = (++) <$> Just "foo" <*> Just "bar" -- pure (++) <*> Just "foo" <*> Just "bar" ... Just ("foo"++) <*> Just "bar"

-- Applicative implementation of lists
{- instance Applicative [] where  
    pure x = [x]  
    fs <*> xs = [f x | f <- fs, x <- xs]  -} -- every element in list 1 is applied to every element in list 2
pureList = pure "Hey" :: [String] -- ["Hey"]
applicativeList1 = [(*0),(+100),(^2)] <*> [1,2,3] -- [(1*0),(2*0),(3*0), (1+100),(2+100),(3+100), (1^2),(2^2),(3^2)]
                                                  -- [0,0,0, 101,102,103, 1,4,9]
applicativeList2 = [(+),(*)] <*> [1,2] <*> [4,5]  -- [(1+),(2+),(1*),(2*)] <*> [4,5] ... left associative
                                                  -- [(1+4),(1+5),(2+4),(2+5), (1*4),(1*5),(2*4),(2*5)]
                                                  -- [5,6,6,7, 4,5,8,10]
applicativeList3 =  (++) <$> ["foo","bar"] <*> ["!","."] -- [("foo"++), ("bar"++)] <*> ["!","."]
                                                         -- ["foo!","foo.", "bar!","bar."]
-- To find all possible products of two lists we can use these two methods
prods1 = [x*y | x <- [1,2,3], y <- [4,5,6]] -- [4,5,6,8,10,12,12,15,18]
prods2 = (*) <$> [1,2,3] <*> [4,5,6] -- or pure (*) <*> [..] <*> [..] --> [4,5,6,8,10,12,12,15,18]

-- Applicative implementation of IO
{- instance Applicative IO where  
    pure = return  -- yields some result but doesn't really do anything (pure x = return x)
    a <*> b = do  
        f <- a  
        x <- b  
        return (f x)   -}
pureIO = pure "retval" :: IO String -- "retval" returned on the terminal
pureIO2 = do val <- pure 123
             putStrLn $ show val -- 123 printed to terminal
-- <*> takes an (I/O action that takes a function as its result) and (another I/O action) to return a new I/O action
-- Consider this
concatStrings :: IO String  
concatStrings = do  a <- getLine  -- "foo"
                    b <- getLine  -- "bar"
                    return $ a ++ b -- "foobar"
concatStringsApplicative :: IO String
concatStringsApplicative = (++) <$> getLine <*> getLine -- "foo" "bar" ... "foobar"
-- we can also bind the value of the applicative IO
concatStringsApplicativeBind :: IO ()
concatStringsApplicativeBind = do value <- pure (++) <*> getLine <*> getLine
                                  putStrLn $ "Concatenated: " ++ value

-- Applicative implementation of functions
{- instance Applicative ((->) r) where  
    pure x = (\_ -> x)  -- any parameter is omitted and the value given to pure is returned
    f <*> g = \x -> f x (g x)  -}
pureFunc = pure 12 "anything" -- 12
pureFunc2 = pure "hello" (++ "hi") -- "hello"
-- Calling <*> with two applicative functors results in an applicative functor
-- (if we use <*> on two functions, we get back a function)
-- (+) <$> (+3) <*> (*100) : function that will use (+) on the results of (+3) and (*100) and return that
applicativeFunc1 = (+) <$> (+3) <*> (*100) $ 5 -- ((+3)+) <*> (*100) = \x -> ((x + 3) +) (x * 100)
                                               -- ((5 + 3) +) (5 * 100) = 508
applicativeFunc2 = (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
-- the lambda function here has 3 params which it takes from the results of the three functions applied to a value
-- ( \x -> [(x+3),(x*2),(x/2)] )   
-- We can think of functions as boxes that contain their eventual results, so doing k <$> f <*> g creates
-- a function that will call 'k' with the eventual results from f and g
applicativeFunc3 = (+) <$> (+10) <*> (+5) -- we use (+) on the future results of (+10) and (+5) ... which take 1 parameter
-- ghci> applicativeFunc3 12 = (12+10) (+) (12+5) = 22 (+) 17 = 39
-- applicativeFunc4 = (+) <$> (+10) <*> (+5) <*> (-2) -- error because the applicative functors exceed the no of params

-- Applicative implementation of ZipList
{- instance Applicative ZipList where  
        pure x = ZipList (repeat x) -- because it has to produce the value on every position of the other parameter
        -- the resulting list will be as long as the shorter of the two lists (in this case, the finite list).
        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)   -}
pureZipList = pure "haha" :: ZipList String -- ZipList {getZipList = ["haha","haha",..]} ... ZipList doesn't derive Show
pureZipList2 = pure (*2) <*> ZipList [2,4,6] -- ZipList {getZipList = [4,8,12]} ... getZipList derives Show
-- Here <*> applies the first function to the first value, the second function to the second value, etc.
applicativeZipList1 = getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100] -- [101,102,103]  
applicativeZipList2 = getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..] -- [101,102,103]  
applicativeZipList3 = getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"  -- [('d','c','r'),('o','a','a'),('g','t','t')]
-- (,,) is the same as \x y z -> (x,y,z)
-- Unllike (zipWith3,..,zipWith7), we don't need a separate zip function for each number of lists that we want to zip with. 

-- Control.Applicative defines a function that's called liftA2
{- liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
liftA2 f a b = f <$> a <*> b -}
withoutLiftExample = (:) <$> Just 3 <*> Just [4]  -- Just [3,4]
withLiftExample = liftA2 (:) (Just 3) (Just [4])  -- Just [3,4]

-- ** The 'newtype' keyword
-- is used to create new data types like the 'data' keyword but faster. It only wraps an existing type into a new type
-- you can only have one value constructor and the value constructor can only have one field (unlike 'data'), for example:
type F1 = (Int, String)
data MyDataType = ValCons1 F1 Char | ValCons2 deriving (Show, Eq)
data MyDataType2 = MyDataType2 { getFeild1 :: Int, getField2 :: [Char] } deriving (Show)
newtype MyNewDataType = ValCons Int deriving (Show, Eq)
newtype MyNewDataType2 = MyNewDataType2 { getFeild :: Int } deriving (Show, Eq)

newtype CharList = CharList { getCharList :: [Char] } -- gerCharList function will be generated automatically
-- CharList :: [Char] -> CharList ... takes a [Char] value and converts it to a CharList value
-- getCharList :: CharList -> Char ... takes a CharList value and converts it to a [Char] value

-- We can easily make 'Maybe' an instance of a functor because its type constructor takes only one parameter
-- If we want to make a tuple an instance of a functor and make 'fmap' apply to only the first parameter, we do this:
newtype MyTuple b a = MyTuple {getMyTuple :: (a,b)} deriving (Show)-- make the second type parameter represent the first component
instance Functor (MyTuple b) where
    fmap f (MyTuple (x,y)) = MyTuple(f x, y)
newtypeExample = getMyTuple $ fmap (*10) (MyTuple (2, 3)) -- (200,3)
newtypeExample2 = fmap reverse (MyTuple ("hello", 123)) -- MyTuple {getMyTuple = ("olleh",123)} ... MyTuple must derive from Show

--  The 'undefined' value in Haskell represents an erronous computation.
myList = [1,2,undefined,4]
headMyList = head myList -- 1
tailMyList = tail myList -- throws an exception
-- Now let's see the effect of 'undefined' on creating types with 'data' vs 'newtype'
data MyDataBool = MyDataBool { getMyDataBool :: Bool }
newtype MyNewtypeBool = MyNewtypeBool { getMyNewtypeBool :: Bool }

sayHelloBool :: Bool -> String
sayHelloBool _ = "Hello from normal bool"
sayHelloData :: MyDataBool -> String
sayHelloData (MyDataBool _) = "Hello from data" -- MyDataBool can be (MyDataBool True) or (MyDataBool False)
sayHelloNewtype :: MyNewtypeBool -> String
sayHelloNewtype (MyNewtypeBool _) = "Hello from newtype"
-- Now, lets apply these functions with undefined instead of their respective bools
undefinedNormalBool = sayHelloBool undefined -- "Hello from normal bool"
undefinedDataBool = sayHelloData undefined -- Error ... because data can have multiple parameters and hakell has to
-- evaluate the 'undefined' value to know which pattern of parameters it matches to
undefinedNewtypeBool = sayHelloNewtype undefined -- should work normally because of newtype laziness (but it doesn't for some reason)

-- The fundamental difference between 'data' and 'newtpe' is  data can be used to make your own types from scratch (with
-- multiple value constructors and fields), but newtype is for making a completely new type out of an existing type
-- (wrapping it in a new type, usually so that is easier to make then instances of certain typeclasses).
newtype CharList' = CharList' { getCharList' :: [Char] }
-- Here, its not possible to do: (List ++ CharList) or even (CharList ++ CharList), because '++' only works for Lists
-- However, convert two 'CharLists' to 'lists', '++' them and then convert that back to a 'CharList'.

--The new type isn't automatically made an instance of the typeclasses the original type belongs to, so we derive them
-- or manually define them.
instance Show CharList' where
    show x = "My CharList: " ++ getCharList' x

-- When we use record syntax with newtype declarations, we get functions for converting between the new type and the
-- original: the value constructor (CharList') and the function for extracting the value in its field (getCharList')
myCharList' = CharList' "Hello" -- My CharList: Hello
myList' = getCharList' myCharList' -- "Hello"

-- ***** Monoids
-- A 'Monoid' typeclass has the following property: when you have an associative binary function and a value which acts
-- as an identity with respect to that function
-- For example: * , ++ are instances of Monoid because:
--      (2 * 3) * 4 == 2 * (3 * 4) and ("Hi" ++ "Low") ++ "Slow" == "Hi" ++ ("Low" ++ "Slow") ... associativity
--      5 * 1 == 1 * 5 and "Hello" ++ [] == [] ++ "Hello" ... identity

-- This is how the Monoid typecalss is defined
{- class Monoid m where  -- import Data.Monoid
    mempty :: m  -- represents identity value for a particular monoid
    mappend :: m -> m -> m  -- takes two monoid values and returns a third (2 * 3 = 6)
    mconcat :: [m] -> m  -- takes a list of monoid values and reduces them into a single value using mappend
    mconcat = foldr mappend mempty  -}  -- just takes mempty as a starting value and right folds the list with mappend
-- notice that only concrete types 'm' can be instaces of Monoid 

-- When making instances of Monoids, make sure they follow these rules, you can do without them but it defeats the purpose
-- of making a type an instance of Monoids
{-  --> mempty `mappend` x = x ..... identity
    --> x `mappend` mempty = x  
    --> (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z) ..... associativity -}

-- ** Lists are Monoids, regardless of the type they hold
{- instance Monoid [a] where  
    mempty = []  
    mappend = (++)   -}
monoidList1 = [1,2,3] `mappend` [4,5] -- [1,2,3,4,5]
monoidList2 = [] `mappend` [4,5] -- [4,5]
monoidList3 = "Hello" `mappend` mempty -- "Hello"
monoidList4 = mconcat ["Hello", " ", "Hi", "!"] -- "Hello Hi!"
monoidList5 = mempty :: [a] -- [] ... we give a type of [a] to tell the compiler which instance to refer to

-- ** Product and Sum are Monoids
-- '*' has an identity 1 and '+' has an identity 0, but both equate on Num typeclass. That means there are two valid ways
-- for numbers to be monoids. To get around this problem, Data.Monoid exports two types: Product and Sum
newtype Product' a = Product' { getProduct' :: a } deriving (Eq, Ord, Read, Show, Bounded)
newtype Sum' a = Sum' { getSum' :: a } deriving (Eq, Ord, Read, Show, Bounded)

{- instance Num a => Monoid (Product' a) where
    mempty = Product' 1 -- Sum' 0
    Product' x `mappend` Product' y = Product' (x * y) -- Sum' (x + y) -}

monoidProduct1 = Product 5 `mappend` Product 3 -- Product {getProduct = 15}
monoidProd2 = getProduct . mconcat . map Product $ [2,3,4] -- 24
monoidSum1 = getSum (Sum 5 `mappend` Sum 3) -- 8
monoidSum2 = getSum $ mempty `mappend` Sum 3 `mappend` Sum 4 -- 7

-- ** Any and All are Monoids
-- Bool can also act like a Monoid in two valid ways. The first is with binary function '||' and identity value 'False'  
-- The other way is with binary function '&&' and identity value 'True'. That's why Data.Monoid exports 'Any' and 'All'
newtype Any' = Any' { getAny' :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
newtype All' = All' { getAll' :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
{- instance Monoid Any' where
    mempty = Any' False -- True for All
    Any' x `mappend` Any' y = Any' (x || y) -- x && y for All -}
monoidAny1 = Any True `mappend` Any False -- Any {getAny = True}
monoidAny2 = getAny $ mempty `mappend` Any True -- True
monoidAll1 = getAll $ mempty `mappend` All True -- True
monoidAll2 = getAll . mconcat . map All $ [True, True, False]  -- False

-- ** The ordering Monoid (EQ, GT, LT)
{- instance Monoid Ordering where
    mempty = Eq
    LT `mappend` _ = LT
    EQ `mappend` a = a
    GT `mappend` _ = GT -}
-- When we compare the two words: ON and OX, first: O `compare` O = EQ. so we move on, Second N `compare` X = LT (Voila)
monoidOrd1 = LT `mappend` GT -- LT
monoidOrd2 = GT `mappend` LT -- GT
monoidOrd3 = mempty `mappend` LT -- LT .. mempty = EQ

-- Let's say we are comparing two strings by length, if their lengths are equal, then we compare by their alphabets
stringCompare :: String -> String -> Ordering
stringCompare str1 str2 = let compLength = length str1 `compare` length str2
                              compAlphabet = str1 `compare` str2
                          in if compLength == EQ then compAlphabet else compLength
-- Because Ordering is a monoid, we can write this in a simpler manner
stringCompareMonoid :: String -> String -> Ordering
stringCompareMonoid str1 str2 = (length str1 `compare` length str2) `mappend` (str1 `compare` str2)
-- We can expand this function to also compare for the number of vowels and make it the second most important criterion
-- for comparison
stringCompareMonoidVowel :: String -> String -> Ordering
stringCompareMonoidVowel str1 str2 = (length str1 `compare` length str2) `mappend`
                                        (vowels str1 `compare` vowels str2) `mappend`
                                            (str1 `compare` str2)
                                where vowels = length . filter (`elem` "aeiou")

-- ** Maybe is a Monoid
-- Data.Monoid provides 'First a' and 'Last a' types for Maybe Monoids
newtype First' a = First' { getFirst' :: Maybe a } deriving (Eq, Ord, Read, Show)
newtype Last' a = Last' { getLast' :: Maybe a } deriving (Eq, Ord, Read, Show)

{- instance Monoid (First' a) where  
    mempty = First' Nothing  
    First' (Just x) `mappend` _ = First' (Just x)  
    First' Nothing `mappend` x = x  -}
monoidMaybeFirst1 = First (Just 'a') `mappend` First (Just 'b') -- First {getFirst = Just 'a'}
monoidMaybeFirst2 = getFirst $ First Nothing `mappend` First (Just 'b') -- Just 'b'
monoidMaybeFirst3 =  getFirst . mconcat . map First $ [Nothing, Just 9, Just 10] -- Just 9
monoidMaybeLast1 =  getLast . mconcat . map Last $ [Nothing, Just 9, Just 10] -- Just 10
monoidMaybeLast2 = Last (Just "one") `mappend` Last (Just "two") -- Last {getLast = Just "two"}

-- ** Data.Foldable also has foldl, foldr functions that work on any foldable data structures (not only lists)
foldableMaybe1 = F.foldl (+) 2 (Just 9)  -- 11
foldableMaybe2 =  F.foldr (||) False (Just True) -- True

-- If we want to make our own data structure (like trees) foldable, we can make it an instance of Foldable and implement
-- fold functions on it or we can easily implement the foldMap function on it:
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read. Eq)
instance F.Foldable Tree where
    foldMap f EmptyTree = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                             f x           `mappend`  
                             F.foldMap f r  
