-- ** Monads
-- Monada are an extension of applicative functors. They are concerned with this logic:
-- If you have a value with a context, m a, how do you apply to it a function that takes a normal a and returns a value
-- with a context. Here context means functor (Maybe a, [a], IO a)
-- We want this function: (>>=) :: (Monad m) => m a -> (a -> m b) -> m b  

-- Monads are just applicative functors that suppot >>=. '>>=' take a modadic value and a normal function and returns a
-- monadic value after applying the function to it
-- Here's an implementation of '>>=' but only for Maybe
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x

monadicMaybe1 = applyMaybe (Just 5) (\x -> Just (x*2)) -- Just 10
monadicMaybe2 = Nothing `applyMaybe` \x -> Just (x ++ ":)") -- Nothing
monadicMaybe3 = Just "Hello" `applyMaybe` \x -> Nothing -- Nothing

-- Just like functors have the 'Functor' type class and applicative functors have the 'Applicative' type class,
-- monads come with their own type class: 'Monad'
{- class Monad m where  -- or (Applicative m) => Monad m ... Every monad is an applicative functor
    return :: a -> m a  -- same as pure, just the name is different
  
    (>>=) :: m a -> (a -> m b) -> m b -- >>= is read as bind. It takes a monadic val and a func and returns a monadic val
  
    (>>) :: m a -> m b -> m b  
    x >> y = x >>= \_ -> y  
  
    fail :: String -> m a  -- we'll never use it explicitly in our code, Haskell uses it to enable failure in monads
    fail msg = error msg -}

-- Now lets look at a Maybe instance of Monad
{- instance Monad Maybe where  
    return x = Just x  
    Nothing >>= f = Nothing  
    Just x >>= f  = f x  
    fail _ = Nothing  -}
monadicMaybe4 = return "WHAT" :: Maybe String -- Just "WHAT"
monadicMaybe5 = Just 9 >>= \x -> return (x*10) -- Just 90

-- Let's simulate birds landing on and flying away from a horizontal pole. The pole keeps balance if the number of birds on the left side
-- of the pole and on the right side of the pole is within three. |Left - Right| <= 3
type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
-- landLeft _ Nothing = Nothing ... we don't need this because Nothing is propagated in the monads as we'll see below
landLeft birds (left, right)
    | abs (birds + left - right) > 3 = Nothing
    | otherwise = Just (birds + left, right)
    
landRight :: Birds -> Pole -> Maybe Pole
landRight birds (left, right)
    | abs (left - (birds + right)) > 3 = Nothing
    | otherwise = Just (left, birds + right)

x -: f = f x -- we define the finction (-:) to allow a parameter 'x' to be given from the left side of a function 'f'
-- This is defined for better readablity, instead of nesting functions in parenthesis

normParamExample1 = (*3) 100
flipParamExample1 = 100 -: (*3) -- 300
normParamExample2 = landLeft 2 (0,0)
flipParamExample2 = (0,0) -: landLeft 2 -- Just (2,0)

-- birdSenario1 = (0,0) -: landLeft 2 -: landLeft 1 ... this gives an error because (landLeft 2) returns a Maybe but
-- (landLeft 1) takes a normal tuple

birdSenario1 = (0,0) -: landLeft 2 >>= landLeft 1 >>= landRight 4 >>= landRight 2 -- Just (3,6)
birdSenario2 = return (0,0) >>= landLeft 2 >>= landRight 8 >>= landRight (-9) -- Nothing ... because once the pole is
-- off balance, Nothing is returned and is propagated upto the end, even if the end result seems fine

-- Each step relies on the previous one's result. To illustrate this even more, let's defile a function that always results
-- the pole to be off balance.
slipPole :: Pole -> Maybe Pole
slipPole _ = Nothing

birdSenario3 = return (0,0) >>= landLeft 2 >>= landRight 3 >>= slipPole >>= landLeft 1 -- Nothing ... pole already slipped

-- Instead of making functions that ignore their input and return a predetermined monadic value, we can use (>>)
birdSenario4 = return (0,0) >>= landLeft 2 >> Nothing >>= landLeft 1 -- Nothing

-- ***** The 'do' notation in Monads
-- Just like with IO, do is used to glue several monadic actions in sequecne. Each line must featurea a monadic value:
-- Consider this:
nestedMonad1 = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y))) -- Just "3!"
nestedMonad2 = Just 3 >>= (\x -> Nothing >>= (\y -> Just (show x ++ y))) -- Nothing
-- We feed 3 to the outer lambda, which feeds "!" to the inner lambda. Then the inner lambda results in Just (show x ++ y)
-- with the variables obtained from the two lambdas
-- 'do' provides a better syntax for chaining monadic values
doMonad1 :: Maybe String 
doMonad1 = do x <- Just 3     --  Just 3  >>= (\x ->
              y <- Just "!"   --  Just "!" >> = (\y ->
                -- if we have Maybe String and bind it using (<-), we will get a String value ("!")
              Just (show x ++ y) -- Just (show x ++ y) ))   
                -- the last expression is the result of the whole monadic chain

nestedMonad3 = Just 9 >>= (\x -> Just (x > 8))  
doMonad2 :: Maybe Bool
doMonad2 = do x <- Just 9 -- x = 9
              Just (x > 8) -- Just True

-- We could also write the bird landing on a pole example using 'do'
birdPoleDo :: Maybe Pole
birdPoleDo = do
        start <- return (0,0) -- start = (0,0)
        first <- landLeft 2 start -- first = (2,0)
        Just (10,5) -- This monadic value isn't bound to anything, it has no effect unless it's a failure (Nothing)
        second <- landRight 3 first -- second = (2,3)
        -- Nothing-- (_<-Nothing) this is like putting (>>) before the monadic value whose input we want to ignore
        landLeft 1 second -- Nothing ... It doesn't matter whether we used third here or not, the 'Nothing' will propagate

-- do expressions are not imperative... they're sequential, meaning each value in the line relies on the previous ones. 
-- In this case, if one line fails (Nothing), so do the rest.
birdPoleWithoutDo :: Maybe Pole  
birdPoleWithoutDo =
    case Just (0,0) of   
        Just start -> case landLeft 2 start of  
            Nothing -> Nothing  
            Just first -> case landRight 2 first of  
                Nothing -> Nothing  
                Just second -> landLeft 1 second 

-- We can utilize pattern matching in 'do' expressions
justH :: Maybe Char
justH = do (x:xs) <- Just "Hello"
           return x -- Just 'H'
-- If pattern matching fails in a 'do' expression, the 'fail' function is called
-- In the case of Maybe, 'Nothing' is returned. (fail msg = Nothing)
justFail :: Maybe Char
justFail = do (x:xs) <- Just ""
              return x -- Nothing

-- Lists are Monads
{- instance Monad [] where  
    return x = [x]  
    xs >>= f = concat (map f xs)  
    fail _ = []  -}
-- (>>=) takes val with context (Monadiv val) and a function that takes a normal val, and returns a Monadic val (list)
monadicList1 = [2,3,4] >>= \x -> [x, -x] -- [2,-2,3,-3,4,-4]
-- A list ([2,3,4]) is a non-deterministic value. When we feed it to a function that returns a non-deterministic val (list),
-- it equates all possible results by applying the function to each element and concatenates them.

-- Non-determinism also supports failure. Here, [] is the equivalient of Nothing
monadicList2 = [] >>= \x -> [1,2,3] -- []
monadicList3 = "" >>= \x -> [1,2,3] -- [] ... no element can be passed to the lambda, resulting in []
monadicList4 = "Hello" >>= \x -> "" -- "" ... each element is passed and ignored
-- We can chain several lists with >>=
monadicList51 = [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)    -- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
monadicList52 = [1,2] >>= ( \n -> ( ['a','b'] >>= \ch -> [(n,ch)] ) ) -- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
-- Same example using 'do'
monadicListDo :: [(Int,Char)]
monadicListDo = do a <- [1,2]
                   b <- ['a','b']
                   return (a,b) -- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
-- List comprehensions are just syntactic sugar for using lists as monads
normalWayOfDoingThis = [(a,b) | a <- [1,2], b <- ['a','b']]           -- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

-- ** To see how list comprehensions translate to list monads, lets look at the MonadPlus typeclass and the gurad funciton
-- MonadPlus typecalss is for Monads that can also act like Monoids
class Monad m => MonadPlus m where  
    mzero :: m a  -- mempty ... identity value
    mplus :: m a -> m a -> m a  -- mappend ... associativity

-- Because lists are both Monads and Monoids, they can be an instance of this typeclass
instance MonadPlus [] where  
    mzero = [] -- represents a non-deterministic computation that has no results at all — a failed computation
    mplus = (++) -- just joins two non-deterministic values into one

-- The guard function is defined like this
guard :: (MonadPlus m) => Bool -> m ()  -- 'guard' takes a boolean value
guard True = return ()  -- If True, it takes an empty tuple () and puts it in a minimal default context that doesn't fail
guard False = mzero  -- Otherwise, it makes a failed monadic value

-- Lets see 'guard' in action
-- monadPlusMaybe1 = guard (5 > 2) :: Maybe () -- Just () ... success, put () in minimal context ... () is a dummy result
-- monadPlusMaybe2 = guard (5 == 2) :: Maybe () -- Nothing ... fail, return failed monadic value
monadPlusList1 = guard (5 > 2) :: [()] -- [()]
monadPlusList2 = guard (5 == 2) :: [()] -- []

-- We can chain guards with certain conditions to determine success or failure
monadPlusList3 =  guard (5 > 2) >> return "cool" :: [String]  -- [()] >> ["cool"] ... [()] >>= _->["cool"] ... ["cool"]
-- Here, the empty tuple produced by the guard is ignored and the result "cool" is propagated
monadPlusList4 =  guard (5 == 2) >> return "cool" :: [String] -- [] >> ["cool"] ... [] >>= _->["cool"] ... []
--Here, the empty list is propagated (feeding an empty list to a function with >>= always results in an empty list)

-- Here's how we can use guards like list comprehensions ... for listing all numbers up to 50 that contain number 7
usingListComprehension = [x | x <- [1..50], '7' `elem` show x] -- [7,17,27,37,47]
usingMonadGuard = [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x) -- [7,17,27,37,47]
usingDo :: [Int]
usingDo = do x <- [1..50]
             guard ('7' `elem` show x)
             return x -- [7,17,27,37,47]
             
usingMonadIf = [1..50] >>= (\x -> if '7' `elem` show x then [x] else []) -- [7,17,27,37,47]
usingFilter = filter (\x -> '7' `elem` show x) [1..50]-- [7,17,27,37,47]

-- ** A knight's quest
-- Determine if a knight with some initial position (c,r) can reach a certain position (c',r') in 3 moves
type KnightPos = (Int,Int)
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
                      guard (c' `elem` [1..8] && r' `elem` [1..8])
                      return (c',r')
move3 :: KnightPos -> [KnightPos]
move3 initPos = return initPos >>= moveKnight >>= moveKnight >>= moveKnight
canMoveIn3 :: KnightPos -> KnightPos -> Bool
canMoveIn3 initPos targetPos= targetPos `elem` (move3 initPos) 

moveKnightExample1 = (6,2) `canMoveIn3` (6,1) -- True
moveKnightExample2= (6,2) `canMoveIn3` (7,1) -- False

-- ***************************************************************

-- ** Monad laws
-- Just because something is made an instance of the Monad type class doesn't mean it's a monad, it has to abide by
-- Monad's laws. This can't be done automatically by haskell, we have to be sure that all is well for that type.

-- ** First Monad law: Left identity
-- Taking a value, putting it in a default context with return and then feeding it to a function using >>=, is the same as
-- just taking the value and applying the function to it.
-- return x >>= f ... is the same thing as ... f x


