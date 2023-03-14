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
-- Just like with IO, do is used to glue several monadic actions in sequecne. Consider this:
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
        start <- return (0,0) -- start = Just (0,0)
        first <- landLeft 2 start -- first = Just (2,0)
        second <- landRight 3 first -- second = Just (2,3)
        third <- Nothing -- third = Nothing
        landLeft 1 second -- Nothing ... It doesn't matter whether we used third here or not, the 'Nothing' will propagate

birdPoleWithoutDo :: Maybe Pole  
birdPoleWithoutDo =   
    case Just (0,0) of   
        Nothing -> Nothing  
        Just start -> case landLeft 2 start of  
            Nothing -> Nothing  
            Just first -> case landRight 2 first of  
                Nothing -> Nothing  
                Just second -> landLeft 1 second 