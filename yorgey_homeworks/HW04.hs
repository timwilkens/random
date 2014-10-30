module HW04 where

ex1 :: a -> b -> b
ex1 x y = y
{- This function must immediately return its second argument,
   We can glean no further information about the type of b.
   Analogous to the type signature 'a -> a'. -}

ex2 :: a -> a -> a
ex2 x y = x
{- One of the two input arguments must be returned.
   without transformation. Without type constraints
   I'm not sure anything can be done to the arguments,
   or if any decisions can even be made based on their
   content. -}

ex3 :: Int -> a -> a
ex3 _ x = x
{- This function, too, just returns the second argument,
   and ignores the first. Without knowing the type of a,
   we can't use the Int in any interesting way. -}

ex4 :: Bool -> a -> a -> a
ex4 True x _ = x
ex4 False _ y = y
{- This implementation is the ternary operator
   found in C and Perl (and others probably).
   There are six possible functions that can have
   this function signature. Each function must have
   two distinct cases for True and False. Each of these
   two cases has two possible returns: argument 1 or
   argument 2. 2 cases * 2 possibilites = 4 functions.
   A function could also completely ignore the Boolean
   value and just return the two possibilies which gives
   use 4 + 2 = 6. -}

ex5 :: Bool -> Bool
ex5 True = False
ex5 False = True
{- There are four functions that satisfy this type signature,
   A function could:
   1. flip the argument values as above
   2. return each untouched
   3. always return True
   4. always return False -}

ex6 :: (a -> a) -> a
ex6 = error "impossible"
{- I want to say this is impossible although I can see that
   the 'fix' function in Data.Function has this signature.
   We get as our first argument a function which takes an a
   and returns an a. But, we don't know the type of a nor do 
   we actually get a value of type a. So, there is no argument
   to actually apply this function to and get a value of type a
   to return. -}

ex7 :: (a -> a) -> a -> a
ex7 f x = f x
{- There a wide variety of things this function could actually
   do given the function argument. The simplest is just to apply
   the function to the single argument we get. The result is
   application of either a unary function or a partially applied one. -}

ex8 :: [a] -> [a]
ex8 xs = xs ++ xs ++ xs
{- This could do a wide variety of things with the elements that are
   already in the list. It could replicate the list and create a list
   twice as long, or repeat the first element a million times. My gut
   feeling is that there are an infinite number of implementations
   that satisfy this type signature -}

ex9 :: (a -> b) -> [a] -> [b]
ex9 f [] = []
ex9 f (x:_) = [f x]
{- This also has a probably infinite number of concrete implementations.
   This function has the signature of map: take the elements of a list,
   apply some transform to them, and return a new list. The new list 
   COULD be of the same type, but doesn't have to be. Our example is
   particularly useless and only returns the first element modified,
   or an empty list -}

ex10 :: Maybe a -> a
ex10 Nothing = error "impossible"
ex10 (Just a) = a
{- Maybe has two data constructors, Nothing and Just a. If we receive a
   value constructed with Just a, we can safely return the value associated
   with it. But, if our argument was constructed with Nothing, we have no
   value of type a to return. -}

ex11 :: a -> Maybe a
ex11 x = Just x
{- This function can only have two implementations. It can either return nothing
   or it can return Just a as it does above. -}

ex12 :: Maybe a -> Maybe a
ex12 Nothing = Nothing
ex12 Just x = Nothing
{- There are a couple functions that can satisfy this type signature. One is to force
   all arguments to Nothing and the other is identity. Importantly, no argument
   constructed with Nothing can be 'promoted' to Just a. We won't know the type of
   a so won't be able ot provide a default value. -}
