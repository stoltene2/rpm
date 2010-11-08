{-| 
  'System.Rpm.Combinators' provides a mechanism for comparing
  attributes of an Rpm against some sort of specification.
-}

module System.Rpm.Combinators 
    (
      Rpm(..)
    , RpmP
    , zeroP
    , oneP
    , notP
    , (.==.)
    , (./=.)
    , (.<=.)
    , (.<.)
    , (.>=.)
    , (.>.)
    , (.&&.)
    , (.||.)
    -- * Example: Common Uses
    -- $exampleOfUse
    ) where

import System.Rpm.Info (RpmInfo(..))

type Rpm a = RpmInfo -> a


{-| 
A predicate type that takes an 'RpmInfo' datatype and returns a
value of Bool depending on its evaluation.
-}
type RpmP = Rpm Bool


{-|
A standard lifting function.
-}
liftP :: (a -> a -> b) -- ^ The function to lift
      -> Rpm a         -- ^ The field to extract from 'RpmInfo'
      -> a             -- ^ Passed to the Second argument of the function
      -> Rpm b         -- ^ The result wrapped in 'Rpm'

liftP f g val rpmInfo= g rpmInfo `f` val


{-|
A more conventional lifting function.
-}
liftP2 :: (a -> b -> c) 
       -> Rpm a 
       -> Rpm b 
       -> Rpm c

liftP2 f g h rpmInfo = g rpmInfo `f` h rpmInfo


{-| 
The 'zeroP' combinator, much like the 'oneP' combinator, is more
for completeness than anything.
-}
zeroP :: RpmP  -- ^ The constant combinator that always returns
               -- 'False' when run.
zeroP _ = False


oneP :: RpmP  -- ^ The constant combinator that always returns 'False'
              -- when run.
oneP _ = True


(.==.) :: Eq a => Rpm a  -- ^ Some field of 'RpmInfo'
       -> a              -- ^ The value to check equality against
       -> RpmP           
(.==.) = liftP (==)


(./=.) :: Eq a => Rpm a -> a -> RpmP
(./=.) = liftP (/=)


(.<=.) :: Ord a => Rpm a -> a -> RpmP
(.<=.) = liftP (<=)


(.<.) :: Ord a => Rpm a -> a -> RpmP
(.<.) = liftP (<)


(.>=.) :: Ord a => Rpm a -> a -> RpmP
(.>=.) = liftP (>=)


(.>.) :: Ord a => Rpm a -> a -> RpmP
(.>.) = liftP (>)


{-|
@notP@ is used to negate its 'RpmP' when run.
-}
notP :: RpmP -- ^ Predicate to negate
     -> RpmP -- ^ Negated predicate
notP p =  not . p 


{-| 
This is a logical combinator used for constructing more complex
sequences of combinators by requiring both predicates to be true.
-}
(.&&.) :: RpmP 
       -> RpmP 
       -> RpmP
(.&&.) = liftP2 (&&)


{-| 
This is a logical combinator used for constructing more complex
sequences of combinators by requiring one of the predicates to be true.
-}
(.||.) :: RpmP 
       -> RpmP 
       -> RpmP
(.||.) = liftP2 (||)

{-| $exampleOfUse
Example of use.

> main :: IO ()
> main = putStrLn "Hello"

-}

-- nameMatchesFilename :: RpmP
-- nameMatchesFilename rpmInfo = fileName .==. (name rpmInfo)

-- Then, to find where name doesn't match filename,
-- findAll (.!. nameMatchesFilename)
-- or, assertAll (nameMatchesFilename)
-- or, assertAll (buildHost .==. "my1.machine.org" .||. buildHost .==. "my2.machine.org")

-- Conditional predicate generation
-- ifThenElse :: RpmP -> RpmP -> RpmP
-- ifThenElse ( file `matchesP` "foo*" )
--            ( size .<=. 1234)


-- assertEqual .!. (.!. zeroP) == zeroP
-- assertEqual (.!. zeroP) == oneP


