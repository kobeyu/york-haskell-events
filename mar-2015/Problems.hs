module Problems where
import Control.Applicative

--------------------------------------------------------------------------------
-- * I/O
-- In this section, we're going to build up a model of I/O in regular
-- pure Haskell code!

-- | A type for the I/O operations we want to be able to model.
data IOAction a
  = Pure a
  -- ^ A pure value which has been "put in to" the 'IOAction' type.
  | WriteLn String (IOAction a)
  -- ^ Writing a line of text to stdout is defined up as the string we
  -- want to write, followed by an 'IOAction' to then perform.
  | ReadLn (String -> IOAction a)
  -- ^ Reading a line of text from stdin is defined as a function
  -- which takes a string and produces another 'IOAction'.

-- | Lift a value into an 'IOAction'.
lift :: a -> IOAction a
lift = Pure

-- | Write a string to stdout.
writeln :: String -> IOAction ()
writeln s = WriteLn s (Pure ())

-- | Read a line from stdin.
readln :: IOAction String
readln = ReadLn Pure

-- ** Transforming IO values

-- | Apply a function to the result of an 'IOAction'.
--
-- For this function to be \"sensible\", there are some common-sense
-- properties it should obey:
--
-- * @iomap id == id@ - mapping the identity function shouldn't change
--   anything.
--
-- * @iomap (g . h) = iomap g . iomap h@ - mapping two functions \"at
--   the same time\" is the same as mapping them \"one after the
--   other\" (in the right order).
iomap :: (a -> b) -> IOAction a -> IOAction b
iomap f (Pure    a)    = Pure (f a)
iomap f (WriteLn s io) = undefined
iomap f (ReadLn  cio)  = undefined

-- | Apply a function wrapped up in an 'IOAction' to a value also
-- wrapped up in an 'IOAction'. The type of this may look a bit scary,
-- but it lets us generate functions in IO and do things with them.
--
-- There are again some common-sense properties:
--
-- * @ioap (lift id) v == v@ - applying the (pure) identity function
--   does nothing.
--
-- * @ioap (lift f) (lift x) == lift (f x)@ - applying a pure functon
--   to a pure value is the same as applying the function to the
--   value.
ioap :: IOAction (a -> b) -> IOAction a -> IOAction b
ioap (Pure f) (Pure a) = Pure (f a)
ioap f a = undefined

-- | Plug an 'IOAction' value into an 'IOAction' function. This isn't
-- the same as 'ioap', try expressing this with only what you have
-- defined so far and try to see why.
--
-- Finally, we have some more properties, this time formulated in
-- terms of a helper function, 'iocmp':
--
-- * @iocmp lift f == f@ and @iocmp f lift == f@ - 'lift' is the
--   identity of 'iocmp'.
--
-- * @iocmp (iocmp f g) h == iocmp f (iocmp g h)@ - 'iocmp' is
--   associative.
ioandthen :: IOAction a -> (a -> IOAction b) -> IOAction b
ioandthen ioa f = undefined

-- | Compose two 'IOAction'y functions, using 'ioandthen'.
iocmp :: (a -> IOAction b) -> (b -> IOAction c) -> a -> IOAction c
iocmp f g a = ioandthen (f a) g

--------------------------------------------------------------------------------
-- * Advanced!
-- Write an interpreter of type IOAction a -> IO a
-- so that your actions get run in the 'real' world

-- | Run an IOAction in the real world
interp :: IOAction a -> IO a
interp = undefined

-- Now we can do something exciting: pipe together multiple actions with
-- the 'IOAction' type and it'll all work!
testIOAction :: IOAction ()
testIOAction = writeln "Hello, what is your name?" `ioandthen` (\_ ->
               readln                              `ioandthen` (\name ->
               writeln ("Nice to meet you, " ++ name)))

-- Now try @interp testIOAction@!

-- This section taken from <http://chris-taylor.github.io/blog/2013/02/09/io-is-not-a-side-effect/>
