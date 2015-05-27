% Type-system Extensions
% Michael Walker
% May 2015

# Language Extensions

Can be enabled by passing GHC an option when compiling:

~~~~{.bash}
ghc -XPolyKinds ...
~~~~

Or by adding a LANGUAGE pragma to the top of a source file:

~~~~{.haskell}
{-# LANGUAGE PolyKinds #-}
module Foo where
...
~~~~

Best practice is to use LANGUAGE pragmas.

# `{-# LANGUAGE ScopedTypeVariables #-}`

Let's write a function to append a value to a list:

~~~~{.haskell}
append :: [a] -> a -> [a]
append xs x = xs ++ [x]
~~~~

Does this version also look right?

~~~~{.haskell}
append :: [a] -> a -> [a]
append xs x = xs ++ [x :: a]
~~~~

---

~~~~
Couldn't match expected type `a1' with actual type `a'
  `a1' is a rigid type variable bound by
    an expression type signature: a1 at foo.hs:13:26
  `a' is a rigid type variable bound by
    the type signature for append :: [a] -> a -> [a]
    at foo.hs:12:15
  In the expression: x :: a
  In the second argument of `(++)', namely `[x :: a]'
  In the expression: xs ++ [x :: a]
~~~~

Oh dear!

The problem is that the "`a`"s in the signature for `append` and the
"`a`" in the signature for `x` are *not the same*!

---

`ScopedTypeVariables` gives type variables scope over the body of the
definition they are typing, meaning we can do this:

~~~~{.haskell}
append :: forall a. [a] -> a -> [a]
append xs x = xs ++ [x :: a]
~~~~

The `forall` is necessary.

# `{-# LANGUAGE ExistentialQuantification #-}`

~~~~{.haskell}
doWork :: IO (MVar Int)
doWork = ...
~~~~

But we really want a function like this:

~~~~{.haskell}
doWorkInteger :: IO (MVar Integer)
doWorkInteger = ...
~~~~

The obvious implementation is this:

~~~~{.haskell}
doWorkInteger = do
  mvar <- doWork
  modifyMVar mvar fromIntegral
  return mvar
~~~~

But wait! `modifyMVar` blocks!

---

~~~~{.haskell}
data Result x a = Result (MVar x) (x -> a)

readResult :: Result x a -> IO a
readResult (Result mvar f) = f <$> readMVar mvar

doWorkInteger = IO (Result Int Integer)
doWorkInteger = do
  mvar <- doWork
  return $ Result mvar fromIntegral
~~~~

This behaves as we want, but we have that `Int` leaking out. We don't
*care* what the original type is, only what the final type is!

---

~~~~{.haskell}
data Result a = forall x. Result (MVar x) (x -> a)

readResult :: Result a -> IO a
readResult (Result mvar f) = f <$> readMVar mvar

doWorkInteger = IO (Result Integer)
doWorkInteger = do
  mvar <- doWork
  return $ Result mvar fromIntegral
~~~~

Now the *only* thing we can do with the value in the `MVar` is apply
the function to it, because we *don't know what type it is*!

# `{-# LANGUAGE RankNTypes #-}`

Let's write a silly function:

~~~~{.haskell}
choose :: Bool -> a -> b -> Either a b
choose True  a _ = Left  a
choose False _ b = Right b
~~~~

---

Now let's do nothing to the component values:

~~~~{.haskell}
choose :: Bool -> a -> b -> Either a b
choose True  a _ = Left  $ id a
choose False _ b = Right $ id b
~~~~

---

Now let's be polymorphic over the `id`:

~~~~{.haskell}
choose :: Bool -> a -> b -> Either a b
choose = choose' id

choose' :: ??? -> Bool -> a -> b -> Either a b
choose' f True  a _ = Left  $ f a
choose' f False _ b = Right $ f b
~~~~

Stop! What's the type of `f`?

---

~~~~{.haskell}
-- Can't be unified with the 'b':
choose' :: Bool -> (a -> a) -> a -> b -> Either a b

-- Can't be unified with the 'a':
choose' :: Bool -> (b -> b) -> a -> b -> Either a b

-- Can't be unified with either:
choose' :: Bool -> (c -> c) -> a -> b -> Either a b
~~~~

We *cannot* give it a type with Haskell's regular type system. It's
just not polymorphic enough!

---

So we need the `(c -> c)` function to work for *all* possible types,
not just *one* caller-determined type. We need this:

~~~~{.haskell}
choose' :: Bool -> (forall c. c -> c) -> a -> b
        -> Either a b
~~~~

# `{-# LANGUAGE MultiParamTypeClasses #-}`

~~~~{.haskell}
class Store m v where
  new   :: a -> m (v a)
  read  :: v a -> m a
  write :: v a -> a -> m ()

instance Store IO IORef where
  new   = newIORef   :: a -> IO (IORef a)
  read  = readIORef  :: IORef a -> IO a
  write = writeIORef :: IORef a -> a -> IO ()

instance Store (ST t) (STRef t) where
  new   = newSTRef   :: a -> ST t (STRef t a)
  read  = readSTRef  :: STRef t a -> ST t a
  write = writeSTRef :: STRef t a -> a -> ST t ()
~~~~

---

## `{-# LANGUAGE FunctionalDependencies #-}`

Multi-parameter type classes are bad for type inference. In the prior
example, we had an instance for `Store` using `IO` and `IORef`, but
there's nothing to say we couldn't have an instance using `IO` and
`MVar`.

Knowing one parameter doesn't help you with the others.

~~~~{.haskell}
class Store m v | m -> v where
~~~~

We can specify that the `m` type determines the `v` type. Now if we we
have a `Store IO v`, then we know that the `v` must be `IORef`.

---

## `{-# LANGUAGE TypeFamilies #-}`

Another solution is to drop the multi-parameter type class entirely:

~~~~{.haskell}
class Store m where
  type Var :: * -> *

  new   :: a -> m (Var m a)
  read  :: Var m a -> m a
  write :: Var m a -> a -> m ()

instance Store IO where
  type Var = IORef
  ...
~~~~

We're back to a single parameter, where the `Var` is determined
entirely by the monad, so type inference is saved.

# Further Reading

The GHC User's Guide: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/
