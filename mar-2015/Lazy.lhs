> module Lazy where

Note: No GHC extensions! This is pure Haskell, no funny business.

Also, if you skip ahead and print values in ghci you'll spoil the fun!
Bear with me, I think you'll be happy you did.

Let's start simple. We have a data-type `Maybe` that
has two constructors

```
    data Maybe a = Just a
                 | Nothing
```

By default, all constructor fields in Haskell are non-strict.
This means that they do not evaluate their arguments
unless they are needed (just like functions don't).


Here the use of `id` means we have to demand the result
of `justin` in order to 'see' the `Just`.


> justin = id $ Just (5 * 2)
>
> justout = badId $ Just (5 * 2)
>
> badId x = error "You forced me to!"

Try:

```
    ghci> :sp justin
    ghci> :sp justout
```

Both values are thunks, waiting to be needed.

> isJust Nothing  = False
> isJust (Just x) = True

Try:

```
    ghci> isJust justin
    ghci> isJust justout
    ghci> :sp justin
    ghci> :sp justout
```

Does any of that surprise you? `isJust` forces the result of `justin` but *only
as much as necessary*. In this case, that's only the outermost constructor
`Just`.

> oneTen     = [1..10]
> oneTenPlus = map (+1) [1..10]

Try:

```
    ghci> :sp oneTen
    ghci> length oneTen
    ghci> :sp oneTen
    ghci> :sp oneTenPlus
    ghci> length oneTenPlus
    ghci> :sp oneTenPlus
```

Here's one that may surprise you:

> ice = take 10 $ map (++ "!") ["ice", "fozen water", "vanilla", "hielo"]
> justice = Just ice

Think about what you expect before you run each command:

```
    ghci> :sp justice
    ghci> take 8 $ show justice
    ghci> :sp justice
    ghci> fmap length justice
    ghci> :sp justice
```
