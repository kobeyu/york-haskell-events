% Non-strictness and I/O
% JMCT
% March 26th 2015

Overview
--------

Things we're going to talk about:

- Non-strictness?
- Why tutorials focus on pure functions, not I/O

What is non-strictness
----------------------

You may of heard that Haskell is *lazy*, but really it's defined as *non-strict*

What's App
----------

Strict evaluation:

```
    fst (a, b) = a

    loop = loop + 1

    answer = fst (42, loop)
           = fst (42, loop + 1)
           = fst (42, (loop + 1) + 1)
           = ...
```

What's App
----------

Non-strict evaluation:

```
    fst (a, b) = a

    loop = loop + 1

    answer = fst (42, loop)
           = 42
```

What's App
----------

A choice:

```
    square x = x * x

    square (1 + 2) = ?
```                     


What's App
----------

Non-strict:

```
    square x = x * x

    square (1 + 2) = (1 + 2) * (1 + 2)
```                     

What's App
----------

Non-strict + Sharing:

```
    square x = x * x

    square (1 + 2) = let x = (1 + 2)
                     in x * x
```                     

Lazy Data
---------

If you haven't already, clone the york-haskell `events` repo:

https://github.com/york-haskell/events.git 

Move into the mar-2015 directory and load Lazy.lhs in `ghci`

GHCi fun
--------

Two great commands:

```
    ghci> :p  <- print what is currently known about the value
    ghci> :sp <- same as above but no type info
    ghci> :p justin
    justin = (_t1::Maybe Integer)
    ghci> :sp justin
    justin = _
```
