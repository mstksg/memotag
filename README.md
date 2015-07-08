memotag
=======

Extremely small library offering "memoized function application tuples" with
restricted access using the lens library for convenient manipulation.  Doesn't
require the lens library to use --- feel free to use lens-family, etc.

Sometimes you want to work with a value as well as the result of a function
applied to the value; however, the function is pretty expensive, so you'd like
to also keep the result around memoized without much overhead.

A common example is sorting a list based on the results of a function, or any
time you will be comparing the results of a function application, but want to
keep the value itself as the main object.

Make a `MemoTag`:

~~~haskell
ghci> let mt = mkMemoTag sqrt 10
~~~

The `mtValue` lens is useful for "mapping" functions over the value, or for
extracting it:

~~~haskell
ghci> view mtValue mt
10
ghci> let mt' = over mtValue (*2) mt
ghci> view mtValue mt'
20
ghci> let mt'' = set mtValue 30 mt
ghci> view mtValue mt''
30
~~~

The neat thing about `over` and using `mtValue` is that it "auto-updates" the
memoized result for you; it applies the result as you map it.  We can see this
using the `mtResult` getter:

~~~haskell
ghci> view mtResult mt
3.162278            -- square root of 10
ghci> view mtResult mt'
4.472136            -- square root of 20
ghci> view mtResult mt''
5.477226            -- square root of 30
~~~

The "result" field is auto-updated when you use `over` and `set`.

You can also map over the function with `mtFunc`, which will also
auto-update the memoizing field.

Because `mtValue` is a Traversal, you can even have your "mapping
function" do arbitrary side effects in a monad, like `traverse`:

~~~haskel
ghci> let x = mkMemoTag (^2) 3
ghci> x' <- forOf mtValue x $ \y -> do print y
                                       readLn
3           -- stdout output
> 10        -- prompted input with readLn
ghci> view mtValue x'
10
ghci> view mtResult x'
100
ghci> view mtTuple x'
(10, 100)
~~~

Here, we applied a monadic function to update `mtValue`.


Here is a simple example for sorting based on two different things, using the
`Ord` instance:

~~~haskell
ghci> let xs = [1..10]
ghci> let xs' = map (mkMemoTag sin) xs
ghci> map (view mtValue) . sort $ xs'
[5,4,10,6,3,9,7,1,2,8]
ghci> let xs'' = map (over mtValue negate) xs'
ghci> map (view mtValue) . sort $ xs''
[-8,-2,-1,-7,-9,-3,-6,-10,-4,-5]
~~~

One main power of the lensy interface is both the unified power of using
`over` (for "mapping"), `view` (for "getting"), etc., under one interface, and
also to be able to "compose" lenses and getters using `(.)`:

~~~haskell
ghci> let mt = mkMemoTag (uncurry (+)) (1,3)
ghci> let mt' = over (mtValue . _2) (*4) mt
ghci> view mtValue mt'
(1, 12)
ghci> view mtResult mt'
13
~~~


If you don't want to use `view`/`over`, and don't want to compose lenses and
getters with `(.)`, there's also direct extractors (`_mtValue`, `_mtResult`)
and the `Comonad` and `ComonadStore` interfaces:

~~~haskell
extract = view mtResult
pos     = view mtValue
seek v  = set mtValue v
seeks v = over mtValue f
peek v  = view mtResult . set mtValue v
peeks f = view mtResult . over mtValue f
~~~

So the above example could have been `seeks (*2)` instead of `over mtValue
(*2)`.

All of the `ComonadStore` interface functions also auto-update the memoizing
field.



