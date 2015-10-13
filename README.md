# cis-194-homework
So the way I thought of to work is keeping all the homework files in
`src/CIS194/Template`, and for each person doing the homework, copying the files
in `Template` into their own directory under `CIS194` if they don't already
exist.  It shouldn't be hard to write a little script to do this.  Part of the
homework is writing tests so we can keep the `HW*Test.hs` files in `Template` as
well to be copied and filled in.

For each copied file you'll have to update the module name and any modules it
imports, like change `CIS194.Template.HW01` to `CIS194.Me.HW01`, for example.
The directory names do have to be capitalised, the error messages should make
things like this clear.  You'll also have to change `src/Main.hs` to run your
own tests from `Main`, but try not to commit that.

``` bash
$ mkdir src/CIS194/$ME
$ cp -nr src/CIS194/Template/* src/CIS194/$ME/
$ sed -i "s/Template/$ME/" $(find src/CIS194/$ME/ -type f)
$ git add src/CIS194/$ME
$ sed -i "s/Template/$ME/" src/Main.hs
```

Now you should be able to run the tests by going

``` bash
$ cabal sandbox init
$ cabal update
$ cabal install --dep
$ cabal repl
*Main> :main
```

assuming cabal and ghc are installed correctly.  If you have any problems
getting that far hit me (Ben) up for help :)

## Resources
The lectures make reference to various chapters in the book Real World Haskell,
which is available [free, online](http://book.realworldhaskell.org/).  There are
also links in the lectures themselves.  Stephen Diehl's page [What I Wish I Knew
When Learning Haskell](http://dev.stephendiehl.com/hask/) covers a huge range of
topics, a lot of them practical and is very useful.  Chris Allen's [Learn
Haskell](https://github.com/bitemyapp/learnhaskell) is also very extensive and
has a lot of excellent links.
