# cis-194-homework

The organisation was a bit messy last time, so this time I thought using
branches more would keep things tidy.  I'll put template sources in
`2016/src/CIS194/WeekN.hs` on master and everyone can pull the new templates into
their own branches and just never merge into master.

To get a working Haskell system, download or install `stack`
(`https://docs.haskellstack.org/`) and run the following:

```
$ git clone git@github.com:ROKT-CIS-194/cis-194-homework.git
$ cd cis-194-homework/2016/
cis-194-homework/2016 $ stack setup
... possibly downloading and installing GHC

cis-194-homework/2016 $ stack repl
... installing library dependencies on the first run, it can take a while
Configuring GHCi with the following packages: cis194
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Ok, modules loaded: none.
Main> :main
```

The course is based on CodeWorld which has its own Haskell library, the API is
published at https://hackage.haskell.org/package/codeworld-api.  When it's run
it opens a socket on localhost and serves images to your browser, after changing
the source file you'll need to stop the server (C-c C-c in emacs), reload the
source file (`:r`), and run it again (`:main` or similar).

## Resources
The lectures make reference to various chapters in the book Real World Haskell,
which is available [free, online](http://book.realworldhaskell.org/).  There are
also links in the lectures themselves.  Stephen Diehl's page [What I Wish I Knew
When Learning Haskell](http://dev.stephendiehl.com/hask/) covers a huge range of
topics, a lot of them practical and is very useful.  Chris Allen's [Learn
Haskell](https://github.com/bitemyapp/learnhaskell) is also very extensive and
has a lot of excellent links.
