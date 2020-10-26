# bombadil

*A wrapper for the [Erlang toml library](https://github.com/dozzie/toml) that uses the map data structure*

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe-badge]][lfe]
[![Erlang Versions][erlang-badge]][versions]
[![Tag][github-tag-badge]][github-tag]

[![Project Logo][logo]][logo-large]

##### Table of Contents

* [About](#about-)
* [Build](#build-)
* [Start the Project REPL](#start-the-repl-)
* [Tests](#tests-)
* [Usage](#usage-)
* [License](#license-)

## About [&#x219F;](#table-of-contents)

This is an LFE TOML library 100% usable in any other BEAM language that is
capable of building `rebar3`-based projects. It does a couple of useful and
convenient things:

1. Provides a `read` function that recursively converts all of the `toml` 
   Erlang `dict`s to maps and converts the `toml` library's type tuples to
   just the values themselves.
1. Provides Clojure-inspired functions for accessing and updating map data
   (`assoc`, `assoc-in`, `get-in`). Note however that, due to the fact that
   Erlang and LFE, unlike Clojure, do not support arbitrary arity in function
   definitions, the signature and use of some of these functions is different
   than the similar ones in Clojure.
1. Provides a [set of functions](https://github.com/lfex/bombadil/blob/master/src/bombadil.lfe#L10)
   in the `bombadil` module which just call the
   [same functions](http://dozzie.jarowit.net/api/erlang-toml/default/toml.html#index)
   in the `toml` library. Note that these aliases have been Lispified
   (underscores to dashes); for Erlangers we've supplied aliases with
   underscores.
1. Lastly, makes public the utility functions used for the conversion from
   `toml` dicts to `bombadil` maps, in the event that these are useful for
   your own project (or debugging).

## Build [&#x219F;](#table-of-contents)

```shell
$ rebar3 compile
```

# Start the Project REPL [&#x219F;](#table-of-contents)

```shell
$ rebar3 repl
```

or for Erlang:

```shell
$ rebar3 shell
```

# Tests [&#x219F;](#table-of-contents)

```shell
$ rebar3 ltest
```

## Usage [&#x219F;](#table-of-contents)

Read a TOML file, parse its contents, and convert the parsed data to an Erlang
map:

```lisp
(set data (bombadil:read "priv/testing/deep-sections.toml"))
```

or from Erlang:

```erlang
1> Data = bombadil:read("priv/testing/deep-sections.toml").
```

One may work with the resulting data structure in the following ways:

```lisp
lfe> (bombadil:get data "ab")
42
lfe> (bombadil:get-in data '("a" "b" "d" "e"))
#M("in" "deep")
lfe> (bombadil:assoc-in data '("a" "b" "d" "e") 42)
#M("ab" 42
   "cd" 3.14159
   "a" #M("stuff" "things"
          "b" #M("other" "stuff"
                 "c" #M("things" "n stuff")
                 "d" #M("things" "n other things"
                        "e" 42))
        "f" #M("other" "things"))
   "g" #M("h" #M("woah" "wut"))
   "i" #M("j" #M("k" #M("no" "way")))
   "l" #M("stuff" "fur reelies"))
```

or from Erlang:

```erlang
2> bombadil:get(Data, "ab").
42
3> bombadil:get_in(Data, ["a", "b", "d", "e"]).
#{"in" => "deep"}
4> bombadil:assoc_in(Data, ["a", "b", "d", "e"], 42).
#{"ab" => 42,
  "cd" => 3.14159,
  "a" => #{"stuff" => "things"
           "b" => #{"other" => "stuff"
                    "c" => #{"things" => "n stuff"},
                    "d" => #{"things" => "n other things"
                             "e" => 42,}},
           "f" => #{"other" => "things"}},
  "g" => #{"h" => #{"woah" => "wut"}},
  "i" => #{"j" => #{"k" => #{"no" => "way"}}},
  "l" => #{"stuff" => "fur reelies"}}
```

Note that since all of these functions have `data` as their first argument, the
"thread-left" macro in LFE -- `->` -- may be used to chain outputs:

```lisp
lfe> (include-lib "lfe/include/clj.lfe")
lfe> (-> data
         (bombadil:assoc-in '("a" "b" "d" "e") 42)
         (bombadil:get-in '("a" "b" "d" "e")))
42
```

This is, of course, a contrived example; those who have used the data threading
macros in the past are well familiar with the benefits (mostly in code clarity
and thus ease of debugging and maintenance).

Unfortunately this LFE macro convenience is not usable from Erlang.

## License [&#x219F;](#table-of-contents)

Apache License, Version 2.0

Copyright © 2020, Duncan McGreggor <oubiwann@gmail.com>.

<!-- Named page links below: /-->

[logo]: priv/images/logo.png
[logo-large]: https://en.wikipedia.org/wiki/File:Gallen-Kallela_The_defence_of_the_Sampo.jpg
[github]: https://github.com/lfex/bombadil
[gitlab]: https://gitlab.com/lfex/bombadil
[gh-actions-badge]: https://github.com/lfex/bombadil/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfex/bombadil/actions
[lfe]: https://github.com/rvirding/lfe
[lfe-badge]: https://img.shields.io/badge/lfe-2.0-blue.svg
[erlang-badge]: https://img.shields.io/badge/erlang-19%20to%2023-blue.svg
[versions]: https://github.com/lfex/bombadil/blob/master/.github/workflows/cicd.yml
[github-tag]: https://github.com/lfex/bombadil/tags
[github-tag-badge]: https://img.shields.io/github/tag/lfex/bombadil.svg
[github-downloads]: https://img.shields.io/github/downloads/lfex/bombadil/total.svg
