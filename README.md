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
capable of building `rebar3`-based projects. While it used to offer unique 
features for the old Erlang TOML library, the newer tomerl library provides
very similar features. As such, this library has become little more than a
convenience wrapper for tomerl, with just a touch of sugar on top.

## Build [&#x219F;](#table-of-contents)

```shell
rebar3 compile
```

# Start the Project REPL [&#x219F;](#table-of-contents)

```shell
rebar3 repl
```

or for Erlang:

```shell
rebar3 shell
```

# Tests [&#x219F;](#table-of-contents)

```shell
rebar3 ltest
```

## Usage [&#x219F;](#table-of-contents)

Read a TOML file, parse its contents, and convert the parsed data to an Erlang
map:

```lisp
(set `#(ok ,data) (bombadil:read "priv/testing/deep-sections.toml"))
```

or from Erlang:

```erlang
1> {ok, Data} = bombadil:read("priv/testing/deep-sections.toml").
```

One may work with the resulting data structure in the following ways:

```lisp
lfe> (bombadil:get data "ab")
42
lfe> (bombadil:get-in data '("a" "b" "d" "e"))
#M("in" "deep")
lfe> (bombadil:assoc-in data '("a" "b" "d" "e") 42)
#M(#"ab" 42
   #"cd" 3.14159
   #"a" #M(#"stuff" #"things"
           #"b" #M(#"other" #"stuff"
                   #"c" #M(#"things" #"n stuff")
                   #"d" #M(#"things" #"n other things"
                           #"e" 42))
          #"f" #M(#"other" #"things"))
   #"g" #M(#"h" #M(#"woah" #"wut"))
   #"i" #M(#"j" #M(#"k" #M(#"no" #"way")))
   #"l" #M(#"stuff" #"fur reelies"))
```

or from Erlang:

```erlang
2> bombadil:get(Data, "ab").
42
3> bombadil:get_in(Data, ["a", "b", "d", "e"]).
#{<<"in">> => <<"deep">>}
4> bombadil:assoc_in(Data, ["a", "b", "d", "e"], 42).
#{<<"a">> =>
      #{<<"b">> =>
            #{<<"c">> => #{<<"things">> => <<"n stuff">>},
              <<"d">> =>
                  #{<<"e">> => 42,<<"things">> => <<"n other things">>},
              <<"other">> => <<"stuff">>},
        <<"f">> => #{<<"other">> => <<"things">>},
        <<"stuff">> => <<"things">>},
  <<"ab">> => 42,<<"cd">> => 3.14159,
  <<"g">> => #{<<"h">> => #{<<"woah">> => <<"wut">>}},
  <<"i">> =>
      #{<<"j">> => #{<<"k">> => #{<<"no">> => <<"way">>}}},
  <<"l">> => #{<<"stuff">> => <<"fur reelies">>}}
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

That last is, of course, a contrived example; those who have used the data threading
macros in the past are well familiar with the benefits (mostly in code clarity
and thus ease of debugging and maintenance).

Unfortunately this last LFE macro convenience is not usable from Erlang.

## License [&#x219F;](#table-of-contents)

Apache License, Version 2.0

Copyright © 2020-2023, Duncan McGreggor <oubiwann@gmail.com>.

[//]: ---Named-Links---

[logo]: priv/images/logo.png
[logo-large]: https://en.wikipedia.org/wiki/File:Gallen-Kallela_The_defence_of_the_Sampo.jpg
[gh-actions-badge]: https://github.com/lfex/bombadil/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfex/bombadil/actions
[lfe]: https://github.com/rvirding/lfe
[lfe-badge]: https://img.shields.io/badge/lfe-2.1-blue.svg
[erlang-badge]: https://img.shields.io/badge/erlang-20%20to%2025-blue.svg
[versions]: https://github.com/lfex/bombadil/blob/master/.github/workflows/cicd.yml
[github-tag]: https://github.com/lfex/bombadil/tags
[github-tag-badge]: https://img.shields.io/github/tag/lfex/bombadil.svg
