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
capable of building `rebar3`-based projects.


## Build [&#x219F;](#table-of-contents)

```shell
$ rebar3 compile
```

# Start the Project REPL [&#x219F;](#table-of-contents)

```shell
$ rebar3 repl
```

# Tests [&#x219F;](#table-of-contents)

```shell
$ rebar3 as test ltest
```

## Usage [&#x219F;](#table-of-contents)

Read a TOML file, parse its contents, and convert the parsed data to an Erlang
map:

```lisp
(set data (bombadil:read "priv/testing/deep-sections.toml"))
```

or from Erlang:

```erlang
Data = bombadil:read("priv/testing/deep-sections.toml").
```

The resulting data structure's elements may then be acceessed via the usual
suspects:

```lisp
(mref data "ab")
42
(clj:get-in data '("a" "b" "d" "e"))
#M("in" "deep")
```

or from Erlang:

```erlang
maps:get("ab", Data).
42
clj:'get-in'(Data, ["a", "b", "d", "e"]).
#{"in" => "deep"}
```

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
