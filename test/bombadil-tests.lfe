(defmodule bombadil-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest toml->map
  (let ((`#(ok ,data) (bombadil:read "priv/testing/metadata.toml")))
    (is-equal #"f08dd8c3-c1c2-32cb-802e-d5cd13a62e4f"
              (bombadil:get data "id"))
    (is-equal #"The World under the Wood"
              (bombadil:get data "tagline"))
    (is-equal '(#"story" #"pve" #"rpg")
              (bombadil:get data "game-types"))
    (is-equal 'true
              (bombadil:get data "has-factions"))
    (is-equal #"boreal waterways"
              (bombadil:get-in data '("faction" "birch" "biome")))
    (is-equal '(#"cold" #"disease")
              (bombadil:get-in data '("faction" "connihyde" "resistance")))
    (is-equal #"Hyperborea"
              (bombadil:get-in data '("faction" "lichen" "display-name")))))

(deftest toml->map-deeply-nested
  (let ((`#(ok ,data) (bombadil:read "priv/testing/deep-sections.toml")))
    (is-equal 42
              (bombadil:get data "ab"))
    (is-equal 3.14159
              (bombadil:get data "cd"))
    (is-equal #"things"
              (bombadil:get-in data '("a" "stuff")))
    (is-equal #"stuff"
              (bombadil:get-in data '("a" "b" "other")))
    (is-equal #"n stuff"
              (bombadil:get-in data '("a" "b" "c" "things")))
    (is-equal #"n other things"
              (bombadil:get-in data '("a" "b" "d" "things")))
    (is-equal #"deep"
              (bombadil:get-in data '("a" "b" "d" "e" "in")))
    (is-equal #"things"
              (bombadil:get-in data '("a" "f" "other")))
    (is-equal #"wut"
              (bombadil:get-in data '("g" "h" "woah")))
    (is-equal #"way"
              (bombadil:get-in data '("i" "j" "k" "no")))
    (is-equal #"fur reelies"
              (bombadil:get-in data '("l" "stuff")))))

(deftest assoc-in
  (let* ((`#(ok ,data) (bombadil:read "priv/testing/deep-sections.toml"))
         (result (bombadil:assoc-in data
                                    '("a" "b" "d" "e")
                                    (bombadil:get data "ab"))))
    (is-equal
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
     result)))
    