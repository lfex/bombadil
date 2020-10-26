(defmodule bombadil-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun test-path () "")

(deftest toml->map
  (let ((data (bombadil:read "priv/testing/metadata.toml")))
    (is-equal "f08dd8c3-c1c2-32cb-802e-d5cd13a62e4f"
              (mref data "id"))
    (is-equal "The World under the Wood"
              (mref data "tagline"))
    (is-equal '("story" "pve" "rpg")
              (mref data "game-types"))
    (is-equal 'true
              (mref data "has-factions"))
    (is-equal "boreal waterways"
              (clj:get-in data '("faction" "birch" "biome")))
    (is-equal '("cold" "disease")
              (clj:get-in data '("faction" "connihyde" "resistance")))
    (is-equal "Hyperborea"
              (clj:get-in data '("faction" "lichen" "display-name")))))

(deftest toml->map-deeply-nested
  (let ((data (bombadil:read "priv/testing/deep-sections.toml")))
    (is-equal 42
              (mref data "ab"))
    (is-equal 3.14159
              (mref data "cd"))
    (is-equal "things"
              (clj:get-in data '("a" "stuff")))
    (is-equal "stuff"
              (clj:get-in data '("a" "b" "other")))
    (is-equal "n stuff"
              (clj:get-in data '("a" "b" "c" "things")))
    (is-equal "n other things"
              (clj:get-in data '("a" "b" "d" "things")))
    (is-equal "deep"
              (clj:get-in data '("a" "b" "d" "e" "in")))
    (is-equal "things"
              (clj:get-in data '("a" "f" "other")))
    (is-equal "wut"
              (clj:get-in data '("g" "h" "woah")))
    (is-equal "way"
              (clj:get-in data '("i" "j" "k" "no")))
    (is-equal "fur reelies"
              (clj:get-in data '("l" "stuff")))))
