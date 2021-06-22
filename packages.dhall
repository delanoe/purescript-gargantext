let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.2-20210613/packages.dhall sha256:64d7b5a1921e8458589add8a1499a1c82168e726a87fc4f958b3f8760cca2efe

let overrides =
      { globals =
        { dependencies = [ "functions", "maybe" ]
        , repo = "https://github.com/purescript/purescript-globals"
        , version = "v4.1.0"
        }
      , smolder =
        { dependencies = [
            "bifunctors"
          , "catenable-lists"
          , "free"
          , "ordered-collections"
          , "prelude"
          , "strings"
          , "test-unit"
          , "transformers"
          , "tuples"]
        , repo = "https://github.com/bodil/purescript-smolder"
        , version = "v12.3.0"
        }
      }

let additions =
      { sequences =
        { dependencies =
          [ "prelude"
          , "unsafe-coerce"
          , "partial"
          , "unfoldable"
          , "lazy"
          , "arrays"
          , "profunctor"
          , "maybe"
          , "tuples"
          , "newtype"
          ]
        , repo = "https://github.com/hdgarrood/purescript-sequences.git"
        , version = "v2.1.0"
        }
      , spec-discovery =
        { dependencies = [ "prelude", "effect", "arrays", "spec", "node-fs" ]
        , repo = "https://github.com/purescript-spec/purescript-spec-discovery"
        , version = "v4.0.0"
        }
      , spec-quickcheck =
        { dependencies = [ "prelude", "aff", "random", "quickcheck", "spec" ]
        , repo = "https://github.com/purescript-spec/purescript-spec-quickcheck"
        , version = "v3.1.0"
        }
      , ffi-simple =
        { dependencies =
          [ "prelude"
          , "effect"
          , "maybe"
          , "functions"
          , "nullable"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/irresponsible/purescript-ffi-simple"
        , version = "v0.2.10"
        }
      , dom-simple =
        { dependencies =
          [ "arrays"
          , "console"
          , "effect"
          , "ffi-simple"
          , "functions"
          , "nullable"
          , "prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/irresponsible/purescript-dom-simple"
        , version = "v0.2.7"
        }
      , dom-filereader =
        { dependencies = [ "aff", "arraybuffer-types", "web-file", "web-html" ]
        , repo = "https://github.com/nwolverson/purescript-dom-filereader"
        , version = "v5.0.0"
        }
      , formula = 
        { dependencies = [ "effect", "prelude", "reactix", "record", "toestand", "tuples", "typelevel-prelude", "typisch" ]
        , repo = "https://github.com/poorscript/purescript-formula"
        , version = "v0.2.1"
        }
      , markdown =
        { dependencies = [ "precise" ]
        , repo = "https://github.com/poorscript/purescript-markdown"
        , version = "2021-06-22"
        }
      , markdown-smolder =
        { dependencies = [ "markdown", "smolder" ]
        , repo = "https://github.com/poorscript/purescript-markdown-smolder"
        , version = "2021-06-22"
        }
      , precise =
        { dependencies = [ "prelude" ]
        , repo = "https://github.com/purescript-contrib/purescript-precise"
        , version = "v4.0.0"
        }
      , reactix =
        { dependencies =
          [ "aff"
          , "dom-simple"
          , "effect"
          , "ffi-simple"
          , "functions"
          , "nullable"
          , "prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/irresponsible/purescript-reactix"
        , version = "v0.4.11"
        }
      , toestand = 
        { dependencies = [ "effect", "reactix", "prelude", "record", "tuples", "typelevel-prelude", "typisch" ]
        , repo = "https://github.com/poorscript/purescript-toestand"
        , version = "v0.6.1"
        }
      , typisch = 
        { dependencies = [ "prelude" ]
        , repo = "https://github.com/poorscript/purescript-typisch"
        , version = "v0.2.1"
        }
      , tuples-native =
        { dependencies =
          [ "prelude", "typelevel", "unsafe-coerce" ]
        , repo = "https://github.com/athanclark/purescript-tuples-native"
        , version = "v2.0.1"
        }
      , uint =
        { dependencies = [ "maybe", "math" ]
        , repo = "https://github.com/zaquest/purescript-uint"
        , version = "v5.1.1"
        }
      , uri =
        { dependencies =
          [ "these"
          , "arrays"
          , "profunctor-lenses"
          , "unfoldable"
          , "parsing"
          , "integers"
          , "globals"
          ]
        , repo = "https://github.com/slamdata/purescript-uri"
        , version = "v8.0.1"
        }
      , read =
        { dependencies = [ "prelude", "maybe", "strings" ]
        , repo = "https://github.com/truqu/purescript-read"
        , version = "v1.0.1"
        }
      , versions =
        { dependencies = [ "prelude" ]
        , repo = "https://github.com/hdgarrood/purescript-versions.git"
        , version = "v6.0.0"
        }
      , simplecrypto =
        { dependencies = [ "prelude", "maybe", "node-buffer" ]
        , repo = "https://github.com/alpacaaa/purescript-simplecrypto"
        , version = "v1.0.1"
        }
      }

in  upstream // overrides // additions
