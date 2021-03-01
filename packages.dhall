let upstream =
  https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201021/packages.dhall

let overrides =
      { globals =
          { dependencies = [ "functions", "maybe" ]
          , repo = "https://github.com/purescript/purescript-globals"
          , version = "v4.1.0"
          }
      }

let additions =
      { dom-simple =
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
          , repo = "https://github.com/poorscript/purescript-dom-simple"
          , version = "v0.2.7"
          }
      , dom-filereader =
          { dependencies =
            [ "aff", "arraybuffer-types", "web-file", "web-html" ]
          , repo = "https://github.com/nwolverson/purescript-dom-filereader"
          , version = "v5.0.0"
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
          , repo = "https://github.com/poorscript/purescript-ffi-simple"
          , version = "v0.2.10"
          }
      , formula =
          { dependencies =
            [ "prelude", "reactix", "toestand", "typisch" ]
          , repo = "https://github.com/poorscript/purescript-formula"
          , version = "v0.2.1"
          }
      , markdown =
          { dependencies =
            [ "precise" ]
          , repo = "https://github.com/poorscript/purescript-markdown"
          , version = "2020-03-04"
          }
      , markdown-smolder =
          { dependencies =
            [ "markdown", "smolder" ]
          , repo = "https://github.com/poorscript/purescript-markdown-smolder"
          , version = "2020-03-04"
          }
      , precise =
          { dependencies =
            [ "prelude" ]
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
            , "tuples"
            , "unsafe-coerce"
            ]
          , repo = "https://github.com/poorscript/purescript-reactix"
          , version = "v0.4.11"
          }
      , read =
          { dependencies =
            [ "prelude", "maybe", "strings" ]
          , repo = "https://github.com/truqu/purescript-read"
          , version = "v1.0.1"
          }
      , sequences =
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
      , simplecrypto =
          { dependencies =
            [ "prelude", "maybe", "node-buffer"]
          , repo = "https://github.com/alpacaaa/purescript-simplecrypto"
          , version = "v1.0.1"
          }
      , spec-discovery =
          { dependencies =
            [ "prelude", "effect", "arrays", "spec", "node-fs" ]
          , repo = "https://github.com/purescript-spec/purescript-spec-discovery"
          , version = "v4.0.0"
          }
      , spec-quickcheck =
          { dependencies =
            [ "prelude", "aff", "random", "quickcheck", "spec" ]
          , repo = "https://github.com/purescript-spec/purescript-spec-quickcheck"
          , version = "v3.1.0"
          }
      , toestand =
          { dependencies =
            [ "prelude", "effect", "foldable-traversable", "reactix"
            , "record", "tuples", "typelevel-prelude" ]
          , repo = "https://github.com/poorscript/purescript-toestand"
          , version = "v0.5.0"
          }
      , typisch =
          { dependencies = [ "prelude" ]
          , repo = "https://github.com/poorscript/purescript-typisch"
          , version = "v0.2.1"
          }
      , tuples-native =
          { dependencies =
            [ "generics-rep", "prelude", "typelevel", "unsafe-coerce" ]
          , repo = "https://github.com/athanclark/purescript-tuples-native"
          , version = "v2.0.1"
          }
      , uint =
          { dependencies =
            [ "maybe", "math", "generics-rep" ]
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
            , "generics-rep"
            ]
          , repo = "https://github.com/slamdata/purescript-uri"
          , version = "v7.0.0"
          }
      , versions =
          { dependencies =
            [ "prelude" ]
          , repo = "https://github.com/hdgarrood/purescript-versions.git"
          , version = "v5.0.1"
          }
      }

in  upstream ⫽ overrides ⫽ additions
