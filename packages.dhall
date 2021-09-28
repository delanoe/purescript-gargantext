let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211030/packages.dhall sha256:5cd7c5696feea3d3f84505d311348b9e90a76c4ce3684930a0ff29606d2d816c

let overrides =
      { globals =
        { dependencies = [ "functions", "maybe" ]
        , repo = "https://github.com/purescript/purescript-globals"
        , version = "v4.1.0"
        }
      , smolder =
        { dependencies =
          [ "bifunctors"
          , "catenable-lists"
          , "free"
          , "ordered-collections"
          , "prelude"
          , "strings"
          , "test-unit"
          , "transformers"
          , "tuples"
          ]
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
        , version = "v3.0.2"
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
        { dependencies =
          [ "effect"
          , "prelude"
          , "reactix"
          , "record"
          , "toestand"
          , "tuples"
          , "typelevel-prelude"
          , "typisch"
          ]
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
        , repo = "https://github.com/hgiasac/purescript-markdown-smolder"
        , version = "v2.2.0"
        }
      , precise =
        { dependencies = [ "prelude" ]
        , repo = "https://github.com/purescript-contrib/purescript-precise"
        , version = "v4.0.0"
        }
      , reactix =
        { dependencies =
          [ "aff"
          , "arrays"
          , "dom-simple"
          , "effect"
          , "ffi-simple"
          , "foldable-traversable"
          , "functions"
          , "maybe"
          , "nullable"
          , "prelude"
          , "psci-support"
          , "refs"
          , "spec"
          , "spec-mocha"
          , "strings"
          , "tuples"
          , "unfoldable"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/poorscript/purescript-reactix"
        , version = "v0.4.13"
        }
      , simple-json-generics =
        { dependencies = [ "simple-json" ]
        , repo = "https://github.com/justinwoo/purescript-simple-json-generics"
        , version = "v0.1.0"
        }
      , toestand =
        { dependencies =
          [ "effect"
          , "reactix"
          , "prelude"
          , "record"
          , "tuples"
          , "typelevel-prelude"
          , "typisch"
          ]
        , repo = "https://github.com/poorscript/purescript-toestand"
        , version = "v0.6.2"
        }
      , typisch =
        { dependencies = [ "prelude" ]
        , repo = "https://github.com/poorscript/purescript-typisch"
        , version = "v0.2.1"
        }
      , tuples-native =
        { dependencies = [ "prelude", "typelevel", "unsafe-coerce" ]
        , repo = "https://github.com/poorscript/purescript-tuples-native"
        , version = "v2.2.0"
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
      , web-url =
        { dependencies = [ "prelude" ]
        , repo = "https://github.com/mjepronk/purescript-web-url"
        , version = "v1.0.2"
        }
      , convertable-options =
        { dependencies = [ "console", "effect", "maybe", "record" ]
        , repo = "https://github.com/natefaubion/purescript-convertable-options"
        , version = "v1.0.0"
        }
      , d3 =
        { dependencies =
          [ "aff"
          , "aff-promise"
          , "dom-simple"
          , "easy-ffi"
          , "effect"
          , "exceptions"
          , "foreign"
          , "functions"
          , "js-date"
          , "maybe"
          , "prelude"
          , "psci-support"
          , "tuples"
          , "web-dom"
          ]
        , repo = "https://github.com/cgenie/purescript-d3"
        , version = "v0.9.1"
        }
      }

in  upstream // overrides // additions
