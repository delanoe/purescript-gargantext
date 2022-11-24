let upstream =
      https://github.com/garganscript/package-sets/releases/download/v0.1.4/release.dhall
        sha256:e03eafe0c7ea0ac143d07ec6d9f20c804bd6b6f95a8d89bf287c279e770584c8

let overrides =
      { graphql-client =
        { dependencies =
          [ "aff"
          , "aff-promise"
          , "affjax"
          , "affjax-node"
          , "affjax-web"
          , "argonaut-codecs"
          , "argonaut-core"
          , "arrays"
          , "bifunctors"
          , "control"
          , "datetime"
          , "debug"
          , "effect"
          , "either"
          , "enums"
          , "exceptions"
          , "foldable-traversable"
          , "foreign"
          , "foreign-object"
          , "functions"
          , "halogen-subscriptions"
          , "heterogeneous"
          , "http-methods"
          , "integers"
          , "lists"
          , "maybe"
          , "media-types"
          , "newtype"
          , "node-buffer"
          , "node-fs"
          , "nullable"
          , "numbers"
          , "ordered-collections"
          , "parsing"
          , "prelude"
          , "profunctor"
          , "profunctor-lenses"
          , "psci-support"
          , "quickcheck"
          , "record"
          , "spec"
          , "spec-discovery"
          , "string-parsers"
          , "strings"
          , "strings-extra"
          , "transformers"
          , "tuples"
          , "unicode"
          , "unsafe-coerce"
          , "variant"
          ]
        , repo =
            "https://github.com/OxfordAbstracts/purescript-graphql-client.git"
        , version = "v9.2.2"
        }
      , jest =
        { -- markdown-it dependency
          dependencies =
          [ "aff"
          , "aff-promise"
          , "effect"
          , "foldable-traversable"
          , "prelude"
          , "psci-support"
          ]
        , repo = "https://github.com/nonbili/purescript-jest"
        , version = "018543987af27db6a3842048b6b3f5ec47609087"
        }
      , markdown-it =
        { dependencies =
          [ "effect"
          , "foldable-traversable"
          , "foreign"
          , "jest"
          , "options"
          , "prelude"
          , "tuples"
          ]
        , repo = "https://github.com/nonbili/purescript-markdown-it"
        , version = "f6e8ee91298f2fc13c4277e75a19e0538de5f7a2"
        }
      , record-extra =
        { dependencies =
          [ "arrays"
          , "effect"
          , "functions"
          , "lists"
          , "maybe"
          , "prelude"
          , "record"
          , "test-unit"
          , "tuples"
          , "typelevel-prelude"
          ]
        , repo = "https://github.com/justinwoo/purescript-record-extra"
        , version = "0.15.0-starter-kit"
        }
      }

let additions =
      { sequences =
        { dependencies =
          [ "arrays"
          , "assert"
          , "console"
          , "control"
          , "effect"
          , "lazy"
          , "maybe"
          , "newtype"
          , "nonempty"
          , "partial"
          , "prelude"
          , "profunctor"
          , "psci-support"
          , "tuples"
          , "unfoldable"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/garganscript/purescript-sequences.git"
        , version = "7ad61fde5239fbf66ceeefc0b7608aa9cbc53701"
        }
      , spec-discovery =
        { dependencies = [ "prelude", "effect", "arrays", "spec", "node-fs" ]
        , repo = "https://github.com/purescript-spec/purescript-spec-discovery"
        , version = "v8.0.0"
        }
      , spec-quickcheck =
        { dependencies = [ "prelude", "aff", "random", "quickcheck", "spec" ]
        , repo = "https://github.com/purescript-spec/purescript-spec-quickcheck"
        , version = "v3.1.0"
        }
      , dom-filereader =
        { dependencies = [ "aff", "arraybuffer-types", "web-file", "web-html" ]
        , repo = "https://github.com/nwolverson/purescript-dom-filereader"
        , version = "v5.0.0"
        }
      , simple-json-generics =
        { dependencies =
          [ "assert"
          , "control"
          , "effect"
          , "either"
          , "foreign"
          , "partial"
          , "prelude"
          , "simple-json"
          , "transformers"
          , "typelevel-prelude"
          ]
        , repo =
            "https://github.com/garganscript/purescript-simple-json-generics"
        , version = "master"
        }
      , tuples-native =
        { dependencies =
          [ "console"
          , "effect"
          , "functions"
          , "prelude"
          , "tuples"
          , "typelevel"
          , "typelevel-prelude"
          ]
        , repo = "https://github.com/garganscript/purescript-tuples-native"
        , version = "v2.3.0"
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
      , web-url =
        { dependencies = [ "prelude" ]
        , repo = "https://github.com/mjepronk/purescript-web-url"
        , version = "v2.0.0"
        }
      , convertable-options =
        { dependencies = [ "console", "effect", "maybe", "record" ]
        , repo = "https://github.com/natefaubion/purescript-convertable-options"
        , version = "v1.0.0"
        }
      }

in  upstream // overrides // additions
