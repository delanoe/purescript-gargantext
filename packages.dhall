let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190804/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.2-20190804/packages.dhall sha256:2230fc547841b54bca815eb0058414aa03ed7b675042f8b3dda644e1952824e5

let overrides =
      { thermite =
          mkPackage
            [ "aff"
            , "coroutines"
            , "web-dom"
            , "freet"
            , "profunctor-lenses"
            , "react"
            , "react-dom"
            ]
            "https://github.com/poorscript/purescript-thermite.git"
            "hide-2020-03-04"
      , globals =
          mkPackage
            [ "functions", "maybe" ]
            "https://github.com/purescript/purescript-globals"
            "v4.1.0"
      }

let additions =
      { sequences =
          mkPackage
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
            "https://github.com/hdgarrood/purescript-sequences.git"
            "v2.1.0"
      , spec-discovery =
          mkPackage
            [ "prelude", "effect", "arrays", "spec", "node-fs" ]
            "https://github.com/purescript-spec/purescript-spec-discovery"
            "v4.0.0"
      , spec-quickcheck =
          mkPackage
            [ "prelude", "aff", "random", "quickcheck", "spec" ]
            "https://github.com/purescript-spec/purescript-spec-quickcheck"
            "v3.1.0"
      , ffi-simple =
          mkPackage
            [ "prelude"
            , "effect"
            , "maybe"
            , "functions"
            , "nullable"
            , "unsafe-coerce"
            ]
            "https://github.com/irresponsible/purescript-ffi-simple"
            "v0.2.10"
      , dom-simple =
          mkPackage
            [ "arrays"
            , "console"
            , "effect"
            , "ffi-simple"
            , "functions"
            , "nullable"
            , "prelude"
            , "unsafe-coerce"
            ]
            "https://github.com/irresponsible/purescript-dom-simple"
            "v0.2.7"
      , dom-filereader =
          mkPackage
            [ "aff", "arraybuffer-types", "web-file", "web-html" ]
            "https://github.com/nwolverson/purescript-dom-filereader"
            "v5.0.0"
      , markdown =
          mkPackage
            [ "precise" ]
            "https://github.com/poorscript/purescript-markdown"
            "2020-03-04"
      , markdown-smolder =
          mkPackage
            [ "markdown", "smolder" ]
            "https://github.com/poorscript/purescript-markdown-smolder"
            "2020-03-04"
      , precise =
          mkPackage
            [ "prelude" ]
            "https://github.com/purescript-contrib/purescript-precise"
            "v4.0.0"
      , reactix =
          mkPackage
            [ "aff"
            , "dom-simple"
            , "effect"
            , "ffi-simple"
            , "functions"
            , "nullable"
            , "prelude"
            , "unsafe-coerce"
            ]
            "https://github.com/irresponsible/purescript-reactix"
            "v0.4.5"
      , tuples-native =
          mkPackage
            [ "generics-rep", "prelude", "typelevel", "unsafe-coerce" ]
            "https://github.com/athanclark/purescript-tuples-native"
            "v2.0.1"
      , uint =
          mkPackage
            [ "maybe", "math", "generics-rep" ]
            "https://github.com/zaquest/purescript-uint"
            "v5.1.1"
      , uri =
          mkPackage
            [ "these"
            , "arrays"
            , "profunctor-lenses"
            , "unfoldable"
            , "parsing"
            , "integers"
            , "globals"
            , "generics-rep"
            ]
            "https://github.com/slamdata/purescript-uri"
            "v7.0.0"
      , read =
          mkPackage
            [ "prelude", "maybe", "strings" ]
            "https://github.com/truqu/purescript-read"
            "v1.0.1"
      , versions =
          mkPackage
            [ "prelude" ]
            "https://github.com/hdgarrood/purescript-versions.git"
            "v5.0.1"
      , simplecrypto =
          mkPackage
            [ "prelude", "maybe", "node-buffer"]
            "https://github.com/alpacaaa/purescript-simplecrypto"
            "v1.0.1"
      }

in  upstream ⫽ overrides ⫽ additions
