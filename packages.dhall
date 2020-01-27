{-
Welcome to Spacchetti local packages!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let override =
  { packageName =
      upstream.packageName ⫽ { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName ⫽ { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen ⫽ { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom ⫽ { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't alread included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { "package-name" =
       mkPackage
         [ "dependency1"
         , "dependency2"
         ]
         "https://example.com/path/to/git/repo.git"
         "tag ('v4.0.0') or branch ('master')"
  , "package-name" =
       mkPackage
         [ "dependency1"
         , "dependency2"
         ]
         "https://example.com/path/to/git/repo.git"
         "tag ('v4.0.0') or branch ('master')"
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      mkPackage
        [ "arrays"
        , "exists"
        , "profunctor"
        , "strings"
        , "quickcheck"
        , "lcg"
        , "transformers"
        , "foldable-traversable"
        , "exceptions"
        , "node-fs"
        , "node-buffer"
        , "node-readline"
        , "datetime"
        , "now"
        ]
        "https://github.com/hdgarrood/purescript-benchotron.git"
        "v7.0.0"
  }
-------------------------------
-}

let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190804/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190804/src/packages.dhall sha256:2230fc547841b54bca815eb0058414aa03ed7b675042f8b3dda644e1952824e5

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
          "https://github.com/np/purescript-thermite.git"
          "hide"
      , globals =
          mkPackage
          [ "functions"
          , "maybe"
          ]
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
          "v0.2.4"
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
          "v0.2.6"
      , dom-filereader =
          mkPackage
          [ "aff", "arraybuffer-types", "web-file", "web-html" ]
          "https://github.com/nwolverson/purescript-dom-filereader"
          "v5.0.0"
      , markdown =
          mkPackage
          [ "precise" ]
          {- "https://github.com/slamdata/purescript-markdown"
          "v12.0.0" -}
          "https://github.com/poorscript/purescript-markdown"
          "master"
      , markdown-smolder =
          mkPackage
          [ "markdown"
          , "smolder" ]
          {- "https://github.com/hgiasac/purescript-markdown-smolder"
          "v2.0.1" -}
          "https://github.com/poorscript/purescript-markdown-smolder"
          "master"
      , precise =
          mkPackage
          [ "prelude" ]
          "https://github.com/purescript-contrib/purescript-precise"
          {- "v3.0.1" -}
          "master"
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
          "v0.4.2"
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
       , read = mkPackage ["prelude", "maybe", "strings"]
                "https://github.com/truqu/purescript-read"
                "v1.0.1"
      }

in  upstream // overrides // additions
