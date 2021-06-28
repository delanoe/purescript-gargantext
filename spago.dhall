{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "gargantext"
, dependencies =
  [ "aff-promise"
  , "affjax"
  , "argonaut"
  , "console"
  , "css"
  , "datetime"
  , "debug"
  , "dom-filereader"
  , "dom-simple"
  , "effect"
  , "foreign-generic"
  , "foreign-object"
  , "formula"
  , "globals"
  , "integers"
  , "js-timers"
  , "markdown-smolder"
  , "math"
  , "maybe"
  , "milkis"
  , "nonempty"
  , "now"
  , "numbers"
  , "prelude"
  , "psci-support"
  , "random"
  , "react"
  , "reactix"
  , "read"
  , "record-extra"
  , "routing"
  , "sequences"
  , "simple-json"
  , "simple-json-generics"
  , "simplecrypto"
  , "smolder"
  , "spec-discovery"
  , "spec-quickcheck"
  , "string-parsers"
  , "strings"
  , "stringutils"
  , "toestand"
  , "tuples-native"
  , "typisch"
  , "uint"
  , "uri"
  , "versions"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
