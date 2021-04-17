{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "either"
  , "foreign-object"
  , "node-buffer"
  , "node-child-process"
  , "node-fs"
  , "node-process"
  , "psci-support"
  , "record"
  , "strings"
  , "stringutils"
  , "transformers"
  , "yargs"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
