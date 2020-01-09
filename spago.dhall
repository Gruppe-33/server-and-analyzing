{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "spectrophotometer"
, dependencies =
    [ "colors"
    , "console"
    , "control"
    , "datetime"
    , "effect"
    , "express"
    , "filterable"
    , "foldable-traversable"
    , "formatters"
    , "globals"
    , "node-fs"
    , "now"
    , "numbers"
    , "prelude"
    , "psci-support"
    , "simple-json"
    , "web-events"
    , "web-html"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
