{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "dagbok"
, dependencies =
  [ "argonaut"
  , "console"
  , "effect"
  , "foreign"
  , "maybe"
  , "promises"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
