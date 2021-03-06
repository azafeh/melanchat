{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "melanchat"
, license = "AGPL-3.0-or-later"
, repository = "https://github.com/melanchat/melanchat"
, dependencies =
  [ "affjax"
  , "argonaut-generic"
  , "browser-cookies"
  , "console"
  , "debug"
  , "effect"
  , "enums"
  , "exceptions"
  , "flame"
  , "foreign-object"
  , "form-urlencoded"
  , "http-methods"
  , "node-fs"
  , "node-process"
  , "now"
  , "payload"
  , "postgresql-client"
  , "prelude"
  , "psci-support"
  , "read"
  , "run"
  , "simple-jwt"
  , "unordered-collections"
  , "uuid"
  , "web-dom"
  , "web-socket"
  , "web-storage"
  , "web-uievents",
  "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
