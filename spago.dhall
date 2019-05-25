{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
	"stats"
, dependencies =
	[ "assert"
	, "arrays"
	, "console"
	, "psci-support"
	, "foldable-traversable"
	, "integers"
	, "ordered-collections"
	, "math"
	, "maybe"
	, "prelude"
	, "quickcheck"
	]
, packages =
	./packages.dhall
}
