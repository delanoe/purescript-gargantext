module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
-- import Effect.Uncurried (EffectFn1, runEffectFn1)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- foreign import _cryptoFix :: forall e. EffectFn1 e Unit

main :: Effect Unit
main = do
  launchAff_ do
    specs <- discover "Test\\.Gargantext\\..*Spec"
    runSpec [consoleReporter] specs
