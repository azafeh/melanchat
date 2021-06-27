module Shared.Setter where

import Data.Symbol (class IsSymbol)
import Prim.Row (class Nub, class Lacks, class Cons, class Union)
import Record as R
import Shared.IM.Types
import Shared.Types
import Type.Proxy(Proxy(..))
import Prelude

setUserField field value model = model {
      user = R.set field value model.user
}

setIMField :: forall field r v. IsSymbol field => Cons field v r IM => Proxy field -> v -> IMMessage
setIMField field = SetField <<< R.set field