module Console where
  
import Prelude (Unit)
import Data.Argonaut (Json)
import Effect.Promise (Promise)

foreign import info :: String -> Json -> Promise Unit
foreign import error :: String -> Json -> Promise Unit