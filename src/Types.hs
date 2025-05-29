module Types (STLCType(..), Binding, Context, Var(..)) where
import Var (Var(..))
data STLCType = Unit | Bool | Nat | Arrow STLCType STLCType deriving (Eq, Show)
type Binding = (Var,STLCType)
type Context = [Binding] 


