-- | Very closely based on the the Haskell package `HoleyMonoid`.
module Template where

import Prelude
import Control.Semigroupoid (composeFlipped)
import Data.Function as F

{----------------------------------------------------------------------------------------------------------------------}

newtype Template r a = Template ((String -> r) -> a)

runHM :: forall r a. Template r a -> (String -> r) -> a
runHM (Template f) = f

instance Functor (Template r) where
  map f (Template g) = Template (f <<< g)

instance Apply (Template r) where
  apply (Template f) (Template g) = Template (f <*> g)

instance Applicative (Template r) where
  pure x = Template (pure x)

instance Bind (Template r) where
  bind (Template f) g = Template (f >>= \x -> runHM (g x))

instance Monad (Template r)

instance Semigroupoid Template where
  compose f g = f `ibind` \a -> g `ibind` \b -> now (a `append` b)

infixr 9 compose as ++

instance Category Template where
  identity = now mempty

-- | Monadic indexed bind on the underlying `String` moinoidal type.
ibind :: forall b c a. Template b c -> (String -> Template a b) -> Template a c
ibind m f = Template $ \k -> runHM m (\a -> runHM (f a) k)

now :: forall r. String -> Template r r
now a = Template (F.applyFlipped a)

later :: forall a r. (a -> String) -> Template r (a -> r)
later f = Template (composeFlipped f)

laterShow :: forall a r. Show a => Template r (a -> r)
laterShow = later show

run :: forall a. Template String a -> a
run m = runHM m identity