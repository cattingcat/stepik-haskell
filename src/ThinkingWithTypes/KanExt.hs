module ThinkingWithTypes.KanExt where

import Data.Functor.Yoneda
import Data.Functor.Day.Curried
import Control.Monad.Codensity

-- | Allows to remove "too much polymprphism" problem
-- forall f . Functor f => f a   replace with    forall f . Yoneda f a
-- forall f. Applicative f => f a                forall f. Curried (Yoneda f) (Yoneda f) a
-- forall f. Monad f => f a                      forall f. Codensity f a

-- | Yoneda is functor even if @f@ isn't