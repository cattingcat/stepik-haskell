module Sandbox.ApplicativeList where 
import Control.Applicative
import Control.Monad


l1 = [1,2,3]
l2 = [5,6,7]
lf = [(+1), (^2), (*5)]

tst1 = l1 <* l2
tst2 = l1 *> l2
tst3 = lf <*> l1
tst4 = l1 <|> l2     -- Alternative
tst5 = l1 `mplus` l2 -- MonadPlus

-- Requires MonadPlus, and returns Applicative#pure() if True, 
--    or MonadPlus#empty if False
--  Useful in do-notation
tst6, tst7 :: [()]
tst6 = guard (False)
tst7 = guard (True)

tst6', tst7' :: Maybe ()
tst6' = guard (False)
tst7' = guard (True)