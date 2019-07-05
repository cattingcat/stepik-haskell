{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}



module HsFp2.MonadsAndEffects.ReaderTKind where

-- PolyKind and KingSig
newtype ReaderT (e :: *) (m :: k -> *) (a :: k) = ReaderT { runReaderT :: e -> m a }

data SomeMonadicType (a :: *) (m :: * -> *) = Kek (m a) | Puk a

data SuperMonadicType (mm :: (* -> *) -> *) = Lol | Rofl


tst :: ReaderT [String] (SuperMonadicType) (SomeMonadicType Int)
tst = undefined


-- DataKinds and GADTs
data TmpMods = Qwe | Asd | Ert
data Tmp (a :: TmpMods) where 
    -- TmpKek :: Int -> Tmp a
    TmpPuk :: String -> Tmp a


tst1 :: Tmp Qwe
tst1 = TmpPuk "dfsd"
