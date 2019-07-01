module HsFp2.MonadsAndEffects.ExceptT.PasswordInputTask where
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (msum)
import Data.Char (isNumber, isPunctuation)
import Control.Monad (when)

data PwdError = PwdError String

instance Semigroup PwdError where 
    (<>) a b = a

instance Monoid PwdError where 
    mempty = PwdError ""
    mappend a b = a

type PwdErrorMonad = ExceptT PwdError IO

askPassword :: PwdErrorMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."
  
getValidPassword :: PwdErrorMonad String
getValidPassword = do
    s <- liftIO getLine
    when (length s < 8) (do
        liftIO $ putStrLn "Incorrect input: password is too short!"
        throwE $ PwdError "")
    when (not (any isNumber s)) (do 
        liftIO $ putStrLn "Incorrect input: password must contain some digits!"
        throwE $ PwdError "")
    when (not (any isPunctuation s)) (do
        liftIO $ putStrLn "Incorrect input: password must contain some punctuation!"
        throwE $ PwdError "")
    return s


tst1 = runExceptT askPassword
-- Enter your new password:
-- qwerty
-- Incorrect input: password is too short!
-- qwertyuiop
-- Incorrect input: password must contain some digits!
-- qwertyuiop123
-- Incorrect input: password must contain some punctuation!
-- qwertyuiop123!!!
-- Storing in database...
