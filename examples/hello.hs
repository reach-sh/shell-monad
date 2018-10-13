{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
import Control.Monad.Shell
import Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
default (T.Text)

main :: IO ()
main = T.writeFile "hello.sh" $ script $ do
	cmd "echo" "hello, world"
	username <- newVarFrom (Output (cmd "whoami")) ()
	cmd "echo" "from" (WithVar username (<> "'s shell"))
