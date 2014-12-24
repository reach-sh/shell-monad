{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Control.Monad.Shell
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Monoid
default (T.Text)

main :: IO ()
main = T.writeFile "santa.sh" $ script $ do
	hohoho <- func $
		cmd "echo" "Ho, ho, ho!" "Merry xmas!"
	hohoho

	promptFor "What's your name?" $ \name -> pipeLess $ do
		cmd "echo" "Let's see what's in" (val name <> quote "'s") "stocking!"
		forCmd (cmd "ls" "-1" (quote "/home/" <> val name)) $ \f -> do
			cmd "echo" "a shiny new" f
			hohoho

	cmd "rm" "/table/cookies" "/table/milk"
	hohoho

pipeLess :: Script () -> Script ()
pipeLess c = c -|- cmd "less"

promptFor :: T.Text -> (Var -> Script ()) -> Script ()
promptFor prompt cont = do
	cmd "printf" (prompt <> " ")
	var <- newVar "prompt"
	readVar var
	cont var
