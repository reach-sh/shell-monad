{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Control.Monad.Shell
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Monoid
default (T.Text)

main :: IO ()
main = T.writeFile "santa.sh" $ script $ do
	hohoho <- mkHohoho
	hohoho (static 1)

	promptFor "What's your name?" "virginia" $ \name -> pipeLess $ do
		cmd "echo" "Let's see what's in" (WithVar name (<> quote "'s")) "stocking!"
		forCmd (cmd "ls" "-1" (WithVar name (quote "/home/" <>))) $ \f -> do
			cmd "echo" "a shiny new" f
			hohoho (static 1)

	cmd "rm" "/table/cookies" "/table/milk"
	hohoho (static 3)

mkHohoho :: Script (Term Static Int -> Script ())
mkHohoho = func (NamedLike "hohoho") $ do
	num <- takeParameter (NamedLike "num")
	forCmd (cmd "seq" (static (1 :: Int)) num) $ \_n ->
		cmd "echo" "Ho, ho, ho!" "Merry xmas!"

pipeLess :: Script () -> Script ()
pipeLess c = c -|- cmd "less"

promptFor :: T.Text -> T.Text -> (Term Var String -> Script ()) -> Script ()
promptFor prompt defaultname cont = do
	cmd "printf" (prompt <> " ")
	var <- newVar (NamedLike prompt)
	readVar var
	cont =<< defaultVar var defaultname
