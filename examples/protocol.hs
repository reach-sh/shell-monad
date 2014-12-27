{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Control.Monad.Shell
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Control.Applicative
default (T.Text)

data Proto
	= Foo String
	| Bar
	| Baz Integer
	deriving (Show)

class Monad t => OutputsProto t where
	output :: Proto -> t ()

instance OutputsProto IO where
	output = putStrLn . fromProto

instance OutputsProto Script where
	output = cmd "echo" . fromProto

class Monad t => InputsProto t p where
	input :: t p

instance InputsProto IO Proto where
	input = toProto <$> readLn

instance InputsProto Script Var where
	input = do
		v <- newVar ()
		readVar v
		return v

protoExchangeIO :: Proto -> IO Proto
protoExchangeIO p = do
	output p
	input

foo :: Script ()
foo = do
	stopOnFailure True
	handler <- func (NamedLike "handler") $
		handleProto =<< input
	output (Foo "starting up")
	handler
	output Bar
	handler

pFOO, pBAR, pBAZ :: String
(pFOO, pBAR, pBAZ) = ("FOO", "BAR", "BAZ")

fromProto :: Proto -> String
fromProto (Foo s) = pFOO ++ " " ++ s
fromProto Bar = pBAR ++ " "
fromProto (Baz i) = pBAZ ++ " " ++ show i

-- throws exception if the string cannot be parsed
toProto :: String -> Proto
toProto s = case break (== ' ') s of
	(w, ' ':rest)
		| w == pFOO -> Foo rest
		| w == pBAR && null rest -> Bar
		| w == pBAZ -> Baz (read rest)
		| otherwise -> error $ "unknown protocol command: " ++ w
	(_, _) -> error "protocol splitting error"

handleProto :: Var -> Script ()
handleProto v = do
	w <- getProtoCommand v
	caseOf w
		[ (quote (T.pack pFOO), handleFoo =<< getProtoRest v)
		, (quote (T.pack pBAR), handleBar)
		, (quote (T.pack pBAZ), handleBaz =<< getProtoRest v)
		, (glob "*", do
			toStderr $ cmd "echo" "unknown protocol command" w
			cmd "false"
		  )
		]

handleFoo :: Var -> Script ()
handleFoo v = toStderr $ cmd "echo" "yay, I got a Foo" v

handleBar :: Script ()
handleBar = toStderr $ cmd "echo" "yay, I got a Bar"

handleBaz :: Var -> Script ()
handleBaz num = forCmd (cmd "seq" "1" num) $
	toStderr . cmd "echo" "yay, I got a Baz"

getProtoCommand :: Var -> Script Var
getProtoCommand v = trimVar LongestMatch FromEnd v (glob " *")

getProtoRest :: Var -> Script Var
getProtoRest v = trimVar ShortestMatch FromBeginning v (glob "[! ]*[ ]")

main :: IO ()
main = T.writeFile "protocol.sh" $ script foo
