-- | A shell script monad

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Shell (
	Script,
	script,
	linearScript,
	Var,
	val,
	Quoted,
	quote,
	run,
	cmd,
	CmdArg,
	comment,
	newVar,
	newVarContaining,
	globalVar,
	func,
	(-|-),
	forCmd,
	whileCmd,
	ifCmd,
	whenCmd,
	unlessCmd,
	readVar,
	stopOnFailure,
	ignoreFailure,
) where

import qualified Data.Text.Lazy as L
import qualified Data.Set as S
import Data.Monoid
import Control.Applicative
import Data.Char

-- | A shell variable.
newtype Var = Var L.Text
	deriving (Eq, Ord, Show)

-- | Expand a shell variable to its value.
val :: Var -> Quoted L.Text
val (Var v) = Q ("\"$" <> v <> "\"")

-- | A value that is safely quoted.
newtype Quoted a = Q { getQ :: a }
	deriving (Eq, Ord, Show, Monoid)

-- | Quotes the value to allow it to be safely exposed to the shell.
--
-- The method used is to replace ' with '"'"' and wrap the value inside
-- single quotes. This works for POSIX shells, as well as other shells
-- like csh.
quote :: L.Text -> Quoted L.Text
quote t
	| L.all (isAlphaNum) t = Q t
	| otherwise = Q $ q <> L.intercalate "'\"'\"'" (L.splitOn q t) <> q
  where
	q = "'"

-- | A shell function.
newtype Func = Func L.Text
	deriving (Eq, Ord, Show)

-- | A shell expression.
data Expr
	= Cmd L.Text -- ^ a command
	| Comment L.Text -- ^ a comment
	| HereDocBody L.Text -- ^ the body of a here-doc
	| Subshell L.Text [Expr] -- ^ expressions run in a sub-shell
	| Pipe Expr Expr -- ^ Piping the first Expr to the second Expr

-- | Indents an Expr
indent :: Expr -> Expr
indent (Cmd t) = Cmd $ "\t" <> t
indent (Comment t) = Comment $ "\t" <> t
indent (HereDocBody t) = HereDocBody t -- cannot indent
indent (Subshell i l) = Subshell ("\t" <> i) (map indent l)
indent (Pipe e1 e2) = Pipe (indent e1) (indent e2)

-- | Shell script monad.
newtype Script a = Script (Env -> ([Expr], Env, a))
	deriving (Functor)

instance Monad Script where
        return ret = Script $ \env -> ([], env, ret)
        a >>= b = Script $ \start -> let
                (left, mid, v) = call a start
                (right, end, ret) = call (b v) mid
                in (left ++ right, end, ret)
	  where
		call :: Script f -> Env -> ([Expr], Env, f)
		call (Script f) = f

-- | Environment built up by the shell script monad,
-- so it knows which environment variables and functions are in use.
data Env = Env
	{ envVars :: S.Set Var
	, envFuncs :: S.Set Func
	}

instance Monoid Env where
	mempty = Env mempty mempty
	mappend a b = Env (envVars a <> envVars b) (envFuncs a <> envFuncs b)

modifyEnvVars :: Env -> (S.Set Var -> S.Set Var) -> Env
modifyEnvVars env f = env { envVars = f (envVars env) }

modifyEnvFuncs :: Env -> (S.Set Func -> S.Set Func) -> Env
modifyEnvFuncs env f = env { envFuncs = f (envFuncs env) }

-- | Evaluate the monad and generates a list of Expr
gen :: Script f -> [Expr]
gen = fst . eval mempty

-- | Evaluates the monad, and returns a list of Expr and the modified
-- environment.
eval :: Env -> Script f -> ([Expr], Env)
eval env (Script f) = (code, env') where (code, env', _) = f env

-- | Runs the passed Script, using the current environment,
-- and returns the list of Expr it generates.
runM :: Script () -> Script [Expr]
runM s = Script $ \env -> 
	let (r, env') = eval env s
	in ([], env', r)

-- | Generates a shell script, including hashbang,
-- suitable to be written to a file.
script :: Script f -> L.Text
script = flip mappend "\n" . L.intercalate "\n" . ("#!/bin/sh":) . map fmt . gen
  where
	fmt (Cmd t) = t
	fmt (Comment t) = "# " <> L.filter (/= '\n') t
	fmt (HereDocBody t) = t
	fmt (Subshell i l) = i <> "(\n" <> L.intercalate "\n" (map (fmt . indent) l) <> "\n" <> i <> ")"
	fmt (Pipe e1 e2) = fmt e1 <> " | " <> fmt e2

-- | Generates a single line of shell code.
linearScript :: Script f -> L.Text
linearScript = toLinearScript . gen

toLinearScript :: [Expr] -> L.Text
toLinearScript = L.intercalate "; " . map fmt
  where
	fmt (Cmd t) = t
	-- Use : as a no-op command, and pass the comment to it.
	fmt (Comment t) = ": " <> getQ (quote (L.filter (/= '\n') t))
	-- No way to express a here-doc in a single line.
	fmt (HereDocBody _) = ""
	fmt (Subshell i l) = i <> "(" <> L.intercalate "; " (map (fmt . indent) l) <> i <> ")"
	fmt (Pipe e1 e2) = fmt e1 <> " | " <> fmt e2

-- | Adds a shell command to the script.
run :: L.Text -> [L.Text] -> Script ()
run c ps = add $ Cmd $ L.intercalate " " (map (getQ . quote) (c:ps))

-- | Variadic argument version of 'run'.
--
-- The command can be passed any number of CmdArgs.
--
-- Convenient usage of 'cmd' requires the following:
--
-- > {-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
-- > {-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- > import Control.Monad.Shell
-- > import qualified Data.Text.Lazy as L
-- > default (L.Text)
--
-- This allows writing, for example:
--
-- > demo = script $ do
-- >   cmd "echo" "hello, world"
-- >   name <- newVar "name"
-- >   readVar name
-- >   cmd "echo" "hello" name
cmd :: (ShellCmd result) => L.Text -> result
cmd c = cmdAll c []

class CmdArg a where
	toTextArg :: a -> L.Text

-- | Text arguments are automatically quoted.
instance CmdArg L.Text where
	toTextArg = getQ . quote

-- | Var arguments cause the (quoted) value of a shell variable to be
-- passed to the command.
instance CmdArg Var where
	toTextArg v = toTextArg (val v)

-- | Quoted Text arguments are passed as-is.
instance CmdArg (Quoted L.Text) where
	toTextArg (Q v) = v

class ShellCmd t where
	cmdAll :: L.Text -> [L.Text] -> t

instance (CmdArg arg, ShellCmd result) => ShellCmd (arg -> result) where
	cmdAll c acc x = cmdAll c (toTextArg x : acc)

instance (f ~ ()) => ShellCmd (Script f) where
	cmdAll c acc = add $ Cmd $ L.intercalate " " (c:reverse acc)

-- | Adds an Expr to the script.
add :: Expr -> Script ()
add expr = Script $ \env -> ([expr], env, ())

-- | Adds a comment that is embedded in the generated shell script.
comment :: L.Text -> Script ()
comment = add . Comment

-- | Defines a new shell variable.
--
-- The name of the variable that appears in the shell script will be based
-- on provided name (which can be mempty), but each call to newVar will
-- generate a new, unique variable name.
newVar
	:: L.Text -- ^ base of variable name
	-> Script Var
newVar basename = Script $ \env ->
	let v = go env (0 :: Integer)
	in ([], modifyEnvVars env (S.insert v), v)
  where
	go env x
		| S.member v (envVars env) = go env (succ x)
		| otherwise = v
	  where
		v = Var $ "_" <> basename <> L.pack (show (x + 1))

-- | Creates a new shell variable, with an initial value.
newVarContaining
	:: L.Text -- ^ base of variable name
	-> L.Text -- ^ value
	-> Script Var
newVarContaining basename value = do
	v@(Var name) <- newVar basename
	Script $ \env -> ([Cmd (name <> "=" <> getQ (quote value))], env, v)

-- | Gets a Var that refers to a global variable, such as PATH
globalVar :: L.Text -> Script Var
globalVar name = Script $ \env -> let v = Var name in ([], modifyEnvVars env (S.insert v), v)

-- | Defines a shell function, and returns an action that can be run to
-- call the function.
--
-- TODO parameter passing to the function
func :: Script () -> Script (Script ())
func s = Script $ \env ->
	let f = go env (0 :: Integer)
	    env' = modifyEnvFuncs env (S.insert f)
	    (ls, env'') = eval env' s
	in (definefunc f ls, env'', callfunc f)
  where
	basename = "p"
	go env x
		| S.member f (envFuncs env) = go env (succ x)
		| otherwise = f
	  where
		f = Func $ basename <> L.pack (show (x + 1))

	definefunc (Func f) ls = (Cmd $ f <> " () { :") : map indent ls ++ [ Cmd "}" ]

	callfunc :: Func -> Script ()
	callfunc (Func f) = add $ Cmd f

-- | Pipes together two Scripts.
(-|-) :: Script () -> Script () -> Script ()
a -|- b = do
	alines <- runM a
	blines <- runM b
	add $ Pipe (toExp alines) (toExp blines)
  where
	toExp [e] = e
	toExp l = Subshell L.empty l

-- | Runs the command, and separates its output into parts
-- (using the IFS)
--
-- The action is run for each part, passed a Var containing the part.
forCmd :: Script () -> (Var -> Script ()) -> Script ()
forCmd c a = do
	v@(Var vname) <- newVar "x"
	s <- toLinearScript <$> runM c
	add $ Cmd $ "for " <> vname <> " in $(" <> s <> ")"
	block "do" (a v)
	add $ Cmd "done"

-- | As long as the first Script exits nonzero, runs the second script.
whileCmd :: Script () -> Script () -> Script ()
whileCmd c a = do
	s <- toLinearScript <$> runM c
	add $ Cmd $ "while $(" <> s <> ")"
	block "do" a
	add $ Cmd "done"

-- | if with a monadic conditional
--
-- If the conditional exits 0, the first action is run, else the second.
ifCmd :: Script () -> Script () -> Script () -> Script ()
ifCmd cond thena elsea = 
	ifCmd' id cond $ do
		block "then" thena
		block "else" elsea

ifCmd' :: (L.Text -> L.Text) -> Script () -> Script () -> Script ()
ifCmd' condf cond body = do
	condl <- runM cond
	add $ Cmd $ "if " <> condf (singleline condl)
	body
	add $ Cmd "fi"
  where
	singleline l =
		let c = case l of
			[c'@(Cmd {})] -> c'
			[c'@(Subshell {})] -> c'
			_ -> Subshell L.empty l
		in toLinearScript [c]

-- | when with a monadic conditional
whenCmd :: Script () -> Script () -> Script ()
whenCmd cond a = 
	ifCmd' id cond $
		block "then" a

-- | unless with a monadic conditional
unlessCmd :: Script () -> Script () -> Script ()
unlessCmd cond a =
	ifCmd' ("! " <>) cond $
		block "then" a

-- | Creates a block such as "do : ; cmd ; cmd" or "else : ; cmd ; cmd"
--
-- The use of : ensures that the block is not empty, and allows
-- for more regular indetnetion, as well as making the single line
-- formatting work.
block :: L.Text -> Script () -> Script ()
block word s = do
	add $ Cmd $ word <> " :"
	mapM_ (add . indent) =<< runM s

-- | Generates shell code to fill a variable with a line read from stdin.
readVar :: Var -> Script ()
readVar (Var vname) = add $ Cmd $ "read " <> getQ (quote vname)

-- | By default, shell scripts continue running past commands that exit
-- nonzero. Use "stopOnFailure True" to make the script stop on the first
-- such command.
stopOnFailure :: Bool -> Script ()
stopOnFailure b = add $ Cmd $ "set " <> if b then "-" else "+" <> "x"

-- | Makes a nonzero exit status be ignored.
ignoreFailure :: Script () -> Script ()
ignoreFailure s = runM s >>= mapM_ (add . go)
  where
	go (Cmd t) = Cmd $ t <> " || true"
	go c@(Comment _) = c
	go c@(HereDocBody _) = c
	go (Subshell i l) = Subshell i (map go l)
	-- Assumes pipefail is not set.
	go (Pipe e1 e2) = Pipe e1 (go e2)
