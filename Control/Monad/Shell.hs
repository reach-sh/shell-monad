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
	glob,
	run,
	cmd,
	CmdArg,
	Output(..),
	Val(..),
	comment,
	NamedLike(..),
	NameHinted,
	newVar,
	newVarContaining,
	globalVar,
	positionalParameters,
	takeParameter,
	func,
	forCmd,
	whileCmd,
	ifCmd,
	whenCmd,
	unlessCmd,
	readVar,
	stopOnFailure,
	ignoreFailure,
	(-|-),
	(-&&-),
	(-||-),
	(|>),
	(|>>),
	(|<),
	(|>&),
	toStderr,
	(|<&),
	hereDocument,
) where

import qualified Data.Text.Lazy as L
import qualified Data.Set as S
import Data.Monoid
import Control.Applicative
import Data.Char
import System.Posix.Types (Fd)
import System.Posix.IO (stdInput, stdOutput, stdError)

-- | A shell variable.
newtype Var = Var L.Text
	deriving (Eq, Ord, Show)

-- | Expand a shell variable to its value.
val :: Var -> Quoted L.Text
val (Var v) = Q ("\"$" <> v <> "\"")

-- | A value that is safely quoted.
newtype Quoted a = Q { getQ :: a }
	deriving (Eq, Ord, Show, Monoid)

-- | Quotes the Text to allow it to be safely exposed to the shell.
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

-- | Treats the Text as a glob, which expands to one parameter per
-- matching file.
--
-- The input is assumed to be a well-formed glob. Characters in it that
-- are not alphanumeric and are not wildcard characters will be escaped
-- before it is exposed to the shell. This allows eg, spaces in globs.
glob :: L.Text -> Quoted L.Text
glob = Q . L.concatMap escape
  where
	escape c
		| isAlphaNum c = L.singleton c
		| c `elem` "*?[!-:]\\" = L.singleton c
		| otherwise = "\\" <> L.singleton c

-- | A shell function.
newtype Func = Func L.Text
	deriving (Eq, Ord, Show)

-- | A shell expression.
data Expr
	= Cmd L.Text -- ^ a command
	| Comment L.Text -- ^ a comment
	| Subshell L.Text [Expr] -- ^ expressions run in a sub-shell
	| Pipe Expr Expr -- ^ Piping the first Expr to the second Expr
	| And Expr Expr -- ^ &&
	| Or Expr Expr -- ^ ||
	| Redir Expr RedirSpec -- ^ Redirects a file handle of the Expr

-- | Indents an Expr
indent :: Expr -> Expr
indent (Cmd t) = Cmd $ "\t" <> t
indent (Comment t) = Comment $ "\t" <> t
indent (Subshell i l) = Subshell ("\t" <> i) (map indent l)
indent (Pipe e1 e2) = Pipe (indent e1) (indent e2)
indent (Redir e r) = Redir (indent e) r
indent (And e1 e2) = And (indent e1) (indent e2)
indent (Or e1 e2) = Or (indent e1) (indent e2)

-- | Specifies a redirection.
data RedirSpec
	= RedirToFile Fd FilePath -- ^ redirect the fd to a file
	| RedirToFileAppend Fd FilePath -- ^ append to file
	| RedirFromFile Fd FilePath -- ^ use a file as input
	| RedirOutput Fd Fd -- ^ redirect first fd to the second
	| RedirInput Fd Fd -- ^ same, but for input fd
	| RedirHereDoc L.Text -- ^ use a here document as input

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

-- | runScriptuate the monad and generates a list of Expr
gen :: Script f -> [Expr]
gen = fst . runScript mempty

-- | Runs  the monad, and returns a list of Expr and the modified
-- environment.
runScript :: Env -> Script f -> ([Expr], Env)
runScript env (Script f) = (code, env') where (code, env', _) = f env

-- | Runs the passed Script, using the current environment,
-- and returns the list of Expr it generates.
runM :: Script () -> Script [Expr]
runM s = Script $ \env -> 
	let (r, env') = runScript env s
	in ([], env', r)

-- | Generates a shell script, including hashbang,
-- suitable to be written to a file.
script :: Script f -> L.Text
script = flip mappend "\n" . L.intercalate "\n" . 
	("#!/bin/sh":) . map (fmt True) . gen
  where

-- | Formats an Expr to shell  script.
--
-- Can generate either multiline or single line shell script.
fmt :: Bool -> Expr -> L.Text
fmt multiline = go
  where
	go (Cmd t) = t
	go (Comment t)
		| multiline = "# " <> L.filter (/= '\n') t
		-- Comments go to end of line, so instead
		-- use : as a no-op command, and pass the comment to it.
		| otherwise = ": " <> getQ (quote (L.filter (/= '\n') t))
	go (Subshell i l) =
		let (wrap, sep) = if multiline then ("\n", "\n") else ("", ";")
		in i <> "(" <> wrap <> L.intercalate sep (map (go . indent) l) <> wrap <> i <> ")"
	go (Pipe e1 e2) = go e1 <> " | " <> go e2
	go (And e1 e2) = go e1 <> " && " <> go e2
	go (Or e1 e2) = go e1 <> " || " <> go e2
	go (Redir e r) = let use = (\t -> go e <> " " <> t) in case r of
		(RedirToFile fd f) ->
			use $ redirFd fd (Just stdOutput) <> "> " <> L.pack f
		(RedirToFileAppend fd f) ->
			use $ redirFd fd (Just stdOutput) <> ">> " <> L.pack f
		(RedirFromFile fd f) ->
			use $ redirFd fd (Just stdInput) <> "< " <> L.pack f
		(RedirOutput fd1 fd2) ->
			use $ redirFd fd1 (Just stdOutput) <> ">&" <> showFd fd2
		(RedirInput fd1 fd2) ->
			use $ redirFd fd1 (Just stdInput) <> "<&" <> showFd fd2
		(RedirHereDoc t)
			| multiline -> 
				let marker = eofMarker t
				in use $ "<<" <> marker <> "\n" <> t <> "\n" <> marker
			-- Here documents cannot be represented in a single
			-- line script. Instead, generate:
			-- (echo l1; echo l2; ...) | cmd
			| otherwise ->
				let heredoc = Subshell L.empty $
					flip map (L.lines t) $ \l -> Cmd $ 
						"echo " <> getQ (quote l)
				in go (Pipe heredoc e)

-- | Displays a Fd for use in a redirection.
-- 
-- Redirections have a default Fd; for example, ">" defaults to redirecting
-- stdout. In this case, the file descriptor number does not need to be
-- included.
redirFd :: Fd -> (Maybe Fd) -> L.Text
redirFd fd deffd
	| Just fd == deffd = ""
	| otherwise = showFd fd

showFd :: Fd -> L.Text
showFd = L.pack . show

-- | Finds an approriate marker to end a here document; the marker cannot
-- appear inside the text.
eofMarker :: L.Text -> L.Text
eofMarker t = go (1 :: Integer)
  where
	go n = let marker = "EOF" <> if n == 1 then "" else L.pack (show n)
		in if marker `L.isInfixOf` t
			then go (succ n)
			else marker

-- | Generates a single line of shell code.
linearScript :: Script f -> L.Text
linearScript = toLinearScript . gen

toLinearScript :: [Expr] -> L.Text
toLinearScript = L.intercalate "; " . map (fmt False)

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
cmd :: (ShellCmd params) => L.Text -> params
cmd c = cmdAll c []

class CmdArg a where
	toTextArg :: a -> (Env -> L.Text)

-- | Text arguments are automatically quoted.
instance CmdArg L.Text where
	toTextArg = const . getQ . quote

-- | String arguments are automatically quoted.
instance CmdArg String where
	toTextArg = toTextArg . L.pack

-- | Any value that can be shown can be passed to 'cmd'; just wrap it
-- inside a Val.
instance (Show v) => CmdArg (Val v) where
	toTextArg (Val v) = const $ L.pack (show v)

-- | Var arguments cause the (quoted) value of a shell variable to be
-- passed to the command.
instance CmdArg Var where
	toTextArg = toTextArg . val

-- | Quoted Text arguments are passed as-is.
instance CmdArg (Quoted L.Text) where
	toTextArg (Q v) = const v

-- | Allows passing the output of a command as a parameter.
instance CmdArg Output where
	toTextArg (Output s) = \env ->
		let t = toLinearScript $ fst $ runScript env s
		in "\"$(" <> t <> ")\""

class ShellCmd t where
	cmdAll :: L.Text -> [Env -> L.Text] -> t

instance (CmdArg arg, ShellCmd result) => ShellCmd (arg -> result) where
	cmdAll c acc x = cmdAll c (toTextArg x : acc)

instance (f ~ ()) => ShellCmd (Script f) where
	cmdAll c acc = Script $ \env -> 
		let ps = map (\f -> f env) (reverse acc)
		in ([Cmd $ L.intercalate " " (c:ps)], env, ())

-- | The output of a command, or even a more complicated Script
-- can be passed as a parameter to 'cmd'
--
-- Examples:
--
-- > cmd "echo" "hello there," (Output (cmd "whoami"))
-- > cmd "echo" "root's pwent" (Output (cmd "cat" "/etc/passwd" -|- cmd "grep" "root"))
newtype Output = Output (Script ())

-- | An arbitrary value.
newtype Val v = Val v

-- | Adds an Expr to the script.
add :: Expr -> Script ()
add expr = Script $ \env -> ([expr], env, ())

-- | Adds a comment that is embedded in the generated shell script.
comment :: L.Text -> Script ()
comment = add . Comment

-- | Suggests that a shell variable or function have its name contain
-- the specified Text.
newtype NamedLike = NamedLike L.Text

-- | Class of values that provide a hint for the name to use for a shell
-- variable or function.
--
-- To skip providing a hint, use '()'.
-- To provide a hint, use '(NamedLike \"name\")'.
class NameHinted h where
	hinted :: (Maybe L.Text -> a) -> h -> a

instance NameHinted () where
	hinted f _ = f Nothing

instance NameHinted NamedLike where
	hinted f (NamedLike h) = f (Just h)

instance NameHinted (Maybe L.Text) where
	hinted = id

-- | Defines a new shell variable.
--
-- Each call to newVar will generate a new, unique variable name.
--
-- The namehint can influence this name, but is modified to ensure
-- uniqueness.
newVar :: (NameHinted namehint) => namehint -> Script Var
newVar = hinted $ \namehint -> Script $ \env ->
	let v = go namehint env (0 :: Integer)
	in ([], modifyEnvVars env (S.insert v), v)
  where
	go namehint env x
		| S.member v (envVars env) = go namehint env (succ x)
		| otherwise = v
	  where
		v = Var $ "_"
			<> genvarname namehint
			<> if x == 0 then "" else L.pack (show (x + 1))
	
	genvarname = maybe "v" (L.filter isAlpha)

-- | Creates a new shell variable, with an initial value.
newVarContaining :: (NameHinted namehint) => L.Text -> namehint -> Script Var
newVarContaining value = hinted $ \namehint -> do
	v@(Var name) <- newVar namehint
	Script $ \env -> ([Cmd (name <> "=" <> getQ (quote value))], env, v)

-- | Gets a Var that refers to a global variable, such as PATH
globalVar :: L.Text -> Script Var
globalVar name = Script $ \env -> let v = Var name in ([], modifyEnvVars env (S.insert v), v)

-- | This special Var expands to whatever parameters were passed to the
-- shell script.
--
-- Inside a func, it expands to whatever parameters were passed to the
-- func.
--
-- (This is `$@` in shell)
positionalParameters :: Var
positionalParameters = Var "@"

-- | Takes the first positional parameter, removing it from
-- positionalParameters and returning a new Var that holds the value of the
-- parameter.
--
-- If there are no more positional parameters, an error will be thrown at
-- runtime.
--
-- For example:
--
-- > removefirstfile = script $ do
-- >   cmd "rm" =<< takeParameter
-- >   cmd "echo" "remaining parameters:" positionalParameters
takeParameter :: (NameHinted namehint) => namehint -> Script Var
takeParameter = hinted $ \namehint -> do
	p@(Var name) <- newVar namehint
	Script $ \env -> ([Cmd (name <> "=\"$1\""), Cmd "shift"], env, p)

-- | Defines a shell function, and returns an action that can be run to
-- call the function.
--
-- The action is variadic; it can be passed any number of CmdArgs.
-- Typically, it will make sense to specify a more concrete type
-- when defining the shell function.
--
-- The shell function will be given a unique name, that is not used by any
-- other shell function. The namehint can be used to influence the contents
-- of the function name, which makes for more readable generated shell
-- code.
--
-- For example:
--
-- > demo = script $ do
-- >    hohoho <- mkHohoho
-- >    hohoho (Val 1)
-- >    echo "And I heard him exclaim, ere he rode out of sight ..."
-- >    hohoho (Val 3)
-- > 
-- > mkHohoho :: Script (Val Int -> Script ())
-- > mkHohoho = func (NamedLike "hohoho") $ do
-- >    num <- takeParameter
-- >    forCmd (cmd "seq" "1" num) $ \_n ->
-- >       cmd "echo" "Ho, ho, ho!" "Merry xmas!"
func
	:: (NameHinted namehint, ShellCmd callfunc)
	=> namehint
	-> Script ()
	-> Script callfunc
func h s = flip hinted h $ \namehint -> Script $ \env ->
	let f = go (genfuncname namehint) env (0 :: Integer)
	    env' = modifyEnvFuncs env (S.insert f)
	    (ls, env'') = runScript env' s
	in (definefunc f ls, env'', callfunc f)
  where
	go basename env x
		| S.member f (envFuncs env) = go basename env (succ x)
		| otherwise = f
	  where
		f = Func $ "_"
			<> basename
			<> if x == 0 then "" else L.pack (show (x + 1))
	
	genfuncname = maybe "p" (L.filter isAlpha)

	definefunc (Func f) ls = (Cmd $ f <> " () { :") : map indent ls ++ [ Cmd "}" ]

	callfunc (Func f) = cmd f

-- | Runs the command, and separates its output into parts
-- (using the IFS)
--
-- The action is run for each part, passed a Var containing the part.
forCmd :: Script () -> (Var -> Script ()) -> Script ()
forCmd c a = do
	v@(Var vname) <- newVar (NamedLike "x")
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
	go c@(Cmd _) = Or c true
	go c@(Comment _) = c
	go (Subshell i l) = Subshell i (map go l)
	-- Assumes pipefail is not set.
	go (Pipe e1 e2) = Pipe e1 (go e2)
	-- Note that in shell, a && b || true will result in true;
	-- there is no need for extra parens.
	go c@(And _ _) = Or c true
	go (Or e1 e2) = Or e1 (go e2)
	go (Redir e r) = Redir (go e) r

	true = Cmd "true"

-- | Pipes together two Scripts.
(-|-) :: Script () -> Script () -> Script ()
(-|-) = combine Pipe

-- | ANDs two Scripts.
(-&&-) :: Script () -> Script () -> Script ()
(-&&-) = combine And

-- | ORs two Scripts.
(-||-) :: Script () -> Script () -> Script ()
(-||-) = combine Or

combine :: (Expr -> Expr -> Expr) -> Script () -> Script () -> Script ()
combine f a b = do
	alines <- runM a
	blines <- runM b
	add $ f (toSingleExp alines) (toSingleExp blines)

toSingleExp :: [Expr] -> Expr
toSingleExp [e] = e
toSingleExp l = Subshell L.empty l

redir :: Script () -> RedirSpec -> Script ()
redir s r = do
	e <- toSingleExp <$> runM s
	add $ Redir e r

-- | Any function that takes a RedirFile can be passed a
-- a FilePath, in which case the default file descriptor will be redirected
-- to/from the FilePath.
--
-- Or, it can be passed a tuple of (Fd, FilePath), in which case the
-- specified Fd will be redirected to/from the FilePath.
class RedirFile r where
	fromRedirFile :: Fd -> r -> (Fd, FilePath)

instance RedirFile FilePath where
	fromRedirFile = (,)

instance RedirFile (Fd, FilePath) where
	fromRedirFile = const id

fileRedir :: RedirFile f => f -> Fd -> (Fd -> FilePath -> RedirSpec) -> RedirSpec
fileRedir f deffd c = uncurry c (fromRedirFile deffd f)

-- | Redirects to a file, overwriting any existing file.
--
-- For example, to shut up a noisy command:
--
-- > cmd "find" "/" |> "/dev/null"
(|>) :: RedirFile f => Script () -> f -> Script ()
s |> f = redir s (fileRedir f stdOutput RedirToFile)

-- | Appends to a file. (If file doesn't exist, it will be created.)
(|>>) :: RedirFile f => Script () -> f -> Script ()
s |>> f = redir s (fileRedir f stdOutput RedirToFileAppend)

-- | Redirects standard input from a file.
(|<) :: RedirFile f => Script () -> f -> Script ()
s |< f = redir s (fileRedir f stdInput RedirFromFile)

-- | Redirects the first file descriptor to output to the second.
--
-- For example, '(cmd "foo", stdOutput) |>& stdError' will
-- redirect a command's stdout to stderr.
(|>&) :: (Script (), Fd) -> Fd -> Script ()
(s, fd1) |>& fd2 = redir s (RedirOutput fd1 fd2)

-- | Redirects a script's output to stderr.
toStderr :: Script () -> Script ()
toStderr s = (s, stdOutput) |>& stdError

-- | Redirects the first file descriptor to input from the second.
(|<&) :: (Script (), Fd) -> Fd -> Script ()
(s, fd1) |<& fd2 = redir s (RedirInput fd1 fd2)

-- | Provides the Text as input to the Script, using a here-document.
hereDocument :: Script () -> L.Text -> Script ()
hereDocument s t = redir s (RedirHereDoc t)
