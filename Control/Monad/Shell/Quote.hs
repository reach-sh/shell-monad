-- | Shell quoting

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances #-}

module Control.Monad.Shell.Quote (
	Quoted(..),
	Quotable(..),
	Val(..),
) where

import qualified Data.Text.Lazy as L
import Data.Monoid
import Data.Char

-- | A value that is safely quoted so that it can be exposed to the shell.
--
-- While the constructor is exposed, you should avoid directly constucting
-- Quoted values. Instead, use 'quote'.
newtype Quoted a = Q { getQ :: a }
	deriving (Eq, Ord, Show, Monoid)

-- | Quotes a value to allow it to be safely exposed to the shell.
--
-- The method used is to replace ' with '"'"' and wrap the value inside
-- single quotes. This works for POSIX shells, as well as other shells
-- like csh.
class Quotable t where
	quote :: t -> Quoted L.Text

instance Quotable L.Text where
	quote t
		| L.all (\c -> isAlphaNum c || c == '_') t = Q t
		| otherwise = Q $ q <> L.intercalate "'\"'\"'" (L.splitOn q t) <> q
	  where
		q = "'"

instance Quotable String where
	quote = quote . L.pack

-- | Any Showable value can be quoted, just use 'quote (Val v)'
instance (Show v) => Quotable (Val v) where
	quote (Val v) = quote $ show v

-- To avoid double-quoting Text and String, override the above instance.
-- This needs OverlappingInstances
instance Quotable (Val L.Text) where
	quote (Val s) = quote s
instance Quotable (Val String) where
	quote (Val s) = quote s

-- | An arbitrary value.
newtype Val v = Val v
