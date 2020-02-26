-- | Shell quoting

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Shell.Quote (
	Quoted(..),
	Quotable(..),
	Val(..),
) where

import qualified Data.Text.Lazy as L
import Data.String
import Data.Char

-- | A value that is safely quoted so that it can be exposed to the shell.
--
-- While the constructor is exposed, you should avoid directly constucting
-- Quoted values. Instead, use 'quote'.
newtype Quoted a = Q { getQ :: a }
	deriving (Eq, Ord, Show, Semigroup, Monoid)

-- | Quotes a value to allow it to be safely exposed to the shell.
--
-- The method used is to replace ' with '"'"' and wrap the value inside
-- single quotes. This works for POSIX shells, as well as other shells
-- like csh.
--
-- The single quotes are omitted for simple values that do not need
-- any quoting.
class Quotable t where
	quote :: t -> Quoted L.Text

instance Quotable L.Text where
	quote t
		| L.all bareable t && L.any bareable t = Q t
		| otherwise = Q $ q <> L.intercalate "'\"'\"'" (L.splitOn q t) <> q
	  where
		q = "'"
		bareable c = isAlphaNum c || c == '_'

instance Quotable String where
	quote = quote . L.pack

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

instance IsString (Quoted L.Text) where
	fromString = quote
