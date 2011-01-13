
{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}

-- | definizione principale dei dati comuni ai moduli.
module Generazione (index, Personale (..) , Turno (..) , Counter (..) , Associazione, PrimoSecondo (..)) where

import Control.Monad (msum)
-- import Control.Monad.Instances
import Data.List (lookup, (\\), intersect)


class Index a where
	index :: a -> Int

instance Index Int where
	index = id

newtype Personale = Personale Int deriving (Eq,Ord,Show,Read,Index)

newtype Turno = Turno Int deriving (Eq,Ord,Show,Read,Index)

type Counter a =  [(a,Int)] 

type Associazione = (Personale,Turno)

data PrimoSecondo = PrimoSecondo {
	matchTurno :: [Turno],
	matchPresenti :: [Personale],
	matchAssenti :: [Personale],
	primoTurno :: [Personale],
	secondoTurno :: [Personale]
	} deriving Show



