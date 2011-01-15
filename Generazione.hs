
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | definizione principale dei dati comuni ai moduli.
module Generazione (Index (..), Personale , Turno , Counter , Associazione, PrimoSecondo (..)) where

type Counter a =  [(a,Int)] 

type Associazione = (Personale,Turno)

data PrimoSecondo = PrimoSecondo {
	matchTurno :: [Turno],
	matchPresenti :: [Personale],
	matchAssenti :: [Personale],
	primoTurno :: [Personale],
	secondoTurno :: [Personale]
	} deriving Show

data Personale' 
data Turno' 


newtype Index a = Index {index :: Int} deriving (Eq,Show,Ord)

type Personale = Index Personale'
type Turno = Index Turno' 
