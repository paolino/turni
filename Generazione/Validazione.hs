module Generazione.Validazione where

import Data.List (intersect,(\\))

import Generazione (Personale,Associazione,Turno, PrimoSecondo (..))

valida'Sincronizzati :: [[Personale]] -> Maybe String
valida'Sincronizzati pss = 
	if null $ foldr intersect [] pss then Nothing else Just "insiemi di personale sincronizzato sovrapposti"

valida'Impossibili'Obbligatori :: [Associazione] -> [Associazione] -> Maybe String
valida'Impossibili'Obbligatori is os = case intersect is os of
	[] -> Nothing
	_ -> Just "associazioni impossibili e obbligatorie sovrapposte"

valida'monteTurni :: [(Personale,Int)] -> [(Turno,Int)] -> Maybe String
valida'monteTurni xs zs = case sum (map snd xs) == sum (map snd zs) of
	True -> Nothing
	False -> Just "monte occupazione personale e turni diverso"

valida'pause :: [Personale] -> [Personale] -> Maybe String
valida'pause xs ys = case intersect xs ys of
	[] -> Nothing
	_ -> Just "filtri pause sovrapposti"

valida'split :: (Turno -> (Int,Int)) -> [(Turno,Int)] -> Maybe String
valida'split sps ys = case any (\(t,n) -> let (x,y) = sps t in x + y /= n) ys of
	True -> Just "suddivisione interna turni incoerente con monte occupazione turni"
	False -> Nothing

valida'primoSecondo :: Int -> PrimoSecondo -> Maybe String
valida'primoSecondo n (PrimoSecondo _ mp ma p s) = case intersect mp ma of
	[] -> case (p ++ s) \\ mp of
		[] -> Nothing
		_ -> Just $ "suddivisioni filtro primo secondo incoerenti con le presenze. Filtro " ++ show n
	_ -> Just $ "presenze e assenze sovrapposte in filtro primo secondo. Filtro " ++ show n


