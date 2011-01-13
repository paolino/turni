{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}
module Generazione.Parser (parseGenerazione, Generazione (..)) where

import Text.ParserCombinators.ReadP (ReadP, char, string, skipSpaces, look, satisfy, sepBy, 
		many, (<++), munch1, readP_to_S, readS_to_P)
import Data.Char (isSpace, isAlpha)
import Control.Monad (guard, liftM2, msum)
-- import Control.Monad.Instances
import Data.List (lookup, isPrefixOf, find)

import Generazione (index, Personale (..), Turno (..), Counter, Associazione, PrimoSecondo (..))
import Generazione.Validazione

header x = do	
	skipSpaces
	string x
	skipSpaces
	char ':'

rigaVuota f = do 
	look >>= guard . not . isPrefixOf "\n\n" 
	(satisfy isSpace >> rigaVuota f) <++ f

stanza x f = header x >> rigaVuota f `sepBy` char ','

nome = munch1 isAlpha

turni = flip zip [1..] `fmap` stanza "Turni" nome

personale = flip zip [1..] `fmap` stanza "Personale" nome

listaPersonale ps = resolvePersonale ps `sepBy` char '-'
listaTurni ts = resolveTurno ts `sepBy` char '-'
listaInteri = (readS_to_P reads :: ReadP Int) `sepBy` char '-'

assocs f g = do
	x <- f
	char '-'
	y <- g
	return (x,y)

resolveString k zs = do
	s <- munch1 isAlpha
	case lookup s zs of
		Just n -> return n
		Nothing -> error $ s ++ " non è un nome valido come " ++ k
	
resolvePersonale = fmap Personale . resolveString "personale"
resolveTurno = fmap Turno . resolveString "turno"


sincronizzatiP = stanza "Sincronizzati" . listaPersonale

obbligatoriP ps ts = stanza "Obbligatori" $ assocs (resolvePersonale ps) (resolveTurno ts)

impossibiliP ps ts = stanza "Impossibili" $ assocs (resolvePersonale ps) (resolveTurno ts)


occupazionePersonaleP ps = stanza "OccupazionePersonale" $ assocs (resolvePersonale ps) (readS_to_P reads :: ReadP Int)

occupazioneTurniP ts = stanza "OccupazioneTurni" $ assocs (resolveTurno ts) (readS_to_P reads :: ReadP Int)

pauseVicineP = stanza "PauseVicine" . resolvePersonale 

pauseLontaneP = stanza "PauseLontane" . resolvePersonale

splitP ts = stanza "Split" $ assocs (resolveTurno ts) $ assocs (readS_to_P reads :: ReadP Int)
 (readS_to_P reads :: ReadP Int)

campoNuovo f = do 
	look >>= guard . not . liftM2 (||) (isPrefixOf "\n\t") (isPrefixOf "\n\n")
	(satisfy isSpace >> campoNuovo f) <++ f

listaTurniV ts = campoNuovo (resolveTurno ts) `sepBy` char ','
listaPersonaleV ps = campoNuovo (resolvePersonale ps) `sepBy` char ','

primoSecondoP ts ps = do 
	header "PrimoSecondo" 
	header "Turno"
	tus <- listaTurniV ts	
	header "PersonalePresente"
	prs <- listaPersonaleV ps
	header "PersonaleAssente"
	pas <- listaPersonaleV ps
	header "Primo"
	primo <- listaPersonaleV ps
	header "Secondo"
	secondo <- listaPersonaleV ps 
	return $ PrimoSecondo tus prs pas primo secondo

mapOf xs x = maybe (error "incongruenza nella risoluzione dei riferimenti ai nomi") fst . 
	find ((==) (index x) . snd) $ xs
mapSplit xs x = maybe (error "incongruenza nella ricerca dello split") snd . 
	find ((==) x . fst) $ xs

parse = do 
	ts <- turni
	ps <- personale 
	ss <- sincronizzatiP ps 
	is <- impossibiliP ps ts 
	os <- obbligatoriP ps ts
	ops <- occupazionePersonaleP ps
	ots <- occupazioneTurniP ts
	pv <- pauseVicineP ps
	pl <- pauseLontaneP ps
	spl <- splitP ts
	psps <- many $ primoSecondoP ts ps
	return $ Generazione (mapOf ts) (mapOf ps) ss is os ops ots pv pl (mapSplit spl) psps

-- | prova a deserializzare una Generazione	
parseGenerazione :: String -> Either String Generazione
parseGenerazione t = case flip readP_to_S t parse  of 
	[] -> Left "errore indefinito nella sintassi del file"
	xs -> case validazione (fst . last $ xs) of
		Nothing -> Right $ fst . last $ xs
		Just e -> Left e

data Generazione = Generazione {
	mappaTurni	:: Turno -> String,
	mappaPersonale  :: Personale -> String,
        sincronizzati 	:: [[Personale]],
        impossibili 	:: [Associazione],
        obbligatori	:: [Associazione],
        occupazionePersonale :: Counter Personale,
        occupazioneTurni :: Counter Turno,
        pauseVicine :: [Personale],
        pauseLontane :: [Personale],
	splitting :: (Turno -> (Int,Int)),
	primoSecondo :: [PrimoSecondo]	
	} 

-- | controlla la validita' di una configurazione
validazione :: 	Generazione 	-- ^ configurazione da controllare
		-> Maybe String	-- ^ Nothing se è corretta altrimenti una descrizione dell'errore in Just
validazione (Generazione _ _ si imp obb occp occt pv pl spl prses) =
	msum $ [valida'Sincronizzati si, valida'Impossibili'Obbligatori imp obb, valida'monteTurni occp occt,
		valida'pause pv pl, valida'split spl occt] ++ map  (uncurry valida'primoSecondo) (zip [1..] prses)
