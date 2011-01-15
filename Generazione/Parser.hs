{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}
module Generazione.Parser (parseGenerazione, Generazione (..)) where

import Text.ParserCombinators.ReadP (ReadP, char, string, skipSpaces, look, satisfy, sepBy, 
		many, (<++), munch1, readP_to_S, readS_to_P)
import Data.Char (isSpace, isAlpha)
import Control.Monad (guard, liftM2, msum)
import Control.Monad.Instances
import Data.List (isPrefixOf, find)

import Generazione (Index (..), Personale , Turno , Counter, Associazione, PrimoSecondo (..))
import Generazione.Validazione

void :: Monad m => m a -> m ()
void x = x >> return ()

header :: String -> ReadP ()
header x = do	
	skipSpaces
	void $ string x
	skipSpaces
	void $ char ':'

rigaVuota :: ReadP a -> ReadP a
rigaVuota f = do 
	look >>= guard . not . isPrefixOf "\n\n" 
	(satisfy isSpace >> rigaVuota f) <++ f

stanza :: String -> ReadP a -> ReadP [a]
stanza x f = header x >> rigaVuota f `sepBy` char ','

nome :: ReadP String
nome = munch1 isAlpha

type MappaTP = [(String,Int)]

turni :: ReadP MappaTP
turni = flip zip [1..] `fmap` stanza "Turni" nome

personale :: ReadP MappaTP
personale = flip zip [1..] `fmap` stanza "Personale" nome

resolve :: MappaTP -> ReadP (Index a)
resolve zs = do
	s <- munch1 isAlpha
	case lookup s zs of
		Just n -> return (Index n)
		Nothing -> error $ s ++ " non è un nome valido"

listaPersonale :: MappaTP -> ReadP [Personale] 
listaPersonale ps = resolve ps `sepBy` char '-'

assocs :: ReadP a -> ReadP b -> ReadP (a,b)
assocs f g = do
	x <- f
	void $ char '-'
	y <- g
	return (x,y)


sincronizzatiP :: MappaTP -> ReadP [[Personale]]
sincronizzatiP = stanza "Sincronizzati" . listaPersonale

obbligatoriP :: MappaTP -> MappaTP -> ReadP [Associazione]
obbligatoriP ps ts = stanza "Obbligatori" $ assocs (resolve ps) (resolve ts)

impossibiliP :: MappaTP -> MappaTP -> ReadP [Associazione]
impossibiliP ps ts = stanza "Impossibili" $ assocs (resolve ps) (resolve ts)

occupazionePersonaleP :: MappaTP -> ReadP [(Personale,Int)]
occupazionePersonaleP ps = stanza "OccupazionePersonale" $ assocs (resolve ps) (readS_to_P reads)

occupazioneTurniP :: MappaTP -> ReadP [(Turno,Int)]
occupazioneTurniP ts = stanza "OccupazioneTurni" $ assocs (resolve ts) (readS_to_P reads)

pauseVicineP :: MappaTP -> ReadP [Personale]
pauseVicineP = stanza "PauseVicine" . resolve 

pauseLontaneP :: MappaTP -> ReadP [Personale]
pauseLontaneP = stanza "PauseLontane" . resolve

splitP :: MappaTP -> ReadP [(Turno,(Int,Int))]
splitP ts = stanza "Split" $ assocs (resolve ts) $ assocs (readS_to_P reads) (readS_to_P reads)

campoNuovo :: ReadP a -> ReadP a
campoNuovo f = do 
	look >>= guard . not . liftM2 (||) (isPrefixOf "\n\t") (isPrefixOf "\n\n")
	(satisfy isSpace >> campoNuovo f) <++ f

listaTurniV :: MappaTP -> ReadP [Turno]
listaTurniV ts = campoNuovo (resolve ts) `sepBy` char ','

listaPersonaleV :: MappaTP -> ReadP [Personale]
listaPersonaleV ps = campoNuovo (resolve ps) `sepBy` char ','

primoSecondoP :: MappaTP -> MappaTP -> ReadP PrimoSecondo
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

mapOf :: MappaTP -> Index a -> String
mapOf xs x = maybe (error "incongruenza nella risoluzione dei riferimenti ai nomi") fst . 
	find ((==) (index x) . snd) $ xs

mapSplit :: [(Turno,(Int,Int))] -> Turno  -> (Int,Int)
mapSplit xs x = maybe (error "incongruenza nella ricerca dello split") snd . 
	find ((==) x . fst) $ xs

parse :: ReadP Generazione
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
	splitting :: Turno -> (Int,Int),
	primoSecondo :: [PrimoSecondo]	
	} 

-- | controlla la validita' di una configurazione
validazione :: 	Generazione 	-- ^ configurazione da controllare
		-> Maybe String	-- ^ Nothing se è corretta altrimenti una descrizione dell'errore in Just
validazione (Generazione _ _ si imp obb occp occt pv pl spl prses) =
	msum $ [valida'Sincronizzati si, valida'Impossibili'Obbligatori imp obb, valida'monteTurni occp occt,
		valida'pause pv pl, valida'split spl occt] ++ map  (uncurry valida'primoSecondo) (zip [1..] prses)
