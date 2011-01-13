{-# LANGUAGE TupleSections #-}

-- | computazione naive di una soluzione random al problema della scelta delle associazioni.
module Turni (soluzione, fromAssociazioni) where

import Control.Monad (liftM2)
import Control.Monad.Random.Class (MonadRandom)
import Data.List (find, (\\), foldl', sort, groupBy, sortBy, partition)
import Control.Arrow ((&&&))
import Data.Ord (comparing)
import Data.Function (on)
import Generazione (Personale,Turno, Associazione, Counter) 
import Shuffle (shuffle)

-- take a counter down 1 and signal a 0 reached
mark :: Eq a => a -> Counter a -> (Bool, Counter a)
mark x xs = let
	([(_,c)],ys) = partition ((==) x . fst) xs
	in (c - 1 <= 0, (x,c - 1):ys)
	

type Stato = ([Associazione], Counter Personale, Counter Turno, [Associazione])

inserisci :: Stato -> [Associazione] -> Stato
inserisci = foldl' inserisci' where
	inserisci' :: Stato -> Associazione -> Stato
	inserisci' (vs,ps,gs,t) v@(p,g) = let 
		(bp,ps') = mark p ps
		(bg,gs') = mark g gs
		t' = if bp then filter ((/=) p . fst) t else t
		t'' = if bg then filter ((/=) g . snd) t' else t'
		in (v:vs,ps',gs',t'')

risolvi :: [[Personale]] -> Stato -> Maybe [Associazione]
risolvi _ (vs,ps,gs,[]) = if all ((==) 0 . snd) ps && all ((==) 0 . snd) gs then Just . reverse $ vs
		else Nothing
risolvi sincronizzati (vs,ps,gs,ts@((p,g):_)) = risolvi sincronizzati $ inserisci (vs,ps,gs,ts \\ xs) xs where
	xs = map (,g) $ sincronizzato p
	sincronizzato :: Personale -> [Personale]
	sincronizzato x = maybe [x] id $ find (x `elem`) sincronizzati 



fromAssociazioni :: [Associazione] -> [(Turno,[Personale])] 
fromAssociazioni =  map (snd . head &&& sort . map fst) . groupBy ((==) `on`  snd) . sortBy (comparing snd)


-- | calcola una soluzione random a partire dai vincoli
soluzione 	:: (Functor m , MonadRandom m)
		=> [[Personale]]  	-- ^ vincolo sui gruppi indivisibili
		-> [Associazione] 	-- ^ associazioni impossibili
		-> [Associazione] 	-- ^ associazioni obbligate
		-> Counter Personale 	-- ^ impiego del personale per ogni persona nell'insieme di turni
		-> Counter Turno	-- ^ impiego del personale all'interno di ogni turno	 
		-> m (Maybe [Associazione]) -- ^ una soluzione random che soddisfa i vincoli

soluzione sincronizzati negativi positivi occupazione presenze = do
	b <- shuffle $ liftM2 (,) (map fst occupazione) (map fst presenze) \\ (negativi ++ positivi)
	return . risolvi sincronizzati . inserisci ([],occupazione, presenze,b) $ positivi
 
	
	





