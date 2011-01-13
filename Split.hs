
-- | separazione interna di un turno assegnato
module Split (prettyPersonaleSplit, prettyTurniSplit, split) where

import Data.List (groupBy, sortBy, nub, (\\), splitAt, intersect)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Function (on)
import Control.Arrow ((***),(&&&))
import Control.Monad.Random.Class (MonadRandom)

import Shuffle (shuffle)
import Generazione (PrimoSecondo (..), Turno, Personale)

separatePrimoSecondo :: [PrimoSecondo] -> (Turno,[Personale]) -> Maybe ([Personale],[Personale],[Personale])
separatePrimoSecondo ps x@(_,qs) = let 	
	(p,s) = (nub . concat *** nub . concat) . unzip . mapMaybe (matchPrimoSecondo x) $ ps
	in case intersect p s of
		[] -> Just (p,s,qs \\ (p ++ s))
		_ -> Nothing

matchPrimoSecondo :: (Turno,[Personale]) -> PrimoSecondo -> Maybe ([Personale],[Personale])
matchPrimoSecondo (g,ps) (PrimoSecondo mt mp ma p s) 
		| g `elem` mt = if all (`elem` ps) mp && all (not . (`elem` ps)) ma then Just (p,s) else Nothing
		| otherwise = Nothing


-- | suddivisione di un turno a partire dai vincoli di suddivisione, fallisce se non si creano due insiemi distinti
split :: 	(Functor m, MonadRandom m)
		=> [PrimoSecondo]			-- ^ vincoli di suddivisione
		-> (Turno,[Personale]) 			-- ^ turno da suddividere
		-> Int					-- ^ personale totale primo turno
		-> m (Maybe (Turno,([Personale],[Personale])))	-- ^ turno suddiviso
split qs x@(g,_) n = case separatePrimoSecondo qs x of
			Just (ps,ss,rs) -> do 
				rs' <- shuffle rs
				return . Just $ (g, splitAt n $ ps ++ rs' ++ ss)
			Nothing -> return Nothing

	
-- allunga le stringhe alla misura della massima
square xs = zipAmend (replicate (( maximum . map length $ xs) +1) ' ') where
	zipAmend xs [] = xs
	zipAmend (y:ys) (x:xs) = x: zipAmend ys xs

-- | mappa gli indici di turni e personale indietro ai loro valori e fa una bella stampa, vista turni
prettyTurniSplit :: (Personale -> String) -> (Turno -> String) -> [(Turno,([Personale],[Personale]))] -> String
prettyTurniSplit fp ft xs = let
	(rs,xs') = unzip . map (\(x,(ys,zs)) -> let x' = ft x; ys' = map fp ys ; zs' = map fp zs 
		in (x':ys' ++ zs',(x',ys',zs'))) $ xs
	q = square . concat $ rs
	in concatMap (\(x,ys,zs) -> 	q x ++ " 1°: " ++ concatMap q ys ++ "\n" ++
					q x ++ " 2°: " ++ concatMap q zs ++ "\n") xs' 
-- cambia visione
transposeTurni :: [(Turno,([Personale],[Personale]))] -> [(Personale,[(Bool,Turno)])]
transposeTurni xs = let 
	ys = concatMap (\(g,(ps,ss)) -> map (\p -> (p,(True,g))) ps ++ map (\s -> (s,(False,g))) ss) xs
	in map (fst . head &&& sortBy (comparing snd) . map snd) . groupBy ((==) `on` fst) . sortBy (comparing fst) $ ys

-- | mappa gli indici di turni e personale indietro ai loro valori e fa una bella stampa, vista personale
prettyPersonaleSplit :: (Personale -> String) -> (Turno -> String) -> [(Turno,([Personale],[Personale]))] -> String
prettyPersonaleSplit fp ft xs = let
	(rs,xs') = unzip . map (\(x,ys) -> let x' = fp x; ys' = map (ft . snd) ys in 
		(x':ys',(x',zipWith (\(b,_) y' -> (b,y')) ys ys'))) $ transposeTurni xs
	q = square . concat $ rs
	in concatMap (\(x,ys) -> q x ++ ": " 
		++ concatMap (\(b,y) -> q y ++ "(" ++ (if b then "1°" else "2°") ++ ")  ") ys ++ "\n") xs' 

