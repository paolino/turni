module Riposo (controllaRiposo) where

import Control.Monad (ap)

import Generazione (Personale)



-- | controllo correttezza dei riposi a coppie o singoli
controllaRiposo 	:: [Personale] 		-- ^ elenco del personale che desidera i riposi a coppie 
			-> [Personale] 		-- ^ elenco del personale che desidera i riposi separati
			-> [[Personale]] 	-- ^ lista degli impieghi nei turni
			-> Bool			-- ^ controllo correttezza
controllaRiposo pauseVicine pauseLontane xs = all ($xs) $ map vicini pauseVicine ++ map lontani pauseLontane  where
	vicini z  = any (\(x,y) -> not (z `elem` x) && not (z `elem` y)) . ap zip tail
	lontani z = not . vicini z

