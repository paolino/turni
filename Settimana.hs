import Control.Monad (forM)


import Split (split,prettyPersonaleSplit, prettyTurniSplit)
import Turni (soluzione, fromAssociazioni)
import Generazione.Parser (parseGenerazione, Generazione (..))
import Riposo (controllaRiposo)


main :: IO ()
main = do 
	ec <- parseGenerazione `fmap` readFile "dati2.txt"
	case ec of 
		Left e -> error e
		Right (	Generazione mapT mapP sincronizzati impossibili obbligatori occupazionePersonale
				occupazioneTurni pauseVicine pauseLontane splitting primoSecondo) -> 
			let 
			r = do
				s <- soluzione sincronizzati impossibili obbligatori occupazionePersonale 
					occupazioneTurni
				case s of 
					Just s -> let s' = fromAssociazioni s in
						if controllaRiposo pauseVicine pauseLontane $ map snd s' 
						then do 
							ms'' <- mapM (\x@(t,_) -> split primoSecondo x (fst $ splitting t)) s'
							case sequence ms'' of
								Nothing -> return ()
								Just s'' -> do 
									putStrLn $ prettyTurniSplit mapP mapT s''
									putStrLn $ prettyPersonaleSplit mapP mapT s''
						else return ()		
					Nothing -> return ()
				r
			in r	

