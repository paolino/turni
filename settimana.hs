
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO (hClose, hPutStrLn, Handle, openFile, IOMode (WriteMode), stdout)
import Control.Monad (foldM)

import Split (split,prettyPersonaleSplit, prettyTurniSplit)
import Turni (soluzione, fromAssociazioni)
import Generazione.Parser (parseGenerazione, Generazione (..))
import Riposo (controllaRiposo)

data Flag =  Flag String Handle Int 
	

options :: [OptDescr (Flag -> IO Flag)]
options =
	[ Option ['o']     ["output"]  (ReqArg outp "FILE")  "output FILE"
	, Option ['i']     ["input"]   (ReqArg inp  "FILE")  "input FILE"
	, Option ['n'] 	   ["cicli"]  (ReqArg  cic "NUMERO") "numero di soluzioni calcolate"
	]

cic, outp, inp :: String -> Flag -> IO Flag
cic s (Flag i o _) = case reads s of
	[] -> error "impossibile decifrare il numero di soluzioni da calcolare"
	((x,_):_) -> return (Flag i o x) 

outp s (Flag i _ n) = do
	h <- openFile s WriteMode
	return (Flag i h n)

inp s (Flag _ o n) = do
	h <- readFile s
	return (Flag h o n)
	
    
opts :: [String] -> IO Flag
opts argv = 
   case getOpt Permute options argv of
      (o,_,[]  ) -> do
		h <- getContents
		foldM (flip ($)) (Flag h stdout 100) o
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Utilizzo: settimana [OPTION...]"



turn :: Int -> Handle -> Generazione -> IO Bool
turn m o c = do  
	ms <- soluzione (sincronizzati c) (impossibili c) (obbligatori c) (occupazionePersonale c) (occupazioneTurni c)
	case ms of 
		Just s -> let s' = fromAssociazioni s in
			if controllaRiposo (pauseVicine c) (pauseLontane c) $ map snd s' 
			then do 
				ms'' <- mapM (\x@(t,_) -> split (primoSecondo c) x (fst $ splitting c t)) s'
				case sequence ms'' of
					Nothing -> return False
					Just s'' -> do 
						hPutStrLn o $ "Soluzione: " ++ show m ++ "\n"
						hPutStrLn o $ prettyTurniSplit (mappaPersonale c) (mappaTurni c) s''
						hPutStrLn o $ prettyPersonaleSplit (mappaPersonale c) (mappaTurni c) s''
						return True
			else return False
		Nothing -> return False
								
main :: IO ()
main = do 
	Flag i o n <- getArgs >>= opts
	case parseGenerazione i  of 
		Left e -> error e
		Right c -> let 	f 0 = return ()
				f m = do
					b <- turn (n - m + 1) o c
					f (m - if b then 1 else 0)
			in f n
	hClose o
