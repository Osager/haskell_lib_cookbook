import System.Console.Haskeline
import Control.Monad.IO.Class

main :: IO ()
main = do
	putStrLn ""
	putStrLn "Updating from the internet. Please wait..."
	--updateTextBase
	putStrLn "done!"

	runInputT defaultSettings loop



loop :: InputT IO ()
loop = do
	nextText "Suddenly, there is a new economist making waves – and he is not on the right. At the conference of the Institute of New Economic Thinking in Toronto last week, Thomas Piketty's book Capital in the Twenty-First Century got at least one mention at every session I attended. You have to go back to the 1970s and Milton Friedman for a single economist to have had such an impact."           
	minput <- getInputLine "please type your answer: "
	case minput of
		Nothing -> return ()
		Just "quit" -> return ()
		Just input -> do 
			outputStrLn $ "Input was: " ++ input 
			loop

nextText text = runInputT defaultSettings $ do
	outputStrLn ""
	outputStrLn ""
	outputStrLn text
	outputStrLn ""
	outputStrLn "------------------------------------------"

updateTextBase = undefined