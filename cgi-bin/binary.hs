import System.Environment
import Data.Char
import Numeric
import Web

textToBin t = concat $ map (charToBin) (map ord t) where
	charToBin x = fillZeros $ showIntAtBase 2 intToDigit x "" where
		fillZeros k | length k > 7 = k
        		| otherwise = fillZeros (['0']++k)

main = do
        e <- getEnv "QUERY_STRING"
	--e <- getLine
	let entrada = parseGet e
	let text = valueByKey "text" entrada
	putStrLn "Content-type: text/html\n\n"
        putStrLn $ textToBin text
