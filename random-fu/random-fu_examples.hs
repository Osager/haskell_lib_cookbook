import Data.Random
import Data.Random.Source.IO

main = sample (randomElement [0..9]) >>= print