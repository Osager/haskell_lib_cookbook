import System.Random

instance (Random x, Random y, Random z) => Random (x, y,z) where
  randomR ((x1, y1,z1), (x2, y2,z2)) gen1 =
    let (x, gen2) = randomR (x1, x2) gen1
        (y, gen3) = randomR (y1, y2) gen2
        (z, gen4) = randomR (z1, z2) gen3
    in ((x, y,z), gen4)

main = do
	let pair = take 10 $ randomRs ((10,20,30),(60,40,60)) $ mkStdGen 10  :: [(Int,Int,Int)]
	print pair
