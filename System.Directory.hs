module Main where


import Control.Monad
import System.Directory(getDirectoryContents,copyFile,doesDirectoryExist,doesFileExist)
import System.FilePath(replaceDirectory,(</>))

import System.IO
import Control.Exception


handler :: IOException -> IO[FilePath]
handler e = do
	putStrLn "Oops there's some error: "
	print $ show e
	return []

path :: FilePath
path = "/home/nut/dev/haskell/"

main :: IO()
main = do 
	walk path

-- imperative style
walk :: FilePath -> IO()
walk dir = do
	contentsFullPath <- handle handler $ getDirectoryContents dir >>= removeDotFile >>= getFullPath
	dirList <- filterM doesDirectoryExist contentsFullPath
	fileList <- filterM doesFileExist contentsFullPath
	forM_ dirList walk >> forM_ fileList processFile			-- processFile: different file processing code
	where
		removeDotFile = return . filter (`notElem` ["..", "."])
		getFullPath = return . zipWith ( </> ) (repeat dir)
		processFile = myFileProcessor


-- different file processing code
getFileSize :: FilePath -> IO()
getFileSize path = withFile path ReadMode $ \h -> do
	size <- hFileSize h
	print path
	print $ show (quot size 1000) ++ "k"

myFileProcessor = print


