import Control.Monad
import Data.List
import System.Directory

-- a line is a type signature if it contains :: but doesn't contain =
isTypeSignature :: String -> Bool
isTypeSignature s = not (isInfixOf "=" s) && isInfixOf "::" s

-- return list of types for a .hs file
readTypesFile :: FilePath -> IO [String]
readTypesFile file
  | isSuffixOf ".hs" file = do
    content <- readFile file
    let ls = lines content
    return (filter isTypeSignature ls)
  | otherwise = return []

-- list children of directory, prepend directory name
qualifiedChildren :: String -> IO [String]
qualifiedChildren path = do
  childs <- listDirectory path
  return (map (\name -> path ++ "/" ++ name) childs)

readTypesDir :: String -> IO [String]
readTypesDir path = do
  childs <- qualifiedChildren path
  typess <- forM childs readTypes
  return (concat typess)

-- recursively read types contained in a file or directory
readTypes :: String -> IO [String]
readTypes path = do
  isDir <- doesDirectoryExist path
  if isDir then readTypesDir path else readTypesFile path

main :: IO ()
main = do
  ts <- readTypes "."
  mapM_ putStrLn ts
