module Main (main) where

import Control.Monad (filterM)
import Data.List (isSuffixOf)
import Options.Applicative
import System.Directory
import System.FilePath ((</>))
import XData
import Terraform.TerraformParser
import Control.Monad (forM)

-- File Types that we can analyze
data FileType = TF | JVM
  deriving (Show, Eq)

-- File extensions for each file type
extensionsForFileType :: FileType -> [String]
extensionsForFileType TF = [".tf"]
extensionsForFileType JVM = [".java", ".kt"]

-- Parse file type  from string
parseFileType :: ReadM FileType
parseFileType = eitherReader $ \s ->
  case s of
    "TF" -> Right TF
    "JVM" -> Right JVM
    _ -> Left "Invalid File Type"

-- Command-line options
data Options = Options
  { folder :: String,
    fileType :: FileType,
    recursive :: Bool
  }
  deriving (Show)

-- Parser for command-line options
parseOptions :: Parser Options
parseOptions =
  Options
    <$> strOption
      ( long "folder"
          <> short 'f'
          <> metavar "FOLDER"
          <> help "Source folder"
      )
    <*> option
      parseFileType
      ( long "fileType"
          <> short 't'
          <> metavar "FILE_TYPE"
          <> help "File Type (TF or JVM)"
      )
    <*> switch
      ( long "recursive"
          <> short 'r'
          <> help "Whether to scan subdirectories"
      )

-- Recursively find files with the given extension
findFilesWithExtensions :: FilePath -> [String] -> Bool -> IO [FilePath]
findFilesWithExtensions folderPath extensions recur = do
  entries <- listDirectory folderPath
  let fullPaths = map (folderPath </>) entries
  let matchingFullPaths = filter hasMatchingExtension fullPaths

  subdirectories <- filterM doesDirectoryExist fullPaths
  subFiles <-
    if recur
      then concat <$> mapM (\subdir -> findFilesWithExtensions subdir extensions recur) subdirectories
      else return []

  return (matchingFullPaths ++ subFiles)
  where
    hasMatchingExtension :: FilePath -> Bool
    hasMatchingExtension filePath =
      any (\extension -> extension `isSuffixOf` filePath) extensions

-- ENTRY POINT!
main :: IO ()
main = do
  options <- execParser (info parseOptions fullDesc)
  putStrLn $ "Folder: " <> folder options
  putStrLn $ "fileType: " <> (show . fileType) options
  putStrLn $ "recursive: " <> (show . recursive) options

  files <- findFilesWithExtensions (folder options) ((extensionsForFileType . fileType) options) (recursive options)
  _ <- forM files $ \fileName -> do
    putStrLn fileName
    fileText <- readFile fileName
    let db = parseFile (fileType options) fileName fileText
    putStrLn (show db)
    return db

  let filePath = "xref.json"
  let db = sampleDb
  putStrLn ("Writing " ++ (show (length (defs db))) ++ " symbols to " ++ filePath ++ " ...")
  writeDatabaseToFile filePath db
  putStrLn "... done"

parseFile :: FileType -> String -> String -> Database
parseFile TF fileName fileText = 
  let 
    decls :: [TfDeclaration]
    decls = parseTF fileName fileText
  in
    Database [(Def "size" $ (show . length) decls)] []

parseFile fType _ _ = error $ "file type " <> (show fType) <> " not implemented"
