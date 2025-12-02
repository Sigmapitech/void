{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Main (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (intercalate)
import qualified Distribution.Compat.NonEmptySet as NES
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.Component (Component (..))
import Distribution.Verbosity (deafening)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    listDirectory,
  )
import System.FilePath
  ( takeExtension,
    takeFileName,
    (</>),
  )
import Text.Regex.TDFA ((=~))

data ComputedComponent = ComputedComponent
  { componentName :: String,
    componentDependencies :: [String],
    componentReverseDependencies :: [String],
    componentType :: String
  }
  deriving (Show)

instance ToJSON ComputedComponent where
  toJSON comp =
    object
      [ "name" .= componentName comp,
        "deps" .= componentDependencies comp,
        "rev-deps" .= componentReverseDependencies comp
      ]

classify :: String -> String -> FilePath -> IO [FilePath]
classify rejectedNamePattern allowedNamePattern path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  case (takeFileName path ++ (if isFile then "." ++ takeExtension path else ""), isDir) of
    (name, _)
      | name =~ rejectedNamePattern -> pure []
      | name =~ allowedNamePattern -> pure [path]
    (_, True) -> findCabalFiles path
    _ -> pure []

findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles dir =
  listDirectory dir >>= \entries ->
    let paths = map (dir </>) entries
     in concat <$> mapM (classify rejectedNamePattern allowedNamePattern) paths
  where
    rejectedNamePattern :: String
    rejectedNamePattern = "^\\..*|^dist-newstyle$"
    allowedNamePattern :: String
    allowedNamePattern = ".*\\.cabal"

parseCabalFile :: FilePath -> IO GenericPackageDescription
parseCabalFile = readGenericPackageDescription deafening

computeComponentDeps :: String -> Component -> ComputedComponent
computeComponentDeps pkg = dispatcher
  where
    retrieveLibName lib = case libName lib of
      LMainLibName -> "library"
      LSubLibName ucn -> ucn

    dispatcher :: Component -> ComputedComponent
    dispatcher (CLib lib) = wrap "lib" lib retrieveLibName libBuildInfo
    dispatcher (CExe exe) = wrap "exe" exe exeName buildInfo
    dispatcher (CTest ts) = wrap "test" ts testName testBuildInfo
    dispatcher (CBench bk) = wrap "benchmark" bk benchmarkName benchmarkBuildInfo
    dispatcher (CFLib lib) = wrap "flib" lib foreignLibName foreignLibBuildInfo

    wrap ctype blob nameRetriever dependencyRetriever =
      ComputedComponent
        { componentName = pkg ++ ":" ++ unUnqualComponentName (nameRetriever blob),
          componentType = ctype,
          componentDependencies = map depToString (targetBuildDepends (dependencyRetriever blob)),
          componentReverseDependencies = []
        }

convertPackageDescToComponents :: PackageDescription -> [ComputedComponent]
convertPackageDescToComponents pd = map (computeComponentDeps pkg) extractComponents
  where
    pkg = unPackageName (pkgName (package pd))
    extractComponents =
      maybe [] (pure . CLib) (library pd)
        ++ map CLib (subLibraries pd)
        ++ map CExe (executables pd)
        ++ map CTest (testSuites pd)
        ++ map CBench (benchmarks pd)
        ++ map CFLib (foreignLibs pd)

depToString :: Dependency -> String
depToString dep =
  intercalate "," $
    map fmt (NES.toList (depLibraries dep))
  where
    depPkg = unPackageName (depPkgName dep)
    fmt LMainLibName = depPkg
    fmt (LSubLibName subname) = depPkg ++ ":" ++ unUnqualComponentName subname

buildReverseDependencies :: [ComputedComponent] -> [ComputedComponent]
buildReverseDependencies comps = map addRevDeps comps
  where
    deps = concatMap (\comp -> map (\d -> (d, componentName comp)) (componentDependencies comp)) comps

    addRevDeps comp =
      comp
        { componentReverseDependencies =
            foldl
              ( \acc (dep, parent) ->
                  if dep == componentName comp
                    then acc ++ [parent]
                    else acc
              )
              []
              deps
        }

removeExternalDeps :: [ComputedComponent] -> [ComputedComponent]
removeExternalDeps = map removeExt
  where
    removeExt comp =
      comp
        { componentDependencies = filter (':' `elem`) (componentDependencies comp)
        }

main :: IO ()
main =
  getCurrentDirectory >>= findCabalFiles >>= \cabalFiles -> do
    gpds <- mapM parseCabalFile cabalFiles
    let fpds = map flattenPackageDescription gpds
        cc = concatMap convertPackageDescToComponents fpds
    BL.putStrLn (encode (removeExternalDeps (buildReverseDependencies cc)))
