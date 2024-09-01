{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Description: Plugin for helping close open imports. -}
module OM.Plugin.Imports (
  plugin,
) where


import Control.Monad (void)
import Data.IORef (readIORef)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Set (Set, member)
import GHC (ModSummary(ms_hspp_file), DynFlags, ModuleName, Name, moduleName)
import GHC.Data.Bag (bagToList)
import GHC.Plugins
  ( GlobalRdrElt(GRE, gre_imp, gre_name, gre_par), HasDynFlags(getDynFlags)
  , ImpDeclSpec(ImpDeclSpec, is_as, is_mod, is_qual), ImportSpec(is_decl)
  , Outputable(ppr), Parent(NoParent, ParentIs)
  , Plugin(pluginRecompile, typeCheckResultAction)
  , PluginRecompile(NoForceRecompile), CommandLineOption, bestImport
  , defaultPlugin, liftIO, moduleEnvToList, nonDetOccEnvElts, showSDoc
  )
import GHC.Tc.Utils.Monad
  ( ImportAvails(imp_mods), TcGblEnv(tcg_imports, tcg_used_gres), MonadIO, TcM
  )
import GHC.Types.Avail (greNamePrintableName)
import GHC.Unit.Module.Imported
  ( ImportedBy(ImportedByUser), ImportedModsVal(imv_all_exports)
  )
import Prelude
  ( Applicative(pure), Bool(False, True), Eq((==)), Foldable(elem)
  , Maybe(Just, Nothing), Monoid(mempty), Num((+)), Ord((>)), Semigroup((<>))
  , ($), (.), (<$>), (||), FilePath, Int, String, concat, otherwise, putStrLn
  , unlines, writeFile
  )
import Safe (headMay)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set


plugin :: Plugin
plugin = defaultPlugin
  { typeCheckResultAction = typeCheckResultActionImpl
  , pluginRecompile = \_ -> pure NoForceRecompile
  }


newtype Options = Options
  { excessive :: Bool
  }


typeCheckResultActionImpl
  :: [CommandLineOption]
  -> ModSummary
  -> TcGblEnv
  -> TcM TcGblEnv
typeCheckResultActionImpl args modSummary env = do
  liftIO (putStrLn ("Generating imports for file: " <> ms_hspp_file modSummary))
  let options = parseOptions args
  used <- getUsedImports env
  flags <- getDynFlags
  void $ writeToDumpFile options (ms_hspp_file modSummary) flags used
  pure env


writeToDumpFile
  :: (MonadIO m)
  => Options
  -> FilePath
  -> DynFlags
  -> Map ModuleImport (Map Name (Set Name))
  -> m (Maybe FilePath)
writeToDumpFile options srcFile flags used =
  liftIO $ do
    let
      filename :: FilePath
      filename = srcFile <> ".full-imports"
    writeFile filename (renderNewImports options flags used)
    pure (Just filename)


getUsedImports
  :: forall m.
     (MonadIO m)
  => TcGblEnv
  -> m (Map ModuleImport (Map Name (Set Name)))
getUsedImports env = do
  rawUsed <- (liftIO . readIORef) (tcg_used_gres env) :: m [GlobalRdrElt]
  let
    {-
      Sometimes, the module from which the name is imported may not
      export the Parent of the name. E.g. Data.List exports 'foldl',
      but not 'Foldable'. So we check to see if the parent is available
      from the module. If it isn't then we just omit the parent. If it
      is, we include the parent with the justification that it provides
      more explicit information to the reader.
    -}
    availableParents :: Map ModuleName (Set Name)
    availableParents =
      Map.unionsWith
        Set.union
        [ Map.singleton
            (moduleName m)
            (Set.singleton (greNamePrintableName name))
        | (m, ibs)
            <- moduleEnvToList . imp_mods . tcg_imports $ env
        , ImportedByUser imv <- ibs
        , GRE { gre_name = name } <- concat . nonDetOccEnvElts . imv_all_exports $ imv
        ]

    used :: Map ModuleImport (Map Name (Set Name))
    used =
      Map.unionsWith
        (Map.unionWith Set.union)
        [ let
            imp :: ImportSpec
            imp = bestImport (bagToList imps)

            modName :: ModuleName
            modImport :: ModuleImport
            (modImport, modName) =
              let
                ImpDeclSpec { is_mod , is_as , is_qual } = is_decl imp
              in
                ( case (is_qual, is_as == is_mod) of
                    (True, True)   -> Qualified is_mod
                    (True, False)  -> QualifiedAs is_mod is_as
                    (False, True)  -> Unqualified is_mod
                    (False, False) -> UnqualifiedAs is_mod is_as
                , is_mod
                )
          in
            Map.singleton
              modImport
              (
                let
                  {-
                    Figure out if we need to omit the parent name because
                    it isn't exported by the module from which the name
                    itself is imported.
                  -}
                  withPossibleParent :: Name -> Map Name (Set Name)
                  withPossibleParent parentName =
                    if
                      Set.member parentName $
                        Map.findWithDefault
                          mempty
                          modName
                          availableParents
                    then
                      Map.singleton parentName (Set.singleton name)
                    else
                      noParent

                  noParent :: Map Name (Set Name)
                  noParent = Map.singleton name mempty
                in
                  case parent of
                    NoParent -> noParent
                    ParentIs parentName ->
                      withPossibleParent parentName
              )
        | GRE
            { gre_name
            , gre_par = parent
            , gre_imp = imps
            } <- rawUsed
        , let
            name :: Name
            name = greNamePrintableName gre_name
        ]
  pure used


data ModuleImport
  = Unqualified
      { name :: ModuleName
      }
  | UnqualifiedAs
      { name :: ModuleName
      , as :: ModuleName
      }
  | Qualified
      { name :: ModuleName
      }
  | QualifiedAs
      { name :: ModuleName
      ,   as :: ModuleName
      }
  deriving stock (Eq, Ord)


renderNewImports
  :: Options
  -> DynFlags
  -> Map ModuleImport (Map Name (Set Name))
  -> String
renderNewImports options flags used =
    unlines
      [ case modImport of
          Unqualified { name } ->
            "import " <> shown name <> " (" <> showParents parents <> ")"
          UnqualifiedAs { name, as } ->
            "import " <> shown name <> " as "
            <> shown as <> " (" <> showParents parents <> ")"
          Qualified { name } ->
            "import qualified " <> shown name
            <> maybeShowList name parents
          QualifiedAs { name, as } ->
            "import qualified "
            <> shown name <> " as " <> shown as
            <> maybeShowList as parents
      | (modImport, parents) <- Map.toAscList used
      ]
  where
    maybeShowList :: ModuleName -> Map Name (Set Name) -> String
    maybeShowList modName parents =
      if options.excessive || modName `member` ambiguousNames
        then " (" <> showParents parents <> ")"
        else ""

    ambiguousNames :: Set ModuleName
    ambiguousNames =
      Map.keysSet
      . Map.filter (> 1)
      . Map.unionsWith (+)
      $ [ case modImport of
            Unqualified { name } -> Map.singleton name (1 :: Int)
            UnqualifiedAs {as} -> Map.singleton as 1
            Qualified { name } -> Map.singleton name 1
            QualifiedAs { as } -> Map.singleton as 1
        | (modImport, _) <- Map.toAscList used
        ]

    showParents :: Map Name (Set Name) -> String
    showParents parents =
      intercalate ", "
        [ shown parent <> showChildren children
        | (parent, children) <- Map.toList parents
        ]

    showChildren :: Set Name -> String
    showChildren children =
      if Set.null children then
        ""
      else
        "(" <> intercalate ", " (shown <$> Set.toAscList children) <> ")"

    shown :: Outputable o => o -> String
    shown = fixInlineName . showSDoc flags . ppr

    fixInlineName :: String -> String
    fixInlineName name =
      case headMay name of
        Nothing -> name
        Just c
          | Char.isAlphaNum c || c == '_' -> name
          | otherwise -> "(" <> name <> ")"


parseOptions :: [CommandLineOption] -> Options
parseOptions args =
  Options
    { excessive = "excessive" `elem` args
    }


