{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Data.Set (Set)
import GHC (DynFlags(dumpDir), ModSummary, ModuleName, Name, moduleName,
  moduleNameString)
import GHC.Plugins (GlobalRdrElt(GRE, gre_imp, gre_name, gre_par),
  HasDynFlags(getDynFlags), ImpDeclSpec(ImpDeclSpec, is_as, is_mod,
  is_qual), ImportSpec(is_decl), Outputable(ppr), Parent(NoParent,
  ParentIs), Plugin(pluginRecompile, typeCheckResultAction),
  PluginRecompile(NoForceRecompile), CommandLineOption, bestImport,
  defaultPlugin, liftIO, moduleEnvToList, occEnvElts, showSDoc)
import GHC.Tc.Utils.Monad (ImportAvails(imp_mods), TcGblEnv(tcg_imports,
  tcg_mod, tcg_used_gres), MonadIO, TcM)
import GHC.Types.Avail (greNamePrintableName)
import GHC.Unit.Module.Imported (ImportedBy(ImportedByUser),
  ImportedModsVal(imv_all_exports))
import qualified Data.Map as Map
import qualified Data.Set as Set


plugin :: Plugin
plugin = defaultPlugin
  { typeCheckResultAction = typeCheckResultActionImpl
  , pluginRecompile = \_ -> pure NoForceRecompile
  }


typeCheckResultActionImpl
  :: [CommandLineOption]
  -> ModSummary
  -> TcGblEnv
  -> TcM TcGblEnv
typeCheckResultActionImpl _ _ env = do
  used <- getUsedImports env
  flags <- getDynFlags
  void $ writeToDumpFile env flags used
  pure env


writeToDumpFile
  :: (MonadIO m)
  => TcGblEnv
  -> DynFlags
  -> Map ModuleImport (Map Name (Set Name))
  -> m (Maybe FilePath)
writeToDumpFile env flags used =
  {-
    If `-dumpdir` has been specified, then write the output into
    the dumpdir.  Mainly this  is because I can't figure out how to
    programmatically find the default dump dir.
  -}
  case dumpDir flags of
    Nothing -> pure Nothing
    Just dir ->
      liftIO $ do
        let 
          modName :: FilePath
          modName = moduleNameString . moduleName . tcg_mod $ env

          filename :: FilePath
          filename = dir <> "/" <> modName <> ".full-imports"
        writeFile filename (renderNewImports flags used)
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
        , GRE { gre_name = name } <- concat . occEnvElts . imv_all_exports $ imv
        ]

    used :: Map ModuleImport (Map Name (Set Name))
    used =
      Map.unionsWith
        (Map.unionWith Set.union)
        [ let
            imp :: ImportSpec
            imp = bestImport imps

            modName :: ModuleName
            modImport :: ModuleImport
            (modImport, modName) =
              let
                ImpDeclSpec { is_mod , is_as , is_qual } = is_decl imp
              in
                ( case (is_qual, is_as == is_mod) of
                    (True, True) -> Qualified is_mod
                    (True, False) -> QualifiedAs is_mod is_as
                    (False, _) -> Unqualified is_mod
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
  = Unqualified ModuleName
  | Qualified ModuleName
  | QualifiedAs ModuleName ModuleName
  deriving stock (Eq, Ord)

renderNewImports
  :: DynFlags
  -> Map ModuleImport (Map Name (Set Name))
  -> String
renderNewImports flags used =
    unlines
      [
        case modImport of
          Unqualified modName ->
            "import " <> shown modName <> " (" <> showParents parents <> ")"
          Qualified modName ->
            "import qualified " <> shown modName
          QualifiedAs modName asName ->
            "import qualified " <> shown modName <> " as " <> shown asName
      | (modImport, parents) <- Map.toAscList used
      ]
  where
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
    shown = showSDoc flags . ppr


