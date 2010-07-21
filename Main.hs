module Main (main) where

import Control.Monad

import Language.Dot
import Language.Haskell.Exts

import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

import Data.Generics.Uniplate.Data


main :: IO ()
main = do
    files <- getArgs
    forM_ files $ \file -> do
        ParseOk r <- parseFile file
        let dot = uncurry renderGraph $ extractGraph r
        putStrLn dot
        (ec, out, err) <- readProcessWithExitCode "dot" ["-Tpng", "-o" ++ replaceExtension file ".png"] dot
        hPutStr stdout out
        hPutStr stderr err
        exitWith ec

extractGraph :: Module -> (Name, [(Name, [Name])])
extractGraph (Module _loc _name _prags _mb_warn _mb_exports _imports decls)
  = head [(head (calls root_e), extractGraphFromDecls bound_decls)
         | PatBind _loc (PVar (Ident "root")) _mb_type (UnGuardedRhs (Let (BDecls bound_decls) root_e)) _binds <- decls]

extractGraphFromDecls :: [Decl] -> [(Name, [Name])]
extractGraphFromDecls decls = all_names `zip` map (filter (`elem` all_names)) all_callss
  where
    (all_names, all_callss) = unzip [(name, calls e) | PatBind _loc (PVar name) _mb_type e _binds <- decls]

--calls :: Exp -> [Name]
calls e = [name | Var (UnQual name) <- universeBi e]

renderGraph :: Name -> [(Name, [Name])] -> String
renderGraph root_name edges = renderDot graph
  where
    n2id :: Name -> Id
    n2id (Ident x) = StringId x
    
    n2nid :: Name -> NodeId
    n2nid = flip NodeId Nothing . n2id
    
    graph = Graph UnstrictGraph DirectedGraph (Just (n2id root_name)) $
                [ stmt
                | (name, other_names) <- edges
                , stmt <- NodeStatement (n2nid name) [] :
                          [ EdgeStatement [ENodeId NoEdge       (n2nid name),
                                           ENodeId DirectedEdge (n2nid other_name)] []
                          | other_name <- other_names]]
