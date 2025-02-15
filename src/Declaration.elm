module Declaration exposing (..)

import Elm.Processing
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Node as Node


getDeclarationByName : String -> RawFile -> Maybe Declaration.Declaration
getDeclarationByName name rawFile =
    Elm.Processing.process Elm.Processing.init rawFile
        |> .declarations
        |> List.map Node.value
        |> List.filter isAliasOrType
        |> findDeclarationByName name


isAliasOrType : Declaration.Declaration -> Bool
isAliasOrType declaration =
    case declaration of
        Declaration.AliasDeclaration decl ->
            True

        Declaration.CustomTypeDeclaration decl ->
            True

        _ ->
            False


findDeclarationByName :
    String
    -> List Declaration.Declaration
    -> Maybe Declaration.Declaration
findDeclarationByName name declarations =
    case declarations of
        (Declaration.AliasDeclaration decl) :: tl ->
            if Node.value decl.name == name then
                Just (Declaration.AliasDeclaration decl)

            else
                findDeclarationByName name tl

        (Declaration.CustomTypeDeclaration decl) :: tl ->
            if Node.value decl.name == name then
                Just (Declaration.CustomTypeDeclaration decl)

            else
                findDeclarationByName name tl

        _ :: tl ->
            findDeclarationByName name tl

        [] ->
            Nothing
