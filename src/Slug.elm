module Slug exposing (Slug, fromString, isNew, map, new, toHref)

import Gen.Route as Route exposing (Route(..))
import Html exposing (Attribute)
import Html.Attributes exposing (href)


type Slug
    = Slug String
    | New


new : Slug
new =
    New


map : (String -> a) -> Slug -> a
map f slug =
    case slug of
        New ->
            f "new"

        Slug str ->
            f str


fromString : String -> Slug
fromString slug =
    if slug == "new" then
        New

    else
        Slug slug


isNew : Slug -> Bool
isNew slug =
    case slug of
        New ->
            True

        Slug _ ->
            False


toHref : (String -> Route) -> Slug -> Attribute msg
toHref toRoute slug =
    (Route.toHref >> href) <|
        case slug of
            New ->
                toRoute "new"

            Slug str ->
                toRoute str
