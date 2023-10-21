module Pages.Home_ exposing (page)

import Gen.Route as Route
import Html exposing (Attribute)
import Page exposing (Page)
import Request exposing (Request)
import Shared
import Slug
import View exposing (View)


newZoneRoute : Attribute msg
newZoneRoute =
    Slug.toHref (\s -> Route.Zone__Slug_ { slug = s }) Slug.new


page : Shared.Model -> Request -> Page
page shared req =
    Page.static
        { view = view }


view : View msg
view =
    { title = "What's growing on!?"
    , body =
        [ Html.h1 [] [ Html.text "What's growing on!?" ]
        , Html.p [] [ Html.text "A simple app to help you keep track of what's growing in your garden." ]
        , Html.a [ newZoneRoute ] [ Html.text "Add a zone" ]
        ]
    }
