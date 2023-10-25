module Pages.Zone.Slug_ exposing (Model, Msg, page)

import Data exposing (Zone)
import Gen.Params.Zone.Slug_ exposing (Params)
import GenericDict as Dict
import Html.Styled as Html
import Page
import RemoteData exposing (RemoteData(..))
import Request
import Shared
import Slug exposing (Slug)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    let
        slug =
            Slug.fromString req.params.slug
    in
    Page.element
        { init = init slug shared
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { isNew : Bool
    , zone : RemoteData String Zone
    }


init : Slug -> Shared.Model -> ( Model, Cmd Msg )
init slug shared =
    if Slug.isNew slug then
        ( { isNew = True
          , zone =
                RemoteData.Success
                    { slug = slug
                    , name = ""
                    , plantings = []
                    }
          }
        , Cmd.none
        )

    else
        ( { isNew = False
          , zone =
                shared.zones
                    |> RemoteData.andThen
                        (Dict.get (Slug.map identity) slug
                            >> Maybe.map RemoteData.Success
                            >> Maybe.withDefault (RemoteData.Failure "Not Found")
                        )
          }
        , Cmd.none
        )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> View Msg
view model =
    case model.zone of
        RemoteData.NotAsked ->
            viewLoading

        RemoteData.Loading ->
            viewLoading

        RemoteData.Failure _ ->
            viewError

        RemoteData.Success zone ->
            if model.isNew then
                viewNewZone

            else
                viewZone zone


viewLoading : View Msg
viewLoading =
    { title = "Loading"
    , body =
        [ Html.h1 [] [ Html.text "Loading" ]
        ]
    }


viewNewZone : View Msg
viewNewZone =
    { title = "New Zone"
    , body =
        [ Html.h1 [] [ Html.text "Create a new planting zone" ]
        ]
    }


viewZone : Zone -> View Msg
viewZone zone =
    { title = "Zone"
    , body =
        [ Html.h1 [] [ Html.text "Zone" ]
        ]
    }


viewError : View Msg
viewError =
    { title = "Error"
    , body =
        [ Html.h1 [] [ Html.text "Error" ]
        , Html.p [] [ Html.text "Something went wrong" ]
        ]
    }
