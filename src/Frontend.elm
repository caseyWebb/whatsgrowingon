module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Navigation as Nav
import Effect
import Gen.Model
import Gen.Pages as Pages
import Gen.Route as Route
import Lamdera
import Request
import Shared
import Task
import Types exposing (..)
import Url
import View


type alias Model =
    FrontendModel


app :
    { init : Lamdera.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
    , view : Model -> Browser.Document FrontendMsg
    , update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
    , updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
    , subscriptions : Model -> Sub FrontendMsg
    , onUrlRequest : UrlRequest -> FrontendMsg
    , onUrlChange : Url.Url -> FrontendMsg
    }
app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        ( shared, sharedCmd ) =
            Shared.init
                { toBackend = Lamdera.sendToBackend << SharedToBackend
                }
                (Request.create () url key)

        ( page, effect ) =
            Pages.init (Route.fromUrl url) shared url key
    in
    ( FrontendModel url key shared page
    , Cmd.batch
        [ Cmd.map Shared sharedCmd
        , Effect.toCmd ( Shared, Page ) effect
        ]
    )


scrollPageToTop : Cmd FrontendMsg
scrollPageToTop =
    Task.perform (\_ -> NoOpFrontendMsg) (Browser.Dom.setViewport 0 0)


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            if url.path /= model.url.path then
                let
                    ( page, effect ) =
                        Pages.init (Route.fromUrl url) model.shared url model.key
                in
                ( { model | url = url, page = page }
                , Cmd.batch
                    [ Effect.toCmd ( Shared, Page ) effect
                    , scrollPageToTop
                    ]
                )

            else
                ( { model | url = url }, Cmd.none )

        Shared sharedMsg ->
            let
                ( shared, cmd ) =
                    Shared.update
                        { toBackend = Lamdera.sendToBackend << SharedToBackend
                        }
                        (Request.create () model.url model.key)
                        sharedMsg
                        model.shared

                ( page, effect ) =
                    Pages.init (Route.fromUrl model.url) shared model.url model.key
            in
            if page == Gen.Model.Redirecting_ then
                ( { model | shared = shared, page = page }
                , Cmd.batch
                    [ cmd |> Cmd.map Shared
                    , Effect.toCmd ( Shared, Page ) effect
                    ]
                )

            else
                ( { model | shared = shared }
                , cmd |> Cmd.map Shared
                )

        Page pageMsg ->
            let
                ( page, effect ) =
                    Pages.update pageMsg model.page model.shared model.url model.key
            in
            ( { model | page = page }
            , Effect.toCmd ( Shared, Page ) effect
            )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        SharedToFrontend msg_ ->
            let
                ( updatedSharedModel, cmd ) =
                    Shared.update
                        { toBackend = Lamdera.sendToBackend << SharedToBackend
                        }
                        (Request.create () model.url model.key)
                        (Shared.fromBackend msg_)
                        model.shared
            in
            ( { model | shared = updatedSharedModel }, cmd |> Cmd.map Shared )

        NoOpToFrontend ->
            ( model, Cmd.none )


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ Pages.subscriptions model.page model.shared model.url model.key |> Sub.map Page
        , Shared.subscriptions (Request.create () model.url model.key) model.shared |> Sub.map Shared
        ]


view : Model -> Browser.Document FrontendMsg
view model =
    View.toBrowserDocument
        (Shared.view
            (Request.create () model.url model.key)
            model.shared
            { page =
                Pages.view model.page model.shared model.url model.key
                    |> View.map Page
            , toMsg = Shared
            }
        )
