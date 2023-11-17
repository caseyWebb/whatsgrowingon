module Backend exposing (..)

import COSE.Algorithm
import Data exposing (..)
import Data.PasskeyAuthenticationOptions as PasskeyAuthenticationOptions
import Data.PasskeyAuthenticationResponse as PasskeyAuthenticationResponse
import Data.Users as Users exposing (Passkey)
import GenericDict as Dict
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import Passkey
import Shared
import Slug
import Types exposing (..)
import Ui.Color exposing (..)


type alias Model =
    BackendModel


type alias Msg =
    BackendMsg


app :
    { init : ( Model, Cmd Msg )
    , update : Msg -> Model -> ( Model, Cmd Msg )
    , updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd Msg )
    , subscriptions : Model -> Sub Msg
    }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


rpName : String
rpName =
    "whatsgrowingon"


rpID : String
rpID =
    "localhost"


init : ( Model, Cmd Msg )
init =
    let
        ( crops, varieties ) =
            [ ( "Onions", White, [ "Yellow Globe", "Red Baron", "Sweet Walla Walla", "Vidalia", "White Bermuda", "Cippolini", "Texas Super Sweet" ] )
            , ( "Broccoli", DarkGreen, [ "Calabrese", "Purple Sprouting", "Romanesco", "Belstar", "Green Goliath", "Di Cicco" ] )
            , ( "Brussels Sprouts", DarkGreen, [ "Long Island Improved", "Jade Cross", "Red Bull", "Churchill", "Diablo", "Nautic" ] )
            , ( "Cabbage", DarkGreen, [ "Golden Acre", "Red Drumhead", "Savoy", "January King", "Brunswick", "Charleston Wakefield" ] )
            , ( "Cauliflower", White, [ "Snowball", "Romanesco", "Cheddar", "Graffiti", "Veronica", "Aalsmeer" ] )
            , ( "Celery", Green, [ "Tall Utah", "Pascal", "Golden Self-Blanching", "Peppermint Stick", "Redventure", "Tango" ] )
            , ( "Collards", DarkGreen, [ "Georgia Southern", "Vates", "Morris Heading", "Champion", "Flash", "Tiger Hybrid" ] )
            , ( "Peppers", Red, [ "California Wonder", "Jalapeno", "Habanero", "Bell", "Poblano", "Serrano", "Cayenne", "Thai", "Ghost Pepper", "Carolina Reaper" ] )
            , ( "Eggplant", Purple, [ "Black Beauty", "Ichiban", "Fairy Tale", "Rosa Bianca", "Japanese Long", "Green Goddess" ] )
            , ( "Kale", DarkGreen, [ "Curly", "Lacinato", "Red Russian", "Premier", "Scarlet", "Tuscan" ] )
            , ( "Kohlrabi", Purple, [ "Purple Vienna", "White Vienna", "Gigante", "Kolibri", "Superschmelz", "Quickstar" ] )
            , ( "Peas", Green, [ "Sugar Snap", "Snow Pea", "Garden Pea", "Wando", "Lincoln", "Little Marvel", "Green Arrow" ] )
            , ( "Potatoes", White, [ "Russet", "Yukon Gold", "Red Pontiac", "Kennebec", "Adirondack Blue", "Fingerling", "Purple Majesty" ] )
            , ( "Swiss Chard", Green, [ "Bright Lights", "Fordhook Giant", "Ruby Red", "Rainbow", "Peppermint", "Lucullus" ] )
            , ( "Tomatoes", Red, [ "Roma", "Brandywine", "San Marzano", "Green Zebra", "Celebrity", "Better Boy", "Sun Gold" ] )
            , ( "Arugula", Green, [ "Rocket", "Dragon's Tongue", "Wild Arugula", "Astro", "Roquette", "Wasabi Arugula" ] )
            , ( "Beets", Red, [ "Detroit Dark Red", "Golden", "Chioggia", "Bull's Blood", "Early Wonder", "Sugar Beet" ] )
            , ( "Carrots", Orange, [ "Nantes", "Imperator", "Danvers", "Chantenay", "Purple Dragon", "Little Finger", "Atomic Red" ] )
            , ( "Horseradish", White, [ "Big Top", "Maliner Kren", "Bohemian", "Common", "Variegated", "Sass" ] )
            , ( "Lettuce", Green, [ "Romaine", "Butterhead", "Iceberg", "Oak Leaf", "Red Leaf", "Summer Crisp", "Lollo Rosso" ] )
            , ( "Mustard", Green, [ "Southern Giant Curled", "Tendergreen", "Red Giant", "Mizuna", "Osaka Purple", "Florida Broad Leaf" ] )
            , ( "Radishes", Red, [ "Cherry Belle", "French Breakfast", "Daikon", "Watermelon", "Black Spanish", "Easter Egg" ] )
            , ( "Spinach", DarkGreen, [ "Bloomsdale", "Giant Noble", "New Zealand", "Malabar", "Baby", "Savoy" ] )
            , ( "Turnips", Purple, [ "Purple Top White Globe", "Golden Globe", "Scarlet Ohno Revival", "Hakurei", "Gilfeather", "White Egg" ] )
            , ( "Bok Choy", Green, [ "White Stem", "Green Stem", "Purple Bok Choy", "Baby Bok Choy", "Shanghai", "Choy Sum" ] )
            , ( "Sweet Potatoes", Orange, [ "Beauregard", "Jewel", "Garnet", "Purple", "White", "Japanese", "Hannah" ] )
            , ( "Basil", Green, [ "Sweet Basil", "Genovese", "Thai Basil", "Lemon Basil", "Holy Basil", "Purple Basil" ] )
            , ( "Beans", Green, [ "Blue Lake", "Pinto", "Black Turtle", "Green Bean", "Wax Bean", "Lima Bean", "Scarlet Runner" ] )
            , ( "Cilantro", Green, [ "Santo", "Leisure", "Calypso", "Long Standing", "Marino", "Slow Bolt" ] )
            , ( "Corn", Yellow, [ "Sweet Corn", "Popcorn", "Flint Corn", "Dent Corn", "Indian Corn", "Silver Queen" ] )
            , ( "Cucumbers", DarkGreen, [ "Marketmore", "English", "Lemon", "Armenian", "Kirby", "Persian", "Japanese" ] )
            , ( "Dill", Green, [ "Bouquet", "Mammoth", "Fernleaf", "Dukat", "Long Island", "Vierling" ] )
            , ( "Fennel", Green, [ "Florence", "Bronze", "Sweet Fennel", "Perfection", "Zefa Fino", "Orion" ] )
            , ( "Melons", Pink, [ "Cantaloupe", "Honeydew", "Watermelon", "Galia", "Charentais", "Crenshaw", "Casaba" ] )
            , ( "Parsley", Green, [ "Curled", "Flat-leaf", "Hamburg", "Italian Giant", "Moss Curled", "Neapolitan" ] )
            , ( "Pumpkins", Orange, [ "Jack O'Lantern", "Sugar Pie", "Giant", "Cinderella", "Blue Hokkaido", "White Ghost" ] )
            , ( "Summer Squash", Yellow, [ "Zucchini", "Yellow Crookneck", "Pattypan", "Golden Zucchini", "Round Zucchini", "Tromboncino" ] )
            , ( "Cowpeas", Purple, [ "Black-eyed Pea", "Crowder Pea", "Cream Pea", "Purple Hull", "Lady Cream", "Zipper Cream" ] )
            , ( "Winter Squash", Orange, [ "Butternut", "Acorn", "Spaghetti", "Kabocha", "Delicata", "Hubbard" ] )
            , ( "Okra", DarkGreen, [ "Clemson Spineless", "Emerald", "Annie Oakley", "Burgundy", "Jambalaya", "Baby Bubba" ] )
            , ( "Garlic", White, [ "Elephant", "Music", "Russian Red", "Spanish Roja" ] )
            ]
                |> List.foldl
                    (\( cropName, color, varietyNames ) acc ->
                        let
                            varieties_ =
                                List.map
                                    (\name -> Slug.fromString name |> (\slug -> Variety slug name Nothing))
                                    varietyNames

                            crop =
                                Slug.fromString cropName
                                    |> (\slug ->
                                            Crop slug cropName (List.map .slug varieties_) color
                                       )
                        in
                        ( Dict.insert (Slug.map identity) crop.slug crop (Tuple.first acc)
                        , Dict.union (Tuple.second acc) (Dict.fromList (Slug.map identity) (List.map (\v -> ( v.slug, v )) varieties_))
                        )
                    )
                    ( Dict.empty, Dict.empty )
    in
    ( { zones = Dict.empty
      , crops = crops
      , varieties = varieties
      , users = Users.init
      , sessions = Dict.empty
      , passkeyChallenges = Dict.empty
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        andThen msg_ ( updatedModel, cmd ) =
            update msg_ updatedModel |> Tuple.mapSecond (List.singleton >> (::) cmd >> Cmd.batch)
    in
    case msg of
        ToFrontend clientId toFrontendMsg ->
            ( model
            , Lamdera.sendToFrontend clientId toFrontendMsg
            )

        GotPasskeyRegistrationOptions clientId (Ok opts) ->
            ( { model
                | passkeyChallenges =
                    Dict.insert identity
                        clientId
                        (Passkey.registrationsOptionsChallenge opts)
                        model.passkeyChallenges
              }
            , Lamdera.sendToFrontend clientId (SharedToFrontend (Shared.GotPasskeyRegistrationOptions opts))
            )

        GotPasskeyRegistrationOptions _ (Err _) ->
            ( model
              -- @TODO send error to frontend
            , Cmd.none
            )

        GotPasskeyRegistrationResult sessionId clientId username (Ok passkeyRegistration) ->
            let
                ( userId, updatedUsers ) =
                    Users.insert model.users
                        { username = username
                        , passkey = passkeyRegistration
                        }
            in
            ( { model
                | users = updatedUsers
              }
            , Cmd.none
            )
                |> andThen (Login sessionId clientId userId)

        GotPasskeyRegistrationResult _ _ _ (Err _) ->
            ( model
            , Cmd.none
              -- , Lamdera.sendToFrontend clientId (SharedToFrontend (Shared.GotWebAuthnRegistrationResult (Err err)))
            )

        GotPasskeyAuthenticationOptions clientId userId (Ok ( challenge, passkeyAuthenticationOptions )) ->
            ( { model | passkeyChallenges = Dict.insert identity clientId challenge model.passkeyChallenges }
            , Cmd.batch
                [ Lamdera.sendToFrontend clientId
                    (SharedToFrontend (Shared.GotPasskeyAuthenticationOptions (Ok passkeyAuthenticationOptions)))
                ]
            )

        GotPasskeyAuthenticationOptions clientId userId (Err err) ->
            ( model
            , Lamdera.sendToFrontend clientId (SharedToFrontend (Shared.GotPasskeyAuthenticationOptions (Err err)))
            )

        GotPasskeyAuthenticationResult sessionId clientId userId (Ok passkeyAuthentication) ->
            update (Login sessionId clientId userId) model

        GotPasskeyAuthenticationResult sessionId clientId userId (Err err) ->
            ( model
            , Cmd.none
              -- , Lamdera.sendToFrontend clientId (SharedToFrontend (Shared.GotPasskeyAuthenticationResult (Err err)))
            )

        Login sessionId clientId userId ->
            let
                user =
                    Users.get model.users userId

                updatedSessions =
                    Dict.insert identity sessionId { user = user } model.sessions
            in
            ( { model
                | sessions = updatedSessions
              }
            , Maybe.map (Shared.Login >> SharedToFrontend >> Lamdera.sendToFrontend clientId) user
                |> Maybe.withDefault Cmd.none
            )

        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd Msg )
updateFromFrontend sessionId clientId msg model =
    let
        respond msg_ =
            Lamdera.sendToFrontend clientId msg_

        currentUser =
            Dict.get identity sessionId model.sessions |> Maybe.andThen .user
    in
    case msg of
        SharedToBackend toBackendMsg ->
            case toBackendMsg of
                Shared.FetchData ->
                    ( model
                    , respond <|
                        SharedToFrontend
                            (Shared.GotData
                                { crops = model.crops
                                , varieties = model.varieties
                                , zones = model.zones
                                , currentUser = currentUser
                                }
                            )
                    )

                Shared.SaveZone zone ->
                    ( { model | zones = Dict.insert (Slug.map identity) zone.slug zone model.zones }
                    , Cmd.none
                    )

                Shared.DeleteZoneToBackend slug ->
                    ( { model | zones = Dict.remove (Slug.map identity) slug model.zones }
                    , Cmd.none
                    )

                Shared.FetchPasskeyRegistrationOptions username ->
                    ( model
                    , Passkey.generateRegistrationOptions
                        { rp =
                            { name = "whatsgrowingon"
                            , id = "localhost"
                            }
                        , user =
                            { name = username
                            , displayName = username
                            }
                        }
                        [ Passkey.requireUserVerification

                        -- , Passkey.allowedAlgorithms [ COSE.Algorithm.RS256 ]
                        ]
                        (GotPasskeyRegistrationOptions clientId)
                    )

                Shared.VerifyPasskeyRegistrationResponse response ->
                    case Dict.get identity clientId model.passkeyChallenges of
                        Nothing ->
                            ( model, Cmd.none )

                        Just challenge ->
                            Debug.todo ""

                Shared.FetchPasskeyAuthenticationOptions username ->
                    case Users.findByUsername model.users username of
                        Just user ->
                            ( model
                            , Http.get
                                { url =
                                    "http://localhost:8787/authenticate?"
                                        ++ "rpName="
                                        ++ rpName
                                        ++ "&rpID="
                                        ++ rpID
                                , expect =
                                    Http.expectString
                                        (Result.mapError
                                            (\httpError ->
                                                case httpError of
                                                    _ ->
                                                        "HTTP Error"
                                            )
                                            >> (\result ->
                                                    GotPasskeyAuthenticationOptions clientId
                                                        user.id
                                                        (Result.map2 Tuple.pair
                                                            (Result.andThen
                                                                (Decode.decodeString
                                                                    (Decode.field "challenge" Decode.string)
                                                                    >> Result.mapError Decode.errorToString
                                                                )
                                                                result
                                                            )
                                                            (Result.map PasskeyAuthenticationOptions.fromString result)
                                                        )
                                               )
                                        )
                                }
                            )

                        Nothing ->
                            ( model
                            , respond (SharedToFrontend (Shared.GotPasskeyAuthenticationOptions (Err "User not found")))
                            )

                Shared.VerifyPasskeyAuthenticationResponse response ->
                    case
                        Maybe.map2 Tuple.pair
                            (Debug.log "challenge" <| Dict.get identity clientId model.passkeyChallenges)
                            (Debug.log "user" <| Users.findByPasskeyId model.users (Debug.log "credentialId" <| PasskeyAuthenticationResponse.credentialId response))
                    of
                        Just ( challenge, user ) ->
                            ( model
                            , Http.post
                                { url =
                                    "http://localhost:8787/authenticate?"
                                        ++ "challenge="
                                        ++ challenge
                                        ++ "&rpID="
                                        ++ rpID
                                        ++ "&origin=http://localhost:8000"
                                , body =
                                    Http.jsonBody
                                        (Encode.object
                                            [ ( "response"
                                              , PasskeyAuthenticationResponse.encoder response
                                              )
                                            ]
                                        )
                                , expect =
                                    Http.expectJson (GotPasskeyAuthenticationResult sessionId clientId user.id)
                                        (Decode.succeed ())
                                }
                            )

                        Nothing ->
                            ( model
                            , Cmd.none
                            )

                Shared.Logout ->
                    ( { model | sessions = Dict.remove identity sessionId model.sessions }
                    , respond (SharedToFrontend Shared.LoggedOut)
                    )

        NoOpToBackend ->
            ( model, Cmd.none )
