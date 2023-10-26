module Backend exposing (..)

import Data exposing (..)
import GenericDict as Dict
import Lamdera exposing (ClientId, SessionId)
import Shared
import Slug
import Types exposing (..)


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


init : ( Model, Cmd Msg )
init =
    let
        ( crops, varieties ) =
            [ ( "Onions", [ "Yellow Globe", "Red Baron", "Sweet Walla Walla", "Vidalia", "White Bermuda", "Cippolini", "Texas Super Sweet" ] )
            , ( "Broccoli", [ "Calabrese", "Purple Sprouting", "Romanesco", "Belstar", "Green Goliath", "Di Cicco" ] )
            , ( "Brussels Sprouts", [ "Long Island Improved", "Jade Cross", "Red Bull", "Churchill", "Diablo", "Nautic" ] )
            , ( "Cabbage", [ "Golden Acre", "Red Drumhead", "Savoy", "January King", "Brunswick", "Charleston Wakefield" ] )
            , ( "Cauliflower", [ "Snowball", "Romanesco", "Cheddar", "Graffiti", "Veronica", "Aalsmeer" ] )
            , ( "Celery", [ "Tall Utah", "Pascal", "Golden Self-Blanching", "Peppermint Stick", "Redventure", "Tango" ] )
            , ( "Collards", [ "Georgia Southern", "Vates", "Morris Heading", "Champion", "Flash", "Tiger Hybrid" ] )
            , ( "Peppers", [ "California Wonder", "Jalapeno", "Habanero", "Bell", "Poblano", "Serrano", "Cayenne", "Thai", "Ghost Pepper", "Carolina Reaper" ] )
            , ( "Eggplant", [ "Black Beauty", "Ichiban", "Fairy Tale", "Rosa Bianca", "Japanese Long", "Green Goddess" ] )
            , ( "Kale", [ "Curly", "Lacinato", "Red Russian", "Premier", "Scarlet", "Tuscan" ] )
            , ( "Kohlrabi", [ "Purple Vienna", "White Vienna", "Gigante", "Kolibri", "Superschmelz", "Quickstar" ] )
            , ( "Peas", [ "Sugar Snap", "Snow Pea", "Garden Pea", "Wando", "Lincoln", "Little Marvel", "Green Arrow" ] )
            , ( "Potatoes", [ "Russet", "Yukon Gold", "Red Pontiac", "Kennebec", "Adirondack Blue", "Fingerling", "Purple Majesty" ] )
            , ( "Swiss Chard", [ "Bright Lights", "Fordhook Giant", "Ruby Red", "Rainbow", "Peppermint", "Lucullus" ] )
            , ( "Tomatoes", [ "Roma", "Brandywine", "San Marzano", "Green Zebra", "Celebrity", "Better Boy", "Sun Gold" ] )
            , ( "Arugula", [ "Rocket", "Dragon's Tongue", "Wild Arugula", "Astro", "Roquette", "Wasabi Arugula" ] )
            , ( "Beets", [ "Detroit Dark Red", "Golden", "Chioggia", "Bull's Blood", "Early Wonder", "Sugar Beet" ] )
            , ( "Carrots", [ "Nantes", "Imperator", "Danvers", "Chantenay", "Purple Dragon", "Little Finger", "Atomic Red" ] )
            , ( "Horseradish", [ "Big Top", "Maliner Kren", "Bohemian", "Common", "Variegated", "Sass" ] )
            , ( "Lettuce", [ "Romaine", "Butterhead", "Iceberg", "Oak Leaf", "Red Leaf", "Summer Crisp", "Lollo Rosso" ] )
            , ( "Mustard", [ "Southern Giant Curled", "Tendergreen", "Red Giant", "Mizuna", "Osaka Purple", "Florida Broad Leaf" ] )
            , ( "Radishes", [ "Cherry Belle", "French Breakfast", "Daikon", "Watermelon", "Black Spanish", "Easter Egg" ] )
            , ( "Spinach", [ "Bloomsdale", "Giant Noble", "New Zealand", "Malabar", "Baby", "Savoy" ] )
            , ( "Turnips", [ "Purple Top White Globe", "Golden Globe", "Scarlet Ohno Revival", "Hakurei", "Gilfeather", "White Egg" ] )
            , ( "Bok Choy", [ "White Stem", "Green Stem", "Purple Bok Choy", "Baby Bok Choy", "Shanghai", "Choy Sum" ] )
            , ( "Sweet Potatoes", [ "Beauregard", "Jewel", "Garnet", "Purple", "White", "Japanese", "Hannah" ] )
            , ( "Basil", [ "Sweet Basil", "Genovese", "Thai Basil", "Lemon Basil", "Holy Basil", "Purple Basil" ] )
            , ( "Beans", [ "Blue Lake", "Pinto", "Black Turtle", "Green Bean", "Wax Bean", "Lima Bean", "Scarlet Runner" ] )
            , ( "Cilantro", [ "Santo", "Leisure", "Calypso", "Long Standing", "Marino", "Slow Bolt" ] )
            , ( "Corn", [ "Sweet Corn", "Popcorn", "Flint Corn", "Dent Corn", "Indian Corn", "Silver Queen" ] )
            , ( "Cucumbers", [ "Marketmore", "English", "Lemon", "Armenian", "Kirby", "Persian", "Japanese" ] )
            , ( "Dill", [ "Bouquet", "Mammoth", "Fernleaf", "Dukat", "Long Island", "Vierling" ] )
            , ( "Fennel", [ "Florence", "Bronze", "Sweet Fennel", "Perfection", "Zefa Fino", "Orion" ] )
            , ( "Melons", [ "Cantaloupe", "Honeydew", "Watermelon", "Galia", "Charentais", "Crenshaw", "Casaba" ] )
            , ( "Parsley", [ "Curled", "Flat-leaf", "Hamburg", "Italian Giant", "Moss Curled", "Neapolitan" ] )
            , ( "Pumpkins", [ "Jack O'Lantern", "Sugar Pie", "Giant", "Cinderella", "Blue Hokkaido", "White Ghost" ] )
            , ( "Summer Squash", [ "Zucchini", "Yellow Crookneck", "Pattypan", "Golden Zucchini", "Round Zucchini", "Tromboncino" ] )
            , ( "Cowpeas", [ "Black-eyed Pea", "Crowder Pea", "Cream Pea", "Purple Hull", "Lady Cream", "Zipper Cream" ] )
            , ( "Winter Squash", [ "Butternut", "Acorn", "Spaghetti", "Kabocha", "Delicata", "Hubbard" ] )
            , ( "Okra", [ "Clemson Spineless", "Emerald", "Annie Oakley", "Burgundy", "Jambalaya", "Baby Bubba" ] )
            , ( "Garlic", [ "Elephant", "Music", "Russian Red", "Spanish Roja" ] )
            ]
                |> List.foldl
                    (\( cropName, varietyNames ) acc ->
                        let
                            varieties_ =
                                List.map
                                    (\name -> Slug.fromString name |> (\slug -> Variety slug name))
                                    varietyNames

                            crop =
                                Slug.fromString cropName
                                    |> (\slug ->
                                            Crop slug cropName (List.map .slug varieties_)
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
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd Msg )
updateFromFrontend _ clientId msg model =
    let
        respond msg_ =
            ( model, Lamdera.sendToFrontend clientId msg_ )
    in
    case msg of
        SharedToBackend toBackendMsg ->
            case toBackendMsg of
                Shared.FetchData ->
                    respond <| SharedToFrontend (Shared.GotData model)

                Shared.SaveZone zone ->
                    ( { model | zones = Dict.insert (Slug.map identity) zone.slug zone model.zones }
                    , Cmd.none
                    )

        NoOpToBackend ->
            ( model, Cmd.none )
