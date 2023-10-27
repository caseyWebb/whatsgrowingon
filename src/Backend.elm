module Backend exposing (..)

import Data exposing (..)
import GenericDict as Dict
import Lamdera exposing (ClientId, SessionId)
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
