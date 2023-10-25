module Slug exposing (Slug, fromString, isNew, map, new, random, toHref)

import Gen.Route as Route exposing (Route(..))
import Html exposing (Attribute)
import Html.Attributes exposing (href)
import Random
import Random.Extra as Random


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


random : Random.Generator Slug
random =
    let
        adjectives =
            [ "Jolly"
            , "Frisky"
            , "Boisterous"
            , "Bumbling"
            , "Sassy"
            , "Clumsy"
            , "Zealous"
            , "Zany"
            , "Wobbly"
            , "Whimsical"
            , "Vivacious"
            , "Vexed"
            , "Jittery"
            , "Jaunty"
            , "Irate"
            , "Hapless"
            , "Giddy"
            , "Fluffy"
            , "Dizzy"
            , "Dapper"
            , "Curious"
            , "Crunchy"
            , "Cranky"
            , "Chirpy"
            , "Cheery"
            , "Chaotic"
            , "Cautious"
            , "Breezy"
            , "Brazen"
            , "Blissful"
            , "Bleary"
            , "Blazing"
            , "Bitter"
            , "Bewildered"
            , "Benevolent"
            , "Beaming"
            , "Batty"
            , "Bashful"
            , "Bald"
            , "Astonished"
            , "Arid"
            , "Animated"
            , "Angelic"
            , "Amused"
            , "Ambitious"
            , "Amazed"
            , "Alluring"
            , "Alarmed"
            , "Agile"
            , "Aghast"
            , "Affectionate"
            , "Adventurous"
            , "Adorable"
            , "Adamant"
            , "Acrobatic"
            , "Accurate"
            , "Absentminded"
            , "Abrasive"
            , "Glowing"
            , "Gleeful"
            , "Glamorous"
            , "Giant"
            , "Gentle"
            , "Genial"
            , "Generous"
            , "Funky"
            , "Funny"
            , "Fumbling"
            , "Frolicsome"
            , "Frigid"
            , "Friendly"
            , "Fresh"
            , "Frenzied"
            , "Free"
            , "Frantic"
            , "Fragile"
            , "Foamy"
            , "Flustered"
            , "Fluffy"
            , "Flowing"
            , "Floral"
            , "Floppy"
            , "Flimsy"
            , "Flickering"
            , "Flashy"
            , "Flamboyant"
            , "Fixed"
            , "Fierce"
            , "Fickle"
            , "Feisty"
            , "Fawning"
            , "Fat"
            , "Fascinated"
            ]

        animals =
            [ "Aardvarks"
            , "Badgers"
            , "Chameleons"
            , "Dingoes"
            , "Emus"
            , "Ferrets"
            , "Gobblers"
            , "Hamsters"
            , "Iguanas"
            , "Jackrabbits"
            , "Kakapos"
            , "Lemurs"
            , "Meerkats"
            , "Newts"
            , "Otters"
            , "Pufferfish"
            , "Quokkas"
            , "Raccoons"
            , "Sloths"
            , "Turtledoves"
            , "Unicorns"
            , "Vultures"
            , "Wallabies"
            , "X-ray Tetras"
            , "Yaks"
            , "Zebras"
            , "Antelopes"
            , "Bison"
            , "Coyotes"
            , "Dolphins"
            , "Echidnas"
            , "Flamingos"
            , "Giraffes"
            , "Hippopotamuses"
            , "Impalas"
            , "Jellyfish"
            , "Koalas"
            , "Llamas"
            , "Moose"
            , "Narwhals"
            , "Ostriches"
            , "Penguins"
            , "Quails"
            , "Rabbits"
            , "Stingrays"
            , "Tapirs"
            , "Uakaris"
            , "Vicunas"
            , "Wombats"
            , "Xenops"
            , "Yellowjackets"
            , "Zebrafish"
            , "Albatrosses"
            , "Budgerigars"
            , "Cats"
            , "Ducks"
            , "Elephants"
            , "Foxes"
            , "Geese"
            , "Hawks"
            , "Ibises"
            , "Jaguars"
            , "Kestrels"
            , "Lions"
            , "Mice"
            , "Nightingales"
            , "Owls"
            , "Peacocks"
            , "Quelea"
            , "Rats"
            , "Snakes"
            , "Tigers"
            , "Umbrellabirds"
            , "Vipers"
            , "Weasels"
            , "Xanclomys"
            , "Yabbies"
            , "Zorses"
            , "Apes"
            , "Beetles"
            , "Crabs"
            , "Doves"
            , "Frogs"
            , "Gazelles"
            , "Hummingbirds"
            , "Insects"
            , "Jays"
            , "Kangaroos"
            , "Lobsters"
            , "Monkeys"
            , "Numbats"
            , "Octopuses"
            , "Panthers"
            , "Quetzals"
            , "Reindeer"
            , "Scorpions"
            , "Toucans"
            , "Urchins"
            ]

        verbs =
            [ "jumping"
            , "running"
            , "dancing"
            , "singing"
            , "whispering"
            , "shouting"
            , "climbing"
            , "crawling"
            , "swimming"
            , "flying"
            , "giggling"
            , "sighing"
            , "painting"
            , "writing"
            , "typing"
            , "pulling"
            , "pushing"
            , "throwing"
            , "catching"
            , "baking"
            , "cooking"
            , "eating"
            , "drinking"
            , "staring"
            , "glimpsing"
            , "winking"
            , "frowning"
            , "smiling"
            , "laughing"
            , "crying"
            , "sketching"
            , "dreaming"
            , "listening"
            , "playing"
            , "rolling"
            , "sliding"
            , "tickling"
            , "hugging"
            , "kissing"
            , "kicking"
            , "punching"
            , "driving"
            , "walking"
            , "waving"
            , "reading"
            , "building"
            , "sneezing"
            , "sniffing"
            , "snoozing"
            , "working"
            , "talking"
            , "whistling"
            , "watching"
            , "wondering"
            , "scratching"
            , "clapping"
            , "prancing"
            , "jogging"
            , "twirling"
            , "racing"
            , "tapping"
            , "knitting"
            , "gardening"
            , "drawing"
            , "doodling"
            , "yawning"
            , "stretching"
            , "fishing"
            , "sailing"
            , "cycling"
            , "spinning"
            , "munching"
            , "thinking"
            , "exploring"
            , "wrestling"
            , "swinging"
            , "miming"
            , "juggling"
            , "humming"
            , "chewing"
            , "blinking"
            , "snuggling"
            , "dripping"
            , "sneaking"
            , "peeping"
            , "shopping"
            , "crafting"
            , "floating"
            , "bouncing"
            , "flipping"
            , "grinning"
            ]
    in
    Random.map4
        (\a ->
            Maybe.map3
                (\b c d ->
                    Slug (String.fromInt a ++ "-" ++ b ++ "-" ++ c ++ "-" ++ d |> String.toLower)
                )
        )
        (Random.int 100 999)
        (Random.sample adjectives)
        (Random.sample verbs)
        (Random.sample animals)
        |> Random.map (Maybe.withDefault (Slug ""))
