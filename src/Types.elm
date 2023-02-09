module Types exposing (FilterType(..), allFilterTypes, filterTypeRegistry, toIdentifier, toString)

import Dict exposing (Dict)


type FilterType
    = Head
    | UpperBody
    | LowerBody
    | Accessories


allFilterTypes : List FilterType
allFilterTypes =
    [ Head
    , UpperBody
    , LowerBody
    , Accessories
    ]


toString : FilterType -> String
toString ft =
    case ft of
        Head ->
            "Head and Neck"

        UpperBody ->
            "Upper body"

        LowerBody ->
            "Lower body"

        Accessories ->
            "Accessories"


{-| This has to produce unique strings!
-}
toIdentifier : FilterType -> String
toIdentifier ft =
    case ft of
        Head ->
            "FilterHead"

        UpperBody ->
            "FilterUpperbody"

        LowerBody ->
            "FilterLowerbody"

        Accessories ->
            "FilterAccessories"


filterTypeRegistry : Dict Int ( FilterType, String )
filterTypeRegistry =
    Dict.fromList
        [ ( 10217, ( Accessories, "Necklace" ) )
        , ( 10300, ( Accessories, "Flowers" ) )
        , ( 10266, ( Accessories, "Fan" ) )
        , ( 10293, ( Head, "Wig (Woman)" ) )
        , ( 10200, ( UpperBody, "Dress" ) )
        , ( 10202, ( Head, "Head gear" ) )
        , ( 10252, ( LowerBody, "Sitting figure" ) )
        , ( 10218, ( Head, "Earrings" ) )
        , ( 10239, ( UpperBody, "Armour" ) )
        , ( 10200, ( Accessories, "Gloves" ) )
        , ( 10201, ( UpperBody, "Ladies dress" ) )
        , ( 10207, ( Accessories, "Emblem" ) )
        , ( 10395, ( Accessories, "Pencil" ) )
        , ( 25309, ( Head, "Collar" ) )
        , ( 10205, ( Head, "Neck Gear" ) )
        , ( 10212, ( UpperBody, "Sleeves" ) )
        , ( 10215, ( UpperBody, "Tops" ) )
        , ( 10216, ( Head, "Bow" ) )
        , ( 10210, ( Head, "Wig (Man)" ) )
        , ( 10214, ( UpperBody, "Coat" ) )
        , ( 10206, ( UpperBody, "Full Body Suit" ) )
        , ( 10232, ( Head, "Pearl Necklace" ) )
        , ( 10219, ( Accessories, "Lace" ) )
        , ( 10222, ( Accessories, "Bracelet" ) )
        , ( 10223, ( Head, "Dormeuse" ) )
        , ( 10221, ( Accessories, "Brooch" ) )
        , ( 10373, ( UpperBody, "Underclothes" ) )
        ]
