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
        [ ( 10200, ( UpperBody, "Traditional Costume" ) )
        , ( 10201, ( UpperBody, "Ladies dress" ) )
        , ( 10202, ( Head, "Head Gear" ) )
        , ( 10203, ( Head, "Women Head Gear" ) )
        , ( 10205, ( Head, "Neck Gear" ) )
        , ( 10206, ( UpperBody, "Full Body Suit" ) )
        , ( 10207, ( Accessories, "Emblem" ) )
        , ( 10208, ( UpperBody, "Cape" ) )
        , ( 10210, ( Head, "Wig (Man)" ) )
        , ( 10212, ( UpperBody, "Sleeves" ) )
        , ( 10213, ( Accessories, "Button" ) )
        , ( 10214, ( UpperBody, "Coat" ) )
        , ( 10215, ( UpperBody, "Tops" ) )
        , ( 10216, ( Head, "Bow" ) )
        , ( 10217, ( Accessories, "Necklace" ) )
        , ( 10218, ( Head, "Earrings" ) )
        , ( 10219, ( Accessories, "Lace" ) )
        , ( 10221, ( Accessories, "Brooch" ) )
        , ( 10222, ( Accessories, "Bracelet" ) )
        , ( 10223, ( Head, "Dormeuse" ) )
        , ( 10224, ( Head, "Moustache" ) )
        , ( 10230, ( Accessories, "Watch" ) )
        , ( 10231, ( UpperBody, "Ornamental Clothing" ) )
        , ( 10232, ( Head, "Pearl Necklace" ) )
        , ( 10233, ( Accessories, "Ring" ) )
        , ( 10234, ( Head, "Beard" ) )
        , ( 10235, ( Accessories, "Knight Order" ) )
        , ( 10236, ( LowerBody, "Trousers" ) )
        , ( 10237, ( LowerBody, "Stockings, Socks" ) )
        , ( 10238, ( UpperBody, "Pectoral" ) )
        , ( 10239, ( UpperBody, "Armour" ) )
        , ( 10240, ( Accessories, "Baton" ) )
        , ( 10242, ( UpperBody, "Military Uniforms" ) )
        , ( 10243, ( Accessories, "Military Medals" ) )
        , ( 10245, ( Accessories, "Letter" ) )
        , ( 10246, ( Head, "Cheapeau Bras*" ) )
        , ( 10247, ( Accessories, "Books" ) )
        , ( 10250, ( Accessories, "Gloves & Mittens" ) )
        , ( 10252, ( LowerBody, "Sitting figure" ) )
        , ( 10257, ( LowerBody, "Boots" ) )
        , ( 10263, ( LowerBody, "Belt" ) )
        , ( 10266, ( Accessories, "Fan" ) )
        , ( 10267, ( UpperBody, "Special Purpose Clothes" ) )
        , ( 10269, ( UpperBody, "Chest" ) )
        , ( 10272, ( UpperBody, "Coat" ) )
        , ( 10274, ( Head, "Hairdress" ) )
        , ( 10283, ( Head, "Crown" ) )
        , ( 10287, ( Head, "Diadem" ) )
        , ( 10292, ( UpperBody, "Veil" ) )
        , ( 10293, ( Head, "Wig (Woman)" ) )
        , ( 10294, ( Accessories, "Jewels" ) )
        , ( 10295, ( Accessories, "Walking Stick" ) )
        , ( 10296, ( Accessories, "Hankerchief" ) )
        , ( 10297, ( Head, "Helmet" ) )
        , ( 10300, ( Accessories, "Flowers" ) )
        , ( 10301, ( Accessories, "Embroidery with Beads" ) )
        , ( 10306, ( UpperBody, "Embroidery" ) )
        , ( 10307, ( Accessories, "Flower Ornaments" ) )
        , ( 10312, ( Head, "Feather Head Gear" ) )
        , ( 10319, ( Accessories, "Paper" ) )
        , ( 10321, ( Accessories, "Ink Well" ) )
        , ( 10323, ( LowerBody, "Apron" ) )
        , ( 10331, ( Head, "Pendant" ) )
        , ( 10332, ( LowerBody, "Hip" ) )
        , ( 10351, ( Accessories, "Medallion" ) )
        , ( 10356, ( Head, "Man Head Gear" ) )
        , ( 10357, ( Head, "Colours & Pigments" ) )
        , ( 10373, ( UpperBody, "Underclothes" ) )
        , ( 10384, ( Head, "Nimbus, Halo" ) )
        , ( 10387, ( Head, "Insigne of Bishop" ) )
        , ( 10393, ( Accessories, "Cross Necklace" ) )
        , ( 10394, ( UpperBody, "Fur Coat*" ) )
        , ( 10395, ( Accessories, "Pencil" ) )
        , ( 10421, ( Head, "Bonnet (Hat)" ) )
        , ( 10423, ( UpperBody, "Historical Fashion" ) )
        , ( 10426, ( Head, "Hacking Weapon*" ) )
        , ( 10428, ( LowerBody, "Belt*" ) )
        , ( 10429, ( LowerBody, "Fur Coat" ) )
        , ( 10499, ( UpperBody, "Mantle" ) )
        , ( 10500, ( Head, "Insignia!Nur ein Ergebnis!" ) )
        , ( 10519, ( Head, "Head Gear*" ) )
        , ( 10515, ( Accessories, "Sceptre" ) )
        , ( 10519, ( UpperBody, "Vest*" ) )
        , ( 10525, ( Accessories, "Devil" ) )
        , ( 10548, ( UpperBody, "Formal Clothes !Nur ein Ergebnis!" ) )
        , ( 10549, ( UpperBody, "Coat (Men) !Nur ein Ergebnis!" ) )
        , ( 10581, ( UpperBody, "Fur Coat" ) )
        , ( 10596, ( UpperBody, "Fur Coat*" ) )
        , ( 10616, ( Accessories, "Archer's Weapons" ) )
        , ( 10617, ( Accessories, "Dragon" ) )
        , ( 10915, ( Accessories, "Weapons*" ) )
        , ( 10926, ( Accessories, "Gems" ) )
        , ( 25309, ( Head, "Collar" ) )
        ]
