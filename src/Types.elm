module Types exposing
    ( FilterType(..)
    , allFilterTypes
    , filterTypeRegistry
    , toColor
    , toIcon
    , toIdentifier
    , toString
    )

import Dict exposing (Dict)
import Html exposing (Html)
import Svg exposing (path, svg)
import Svg.Attributes as SA


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


toColor : FilterType -> String
toColor ft =
    case ft of
        Head ->
            "#A6BED9"

        UpperBody ->
            "#BBE2BC"

        LowerBody ->
            "#F3CA8E"

        Accessories ->
            "#E2BBDB"


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


toIcon : FilterType -> Html msg
toIcon ft =
    svg
        [ SA.viewBox "0 0 700 700"
        , SA.height "100%"
        ]
        (case ft of
            Head ->
                [ path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m350,513.33c-109.85,0-189.58-31.59-189.58-75.11,0-6.44,5.23-11.67,11.67-11.67s11.67,5.23,11.67,11.67c0,21.16,59.19,51.78,166.25,51.78s166.25-30.64,166.25-51.78c0-6.44,5.25-11.67,11.67-11.67s11.67,5.23,11.67,11.67c0,43.52-79.73,75.11-189.58,75.11h-.01Z"
                    ]
                    []
                , path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m350,256.67c-113.12,0-233.33-32.71-233.33-93.33s120.21-93.33,233.33-93.33,233.33,32.72,233.33,93.33-120.21,93.33-233.33,93.33Zm0-163.34c-123.76,0-210,36.89-210,70s86.24,70,210,70,210-36.89,210-70-86.24-70-210-70Z"
                    ]
                    []
                , path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m350,560c-113.4,0-186.67-27.49-186.67-70,0-57.05-11.71-109.81-23.06-160.81-11.6-52.22-23.61-106.24-23.61-165.85,0-6.44,5.23-11.67,11.67-11.67s11.67,5.23,11.67,11.67c0,57.05,11.71,109.81,23.06,160.81,11.6,52.24,23.61,106.26,23.61,165.85,0,19.39,50.68,46.67,163.33,46.67s163.33-27.28,163.33-46.67c0-59.62,12.04-113.63,23.61-165.85,11.34-51.01,23.05-103.74,23.05-160.81,0-6.44,5.25-11.67,11.67-11.67s11.67,5.23,11.67,11.67c0,59.62-12.04,113.63-23.61,165.85-11.34,51.01-23.05,103.74-23.05,160.81,0,42.51-73.29,70-186.67,70h0Z"
                    ]
                    []
                , path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m350,630c-157.01,0-280-61.51-280-140,0-38.96,48.86-79.54,90.7-102.46,5.67-3.1,12.74-1,15.84,4.64,3.1,5.65,1.03,12.71-4.62,15.84-46.27,25.32-78.59,59.06-78.59,81.97,0,63.23,117.55,116.67,256.67,116.67s256.67-53.43,256.67-116.67c0-22.91-32.32-56.63-78.59-81.97-5.62-3.12-7.72-10.22-4.62-15.84,3.1-5.65,10.2-7.75,15.87-4.64,41.81,22.94,90.67,63.54,90.67,102.46,0,78.49-122.99,140-280,140h0Z"
                    ]
                    []
                ]

            UpperBody ->
                [ path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m347.04,621.25c-.84,0-1.7-.09-2.54-.28-70.02-15.63-156.64-54.65-189.14-85.26-2.45-2.33-3.78-5.58-3.66-8.94,1.77-46.99,18.43-146.42,69.4-225.91-26.2-73.96-5.41-149.19,16.29-214.13,1.59-4.76,6.04-7.96,11.06-7.96h52.03c3.57,0,6.95,1.66,9.17,4.46,2.21,2.82,3.01,6.49,2.17,9.96-.21.91-20.91,90.18,43.77,159.9,1.98,2.15,3.1,4.99,3.1,7.93v348.58c0,3.55-1.61,6.88-4.39,9.1-2.05,1.66-4.62,2.54-7.26,2.54h0Zm-171.75-99.08c27.51,22.98,95.08,55.74,160.09,72.59v-329.26c-54.53-61.46-52.62-133.21-48.74-163.43h-29.75c-24.2,74.29-35,136.45-11.92,196,1.38,3.55.93,7.54-1.17,10.69-48.67,73.04-65.78,168.33-68.51,213.41h0Z"
                    ]
                    []
                , path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m347.04,621.25c-2.64,0-5.2-.89-7.3-2.54-2.78-2.24-4.39-5.55-4.39-9.12v-348.58c0-2.94,1.12-5.79,3.1-7.93,65.57-70.68,55.62-160.37,55.49-161.26-.4-3.31.63-6.65,2.87-9.15,2.24-2.5,5.39-3.92,8.73-3.92h52.01c5.02,0,9.47,3.22,11.06,7.96,21.54,64.38,39.76,139.91,13.28,214.15,58.52,91.44,65.61,203.96,66.43,225.96.12,3.38-1.21,6.62-3.66,8.94-30.94,29.14-122.12,69-195.14,85.24-.77.16-1.63.26-2.47.26h0Zm11.67-355.73v329.26c62.84-16.17,136.8-48.39,166.11-72.64-3.15-58.87-21.91-147.91-65.54-213.38-2.1-3.15-2.54-7.14-1.19-10.69,25.67-66.19,10.62-135.73-8.94-196h-31.2c.61,29.22-4.11,101.62-59.24,163.45h0Z"
                    ]
                    []
                , path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m358.7,312.08h-23.33c-6.44,0-11.67-5.23-11.67-11.67s5.23-11.67,11.67-11.67h23.33c6.42,0,11.67,5.23,11.67,11.67s-5.25,11.67-11.67,11.67Z"
                    ]
                    []
                , path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m358.7,358.75h-23.33c-6.44,0-11.67-5.23-11.67-11.67s5.23-11.67,11.67-11.67h23.33c6.42,0,11.67,5.23,11.67,11.67s-5.25,11.67-11.67,11.67Z"
                    ]
                    []
                , path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m358.7,405.42h-23.33c-6.44,0-11.67-5.25-11.67-11.67s5.23-11.67,11.67-11.67h23.33c6.42,0,11.67,5.25,11.67,11.67s-5.25,11.67-11.67,11.67Z"
                    ]
                    []
                , path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m358.7,452.08h-23.33c-6.44,0-11.67-5.25-11.67-11.67s5.23-11.67,11.67-11.67h23.33c6.42,0,11.67,5.25,11.67,11.67s-5.25,11.67-11.67,11.67Z"
                    ]
                    []
                , path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m288.7,475.42h-70c-6.44,0-11.67-5.25-11.67-11.67s5.23-11.67,11.67-11.67h70c6.44,0,11.67,5.25,11.67,11.67s-5.23,11.67-11.67,11.67Z"
                    ]
                    []
                , path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m475.37,475.42h-70c-6.42,0-11.67-5.25-11.67-11.67s5.25-11.67,11.67-11.67h70c6.42,0,11.67,5.25,11.67,11.67s-5.25,11.67-11.67,11.67Z"
                    ]
                    []
                ]

            LowerBody ->
                [ path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m618.33,501.69h-244.28c-21.89,0-50.82-13.11-78.8-25.76-22.73-10.27-46.22-20.91-59.01-20.91h-2.92v35c0,6.42-5.23,11.67-11.67,11.67H81.66c-6.44,0-11.67-5.25-11.67-11.67v-46.67c0-6.44,5.23-11.67,11.67-11.67h536.67c6.44,0,11.67,5.23,11.67,11.67v46.67c0,6.42-5.23,11.67-11.67,11.67h0Zm-312.66-46.67c24.34,11.01,51.73,23.33,68.46,23.33h232.54v-23.33h-301Zm-212.34,23.33h116.67v-23.33h-116.67v23.33Z"
                    ]
                    []
                , path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m166.65,455.02c-6.44,0-11.67-5.25-11.67-11.67,0-40.41-32.88-73.31-73.29-73.31-6.44,0-11.67-5.23-11.67-11.67s5.23-11.67,11.67-11.67c53.27,0,96.62,43.33,96.62,96.65,0,6.42-5.25,11.67-11.67,11.67Z"
                    ]
                    []
                , path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m618.33,466.69c-6.42,0-11.67-5.25-11.67-11.67,0-57.56-26.78-93.33-70-93.33h-17.5c-51.54,0-99.98-20-136.55-56.35l-82.41-81.99c-69.98,22.61-127.19,25.81-184.66,10.48-5.27-1.38-11.01-.23-15.33,3.1-4.34,3.31-6.88,8.54-6.88,14v204.1c0,6.42-5.23,11.67-11.67,11.67s-11.67-5.25-11.67-11.67v-204.12c0-12.65,6-24.8,16.03-32.53,10.06-7.7,23.36-10.34,35.53-7.12,54.62,14.6,109.53,10.81,178.04-12.34,4.18-1.4,8.8-.35,11.95,2.78l87.5,87.06c32.13,31.94,74.76,49.56,120.07,49.56h17.55c43.07,0,93.33,30.57,93.33,116.67,0,6.46-5.23,11.71-11.67,11.71h0Z"
                    ]
                    []
                , path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m327.65,446.27c-6.44,0-11.67-5.25-11.67-11.67,0-86.29,82.86-109.08,126.68-109.08,6.44,0,11.67,5.23,11.67,11.67s-5.23,11.67-11.67,11.67c-4.22,0-103.34.98-103.34,85.75,0,6.42-5.23,11.67-11.67,11.67h0Z"
                    ]
                    []
                ]

            Accessories ->
                [ path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m408.33,606.65c-122.22,0-221.67-99.45-221.67-221.67s99.44-221.67,221.67-221.67,221.67,99.44,221.67,221.67-99.45,221.67-221.67,221.67h0Zm0-420c-109.36,0-198.33,88.97-198.33,198.33s88.97,198.33,198.33,198.33,198.33-88.97,198.33-198.33-88.95-198.33-198.33-198.33h0Z"
                    ]
                    []
                , path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m140,606.65c-38.59,0-70-31.41-70-70s31.41-70,70-70,70,31.41,70,70-31.41,70-70,70Zm0-116.67c-25.74,0-46.67,20.91-46.67,46.67s20.93,46.67,46.67,46.67,46.67-20.93,46.67-46.67-20.93-46.67-46.67-46.67Z"
                    ]
                    []
                , path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m408.33,384.98c-6.42,0-11.67-5.25-11.67-11.67v-233.33c0-6.44,5.25-11.67,11.67-11.67s11.67,5.23,11.67,11.67v233.33c0,6.42-5.25,11.67-11.67,11.67Z"
                    ]
                    []
                , path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m140,548.31c-2.78,0-5.55-.98-7.77-2.96-4.81-4.32-5.2-11.67-.91-16.47,24.57-27.46,18.57-71.77,13.28-110.88-2.17-16.01-4.22-31.12-4.22-44.68,0-123.11,72.31-254.59,274.77-256.64l19.93-19.93c4.53-4.53,11.95-4.53,16.5,0,4.53,4.55,4.53,11.97,0,16.5l-23.33,23.33c-2.17,2.17-5.13,3.41-8.24,3.41-160.49,0-256.29,87.2-256.29,233.33,0,11.97,1.94,26.32,3.99,41.54,5.69,41.91,12.74,94.06-18.99,129.57-2.31,2.57-5.51,3.9-8.7,3.9h0Z"
                    ]
                    []
                , path
                    [ SA.stroke "none"
                    , SA.fill "currentColor"
                    , SA.d "m210,209.98h-46.67c-6.44,0-11.67-5.25-11.67-11.67s5.23-11.67,11.67-11.67h46.67c6.44,0,11.67,5.23,11.67,11.67s-5.23,11.67-11.67,11.67Z"
                    ]
                    []
                ]
        )


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
        , ( 10227, ( UpperBody, "Table-Clothes" ) )
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
        , ( 10249, ( Accessories, "Linen Material*" ) )
        , ( 10250, ( Accessories, "Gloves & Mittens" ) )
        , ( 10252, ( LowerBody, "Sitting figure" ) )
        , ( 10257, ( LowerBody, "Boots" ) )
        , ( 10260, ( UpperBody, "Folk Costume" ) )
        , ( 10263, ( LowerBody, "Belt" ) )
        , ( 10264, ( Accessories, "Flower Ornaments" ) )
        , ( 10266, ( Accessories, "Fan" ) )
        , ( 10267, ( UpperBody, "Special Purpose Clothes" ) )
        , ( 10269, ( UpperBody, "Chest" ) )
        , ( 10270, ( UpperBody, "Jewels Ornaments" ) )
        , ( 10272, ( UpperBody, "Coat" ) )
        , ( 10274, ( Head, "Hairdress" ) )
        , ( 10283, ( Head, "Crown" ) )
        , ( 10287, ( Head, "Diadem" ) )
        , ( 10291, ( LowerBody, "Shoes" ) )
        , ( 10292, ( UpperBody, "Veil" ) )
        , ( 10293, ( Head, "Wig (Woman)" ) )
        , ( 10294, ( Accessories, "Jewels" ) )
        , ( 10295, ( Accessories, "Walking Stick" ) )
        , ( 10296, ( Accessories, "Hankerchief" ) )
        , ( 10297, ( Head, "Helmet" ) )
        , ( 10298, ( LowerBody, "Skirt" ) )
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
        , ( 10363, ( LowerBody, "Leg & Feet Gear" ) )
        , ( 10367, ( Head, "Beret" ) )
        , ( 10372, ( Accessories, "Crucifix" ) )
        , ( 10373, ( UpperBody, "Underclothes" ) )
        , ( 10375, ( Accessories, "Flowers*" ) )
        , ( 10384, ( Head, "Nimbus, Halo" ) )
        , ( 10387, ( Head, "Insigne of Bishop" ) )
        , ( 10391, ( UpperBody, "Morning Dress" ) )
        , ( 10393, ( Accessories, "Cross Necklace" ) )
        , ( 10394, ( UpperBody, "Fur Coat*" ) )
        , ( 10395, ( Accessories, "Pencil" ) )
        , ( 10400, ( Accessories, "Glas of Wine" ) )
        , ( 10419, ( Accessories, "Tambourine" ) )
        , ( 10421, ( Head, "Bonnet (Hat)" ) )
        , ( 10423, ( UpperBody, "Historical Fashion" ) )
        , ( 10426, ( Head, "Hacking Weapon*" ) )
        , ( 10428, ( LowerBody, "Belt*" ) )
        , ( 10429, ( UpperBody, "Fur Coat" ) )
        , ( 10499, ( UpperBody, "Mantle" ) )
        , ( 10500, ( Head, "Insignia!Nur ein Ergebnis!" ) )
        , ( 10515, ( Accessories, "Sceptre" ) )
        , ( 10519, ( Head, "Head Gear*" ) )
        , ( 10519, ( UpperBody, "Vest*" ) )
        , ( 10525, ( Accessories, "Devil" ) )
        , ( 10532, ( Accessories, "Talisman" ) )
        , ( 10548, ( UpperBody, "Formal Clothes !Nur ein Ergebnis!" ) )

        -- Nur ein Ergebnis
        , ( 10549, ( UpperBody, "Coat (Men) !Nur ein Ergebnis!" ) )
        , ( 10581, ( UpperBody, "Fur Coat" ) )
        , ( 10596, ( UpperBody, "Fur Coat*" ) )
        , ( 10616, ( Accessories, "Archer's Weapons" ) )
        , ( 10617, ( Accessories, "Dragon" ) )
        , ( 10915, ( Accessories, "Weapons*" ) )
        , ( 10926, ( Accessories, "Gems" ) )
        , ( 25309, ( Head, "Collar" ) )
        ]
