module Font exposing (..)

import Element exposing (..)
import Element.Font as Font



-- size : Int -> Float -> Element.Attribute msg
-- size px scale =
--     Font.size (round (toFloat px * scale))
-- medium : Element.Attribute msg
-- medium =
--     Font.medium
-- color : Color -> Element.Attribute msg
-- color =
--     Font.color
-- letterSpacing : Float -> Attribute msg
-- letterSpacing =
--     Font.letterSpacing
-- family : List Font.Font -> Attribute msg
-- family =
--     Font.family
-- typeface : String -> Font.Font
-- typeface =
--     Font.typeface
-- sansSerif : Font.Font
-- sansSerif =
--     Font.sansSerif


sansSerifType : Attribute msg
sansSerifType =
    Font.family
        [ --
          Font.typeface "Barlow Semi Condensed"

        --   Font.typeface "Barlow"
        , Font.typeface "-apple-system"
        , Font.typeface "BlinkMacSystemFont"
        , Font.typeface "Segoe UI"
        , Font.typeface "Roboto"
        , Font.typeface "Oxygen"
        , Font.typeface "Ubuntu"
        , Font.typeface "Cantarell"
        , Font.typeface "Open Sans"
        , Font.typeface "Helvetica Neue"
        , Font.typeface "sans-serif"
        , Font.sansSerif
        ]


serifType : Attribute msg
serifType =
    Font.family
        [ Font.typeface "Fraunces"
        , Font.serif
        ]
