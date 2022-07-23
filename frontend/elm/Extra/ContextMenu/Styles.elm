module Extra.ContextMenu.Styles exposing (Style, annotation, container, partition, pxFloat, row, shortcut, text)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes exposing (style)


type alias Style msg =
    List (Attribute msg)


borderColor2 : Element.Color
borderColor2 =
    rgb255 204 204 204


container :
    { containerColor : Element.Color
    , borderWidth : Int
    , containerPadding : Int
    , rounded : Bool
    , width : Int
    , left : Float
    , top : Float
    , fontSize : Int
    }
    -> Style msg
container { containerColor, borderWidth, containerPadding, rounded, width, left, top, fontSize } =
    [ Border.width borderWidth
    , paddingXY 0 containerPadding
    , if rounded then
        Border.rounded containerPadding

      else
        moveUp 0
    , Font.size fontSize
    , Background.color containerColor
    , Border.color borderColor2
    ]
        ++ ([ style "z-index" (String.fromFloat (2147483647 - 10))
            , style "width" (pxInt width)
            , style "position" "fixed"
            , style "top" (pxFloat top)
            , style "left" (pxFloat left)
            , Html.Attributes.class "contextMenu"
            ]
                |> List.map htmlAttribute
           )


row : Element.Color -> Element.Color -> Bool -> Bool -> Float -> Bool -> Bool -> Bool -> Style msg
row hoverColor disabledTextColor invertText usePointer lineHeight hovered disabled hasShortCut =
    [ height (px (round lineHeight))
    ]
        ++ (if disabled then
                [ Font.color disabledTextColor ]

            else if hovered && invertText then
                [ Font.color (rgb255 255 255 255) ]

            else
                []
           )
        ++ (if not disabled && usePointer then
                [ pointer ]

            else
                []
           )
        ++ [ if hovered then
                Background.color hoverColor

             else
                Background.color (rgba 0 0 0 0)
           , width fill
           ]


text : Int -> Style msg
text lineHeight =
    ([ style "line-height" (pxInt lineHeight)
     , style "text-overflow" "ellipsis"
     , style "overflow" "hidden"
     , style "white-space" "nowrap"
     ]
        |> List.map htmlAttribute
    )
        ++ [ width fill ]


annotation : Element.Color -> Float -> Float -> Bool -> Style msg
annotation color annotationHeight fontSize disabled =
    Font.color color
        :: ([ style "margin-top" "-2px"
            , style "line-height" (pxFloat annotationHeight)
            , style "font-size" (pxFloat fontSize)
            ]
                |> List.map htmlAttribute
           )


shortcut : Element.Color -> Float -> Bool -> Style msg
shortcut color lineHeight hovered =
    ([ style "line-height" (pxFloat lineHeight)
     ]
        |> List.map htmlAttribute
    )
        ++ (if hovered then
                []

            else
                [ Font.color color ]
           )


partition : Int -> Int -> Style msg
partition borderWidth margin =
    [ height (px borderWidth)
    , Background.color borderColor2
    , centerY
    , width fill
    ]



----


pxFloat : Float -> String
pxFloat n =
    String.fromFloat n ++ "px"


pxInt : Int -> String
pxInt n =
    String.fromInt n ++ "px"
