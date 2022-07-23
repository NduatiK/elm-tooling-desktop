module View exposing
    ( DeviceSize(..)
    , DeviceWidth(..)
    , ResponsiveDevice
    , View
    , classifyDevice
    , content
    , contentWidth
    , map
    , none
    , placeholder
    , toBrowserDocument
    )

import Browser
import Colors
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Font
import Html.Attributes


type alias View msg =
    ResponsiveDevice
    ->
        { title : String
        , body : Element msg
        }


placeholder : String -> View msg
placeholder str =
    \_ ->
        { title = str
        , body =
            el [ width fill, height fill ] <|
                el [ centerX, centerY ] (Element.text str)
        }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    \device ->
        let
            view_ =
                view device
        in
        { title = view_.title
        , body = Element.map fn view_.body
        }


toBrowserDocument : ResponsiveDevice -> View msg -> Browser.Document msg
toBrowserDocument device view_ =
    let
        view =
            view_ device
    in
    { title = view.title
    , body =
        [ Element.layoutWith
            { options =
                [ focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ Font.sansSerifType
            , Font.size 14
            , Font.color Colors.text
            , Background.color Colors.background

            -- , Background.gradient
            --     { angle = pi
            --     , steps =
            --         [ Colors.white
            --         , Colors.background
            --         ]
            --     }
            ]
          <|
            Element.column
                [ width fill
                , height fill
                , alignTop

                -- height (px view.viewHeight)
                ]
                [ view.body
                ]
        ]
    }


type alias ResponsiveDevice =
    ( DeviceSize, DeviceWidth )


type DeviceSize
    = Large
    | Small


type DeviceWidth
    = Narrow
    | Wide


contentWidth : ResponsiveDevice -> Attribute msg
contentWidth device =
    case device of
        ( Small, _ ) ->
            width fill

        ( Large, Narrow ) ->
            width <| px 1000

        ( Large, Wide ) ->
            width <| px 1200


gutterWidth : ResponsiveDevice -> Attribute msg
gutterWidth device =
    case device of
        ( Small, Narrow ) ->
            paddingXY 16 24

        ( Small, Wide ) ->
            paddingXY 24 24

        ( Large, Narrow ) ->
            paddingXY 44 64

        ( Large, Wide ) ->
            paddingXY 44 44


content : ResponsiveDevice -> List (Attribute msg) -> List (Element msg) -> Element msg
content device attr children =
    Element.column
        (gutterWidth device
            :: contentWidth device
            :: centerX
            :: style "transition" "padding 200ms"
            :: attr
        )
        children


classifyDevice : { window | height : Int, width : Int } -> ResponsiveDevice
classifyDevice window =
    let
        deviceWidth =
            window.width
    in
    if deviceWidth < 784 then
        ( Small, Narrow )

    else if deviceWidth <= 1024 then
        ( Small, Wide )

    else if deviceWidth <= 1248 then
        ( Large, Narrow )

    else
        ( Large, Wide )


style property value =
    Html.Attributes.style property value
        |> htmlAttribute
