module Components exposing (..)

import Html exposing (Html)
import Svg
import Svg.Attributes as SvgA


progress : Float -> Html msg
progress percent =
    let
        svg_value =
            (1 - percent)
                * magic

        magic =
            219.91148575129
    in
    Svg.svg
        [ SvgA.class "radial-progress"
        , SvgA.viewBox "0 0 80 80"
        , SvgA.width "100%"
        , SvgA.height "100%"
        ]
        [ Svg.circle
            [ SvgA.class "incomplete"
            , SvgA.cx "40"
            , SvgA.cy "40"
            , SvgA.r "35"
            , SvgA.style ("stroke-dashoffset: " ++ String.fromFloat magic ++ "px")
            ]
            []
        , Svg.circle
            [ SvgA.class "complete"
            , SvgA.cx "40"
            , SvgA.cy "40"
            , SvgA.r "35"
            , SvgA.style ("stroke-dashoffset: " ++ String.fromFloat svg_value ++ "px; stroke-dasharray: " ++ String.fromFloat magic ++ "px")
            ]
            []
        ]
