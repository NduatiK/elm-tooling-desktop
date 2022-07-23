module Extra.Animate exposing (..)

import Element exposing (..)
import Html
import Html.Attributes


all =
    custom "all" "300ms"


allWithDuration duration =
    custom "all" duration


custom : String -> String -> List (Attribute msg)
custom property duration =
    [ Html.Attributes.style "-webkit-transition" (property ++ " " ++ duration ++ " ease")
        |> htmlAttribute
    , Html.Attributes.style "-moz-transition" (property ++ " " ++ duration ++ " ease")
        |> htmlAttribute
    , Html.Attributes.style "-o-transition" (property ++ " " ++ duration ++ " ease")
        |> htmlAttribute
    , Html.Attributes.style "transition" (property ++ " " ++ duration ++ " ease")
        |> htmlAttribute
    ]
