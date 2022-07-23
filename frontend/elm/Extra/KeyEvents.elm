module Extra.KeyEvents exposing
    ( Key(..)
    , KeyEvent
    , Modifier(..)
    , onKeyDown
    , onKeyDownMatch
    , onKeyDownMatchWithModifiers
    , onKeyDownWithModifiers
    , onKeyUp
    , onKeyUpMatch
    , onKeyUpMatchWithModifier
    , onKeyUpWithModifiers
    )

import Element exposing (..)
import Html.Events
import Json.Decode as Json exposing (Decoder)


type Key
    = Backspace
    | Delete
    | Enter
    | ArrowDown
    | ArrowUp
    | Esc
    | CtrlOrMeta


type Modifier
    = CtrlOrMetaModifier
    | ShiftModifier
    | AltModifier


type alias KeyEvent =
    { key : Key
    , modifiers : List Modifier
    }


stringToKey : String -> Decoder Key
stringToKey str =
    case str of
        "Backspace" ->
            Json.succeed Backspace

        "Delete" ->
            Json.succeed Backspace

        "ArrowUp" ->
            Json.succeed ArrowUp

        "ArrowDown" ->
            Json.succeed ArrowDown

        "Enter" ->
            Json.succeed Enter

        "Esc" ->
            Json.succeed Esc

        "Contrl" ->
            Json.succeed CtrlOrMeta

        "Meta" ->
            Json.succeed CtrlOrMeta

        _ ->
            Json.fail "not used key"


stringToModifier : ModifierStatus -> Decoder (List Modifier)
stringToModifier status =
    [ if status.altKey then
        [ AltModifier ]

      else
        []
    , if status.metaKey then
        [ CtrlOrMetaModifier ]

      else
        []
    , if status.ctrlKey then
        [ CtrlOrMetaModifier ]

      else
        []
    , if status.shiftKey then
        [ ShiftModifier ]

      else
        []
    ]
        |> List.concat
        |> Json.succeed


type alias ModifierStatus =
    { altKey : Bool
    , metaKey : Bool
    , ctrlKey : Bool
    , shiftKey : Bool
    }


jsonDecode =
    Json.map2 KeyEvent
        (Json.field "key" Json.string
            |> Json.andThen stringToKey
        )
        (Json.map4 ModifierStatus
            (Json.field "altKey" Json.bool)
            (Json.field "metaKey" Json.bool)
            (Json.field "ctrlKey" Json.bool)
            (Json.field "shiftKey" Json.bool)
            |> Json.andThen stringToModifier
        )


decodeKey : (KeyEvent -> value) -> Decoder value
decodeKey msg =
    jsonDecode
        |> Json.map msg


decodeKeyMatch : Key -> msg -> Decoder msg
decodeKeyMatch key msg =
    jsonDecode
        |> Json.andThen
            (\result ->
                if result.key == key then
                    Json.succeed msg

                else
                    Json.fail "wrong key"
            )


onKeyDown : (Key -> msg) -> Attribute msg
onKeyDown msg =
    Html.Events.on "keydown" (decodeKey (.key >> msg))
        |> htmlAttribute


onKeyUp : (Key -> msg) -> Attribute msg
onKeyUp msg =
    Html.Events.on "keyup" (decodeKey (.key >> msg))
        |> htmlAttribute


onKeyDownMatch : Key -> msg -> Attribute msg
onKeyDownMatch key msg =
    Html.Events.on "keydown" (decodeKeyMatch key msg)
        |> htmlAttribute


onKeyUpMatch : Key -> msg -> Attribute msg
onKeyUpMatch key msg =
    Html.Events.on "keyup" (decodeKeyMatch key msg)
        |> htmlAttribute


onKeyDownWithModifiers : (KeyEvent -> msg) -> Attribute msg
onKeyDownWithModifiers msg =
    Html.Events.on "keydown" (decodeKey msg)
        |> htmlAttribute


onKeyUpWithModifiers : (KeyEvent -> msg) -> Attribute msg
onKeyUpWithModifiers msg =
    Html.Events.on "keyup" (decodeKey msg)
        |> htmlAttribute


onKeyDownMatchWithModifiers : Key -> msg -> Attribute msg
onKeyDownMatchWithModifiers key msg =
    Html.Events.on "keydown" (decodeKeyMatch key msg)
        |> htmlAttribute


onKeyUpMatchWithModifier : Key -> Modifier -> msg -> Attribute msg
onKeyUpMatchWithModifier key modifier msg =
    Html.Events.on "keyup" (decodeKeyMatch key msg)
        |> htmlAttribute
