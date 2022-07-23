module Api.Ports exposing (..)

import Api.Book
import Api.Data exposing (Data)
import Api.EffectsProxy.EffectsProxy as EffectsProxy
import FeatherIcons exposing (image)
import Http
import Json.Decode
import Json.Encode


smoothScrollToTop : msg -> Cmd msg
smoothScrollToTop noOp =
    EffectsProxy.cmd
        { expect = Api.Data.expectWhatever noOp
        , functionName = "scrollToTop"
        , arguments = []
        }


snapToTop : msg -> Cmd msg
snapToTop noOp =
    EffectsProxy.cmd
        { expect = Api.Data.expectWhatever noOp
        , functionName = "snapToTop"
        , arguments = []
        }


focusAtEndOf : Maybe String -> msg -> Cmd msg
focusAtEndOf id_ noOp =
    case id_ of
        Just id ->
            EffectsProxy.cmd
                { expect = Api.Data.expectWhatever noOp
                , functionName = "focusAtEndOf"
                , arguments = [ Json.Encode.string id ]
                }

        Nothing ->
            Cmd.none


focusAtEndOfWithDelay : Maybe String -> Int -> msg -> Cmd msg
focusAtEndOfWithDelay id_ delayInMs noOp =
    case id_ of
        Just id ->
            EffectsProxy.cmd
                { expect = Api.Data.expectWhatever noOp
                , functionName = "focusAtEndOf"
                , arguments = [ Json.Encode.string id, Json.Encode.int delayInMs ]
                }

        Nothing ->
            Cmd.none


scrollToElement : { id : String } -> msg -> Cmd msg
scrollToElement { id } noOp =
    EffectsProxy.cmd
        { expect = Api.Data.expectWhatever noOp
        , functionName = "scrollToElement"
        , arguments = [ Json.Encode.string id ]
        }
