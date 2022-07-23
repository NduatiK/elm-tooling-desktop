module Api.EffectsProxy.EffectsProxy exposing
    (  cmd
       -- , task

    , taskReturningString
    , taskReturningType, taskReturningWhatever
    )

import Http exposing (Error(..), Expect, Response(..))
import Json.Decode as D
import Json.Encode as E exposing (Value)
import Task exposing (Task)


prefix : String
prefix =
    "data://db/"


cmd :
    { expect : Expect msg
    , functionName : String
    , arguments : List Value
    }
    -> Cmd msg
cmd { expect, functionName, arguments } =
    let
        url =
            prefix ++ functionName
    in
    if List.length arguments == 0 then
        Http.get
            { url = url
            , expect = expect
            }

    else
        Http.post
            { url = url
            , body = Http.jsonBody (E.list identity arguments)
            , expect = expect
            }



{-
   This might be preferred if you want synchronous or chainable or ordered tasks

   If you use Task.sequence, you can order functions
   If you use Task.sequence, you can order
-}


task :
    { resolver : Http.Resolver x a
    , functionName : String
    , arguments : List Value
    , timeout : Maybe Float
    }
    -> Task x a
task { resolver, functionName, arguments, timeout } =
    Http.task
        { method =
            if List.length arguments == 0 then
                "GET"

            else
                "POST"
        , headers = []
        , url = prefix ++ functionName
        , body = Http.jsonBody (E.list identity arguments)
        , resolver = resolver
        , timeout = timeout
        }


taskReturningString { functionName, arguments, timeout } =
    task
        { resolver = genericResolver (\body -> Ok body)
        , functionName = functionName
        , arguments = arguments
        , timeout = timeout
        }


taskReturningWhatever { functionName, arguments, timeout } =
    task
        { resolver = genericResolver (\body -> Ok ())
        , functionName = functionName
        , arguments = arguments
        , timeout = timeout
        }


taskReturningType { decoder, functionName, arguments, timeout } =
    task
        { resolver = jsonResolver decoder
        , functionName = functionName
        , arguments = arguments
        , timeout = timeout
        }


jsonResolver : D.Decoder a -> Http.Resolver Http.Error a
jsonResolver decoder =
    genericResolver
        (\body ->
            case D.decodeString decoder body of
                Ok value ->
                    Ok value

                Err err ->
                    Err (Http.BadBody (D.errorToString err))
        )


genericResolver fn =
    Http.stringResolver
        (\response ->
            case response of
                BadUrl_ error ->
                    Err <| BadUrl error

                Timeout_ ->
                    Err <| Timeout

                NetworkError_ ->
                    Err <| NetworkError

                BadStatus_ metadata body ->
                    Err <| BadStatus metadata.statusCode

                GoodStatus_ metadata body ->
                    fn body
        )
