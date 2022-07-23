module Api.Request exposing
    ( get, put, post, delete
    , db
    )

{-|

@docs CSRFToken, encodeCSRFToken, dataCSRFAttribute
@docs get, put, post, delete

-}

import Html
import Html.Attributes
import Http



-- HTTP HELPERS


get :
    { url : String
    , expect : Http.Expect msg
    }
    -> Cmd msg
get =
    request "GET" Http.emptyBody


post :
    { url : String
    , body : Http.Body
    , expect : Http.Expect msg
    }
    -> Cmd msg
post options =
    request "POST" options.body options


put :
    { url : String
    , body : Http.Body
    , expect : Http.Expect msg
    }
    -> Cmd msg
put options =
    request "PUT" options.body options


delete :
    { url : String
    , expect : Http.Expect msg
    }
    -> Cmd msg
delete =
    request "DELETE" Http.emptyBody


db :
    { url : String
    , expect : Http.Expect msg
    , body : Http.Body
    }
    -> Cmd msg
db options =
    request "DB" options.body options


request :
    String
    -> Http.Body
    ->
        { options
            | url : String
            , expect : Http.Expect msg
        }
    -> Cmd msg
request method body options =
    Http.request
        { method = method
        , headers = []
        , url = options.url
        , body = body
        , expect = options.expect
        , timeout = Just (1000 * 60) -- 60 second timeout
        , tracker = Nothing
        }
