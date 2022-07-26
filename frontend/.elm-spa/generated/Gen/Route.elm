module Gen.Route exposing
    ( Route(..)
    , fromUrl
    , toHref
    )

import Gen.Params.Books.Id_
import Gen.Params.Home_
import Gen.Params.NotFound
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Home_
    | NotFound
    | Books__Id_ { id : String }


fromUrl : Url -> Route
fromUrl =
    Parser.parse (Parser.oneOf routes) >> Maybe.withDefault NotFound


routes : List (Parser (Route -> a) a)
routes =
    [ Parser.map Home_ Gen.Params.Home_.parser
    , Parser.map NotFound Gen.Params.NotFound.parser
    , Parser.map Books__Id_ Gen.Params.Books.Id_.parser
    ]


toHref : Route -> String
toHref route =
    let
        joinAsHref : List String -> String
        joinAsHref segments =
            "/" ++ String.join "/" segments
    in
    case route of
        Home_ ->
            joinAsHref []

        NotFound ->
            joinAsHref [ "#", "not-found" ]

        Books__Id_ params ->
            joinAsHref [ "#", "books", params.id ]
