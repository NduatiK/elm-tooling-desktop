module Pages.Home_ exposing (Model, Msg, page)

import Api.Book exposing (Book)
import Api.DB
import Api.Data as Data exposing (Data)
import Colors exposing (background, clearBackground)
import Components
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons exposing (loader)
import Font
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html.Attributes exposing (class)
import Icons
import Page
import Request
import Shared
import Time
import View as V exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.advanced
        { init = init
        , update = update
        , view = view shared.fontScale
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { books : Data (List Book)
    }


init : ( Model, Effect Msg )
init =
    ( { books = Data.Loading
      }
    , listBooks
    )


listBooks =
    Effect.fromCmd
        -- (Api.DB.listBooks GotBooks)
        (Api.DB.listBooks <| always NoOp)


createBook =
    Effect.fromCmd
        (Api.DB.createBook
            { id = Api.Book.BookID 0
            , title = "Can't Hurt Me"
            , author = "David Goggins"
            , imgUrl = ""
            , about = ""
            , createdAt = Time.millisToPosix 0
            , updatedAt = Time.millisToPosix 0
            }
            -- (\_ -> GetBooks)
            (always NoOp)
        )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Float -> Model -> View Msg
view fontScale model device =
    { title = "Home"
    , body =
        V.content device
            [ spacing 56 ]
            [ viewHeading
            , case device of
                ( V.Small, _ ) ->
                    column
                        [ width fill, spacing 36 ]
                        [ viewBalance
                        , viewPortfolio
                        ]

                ( V.Large, _ ) ->
                    row
                        [ width fill, spacing 100 ]
                        [ column [ width (fillPortion 3) ] [ viewPortfolio ]
                        , column [ width fill, alignTop ]
                            [ viewBalance
                            ]
                        ]
            , viewTopAssets
            ]
    }


viewHeading =
    row [ width fill ]
        [ el
            [ Font.serifType
            , Font.size 32
            , Font.light
            ]
          <|
            text "Assets"
        ]


viewPortfolio =
    column [ width fill ]
        [ row [ width fill, Font.medium, Font.letterSpacing 0.2 ]
            [ text "PORTFOLIO"
            , row [ alignRight ]
                [ el [] (text "YTD")
                , el [] none
                ]
            ]
        , el [ height (px 280), width fill ] none
        ]


viewTopAssets =
    column [ width fill ]
        [ el [ width fill, Font.medium, Font.letterSpacing 0.2 ] <| text "TOP ASSETS"
        , el [ height (px 400), width fill ] none
        ]


viewBalance =
    column [ width fill, alignTop, spacing 12 ]
        [ el [ Font.medium, Font.letterSpacing 0.2 ] <|
            text "BALANCE"
        , el [ Font.bold, Font.letterSpacing 0.2, Font.size 28 ] <|
            text "KES12,201"
        ]
