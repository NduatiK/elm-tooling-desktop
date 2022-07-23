module Extra.ContextMenu.ContextMenu exposing
    ( ContextMenu, Msg, init, update, subscriptions, isOpen
    , Item, item, itemWithAnnotation, disabled, icon, shortcut
    , Config, Direction(..), Overflow(..), Cursor(..), defaultConfig
    , view, open, openIf
    , setOnDehover
    , close
    )

{-| The ContextMenu component that follows the Elm Architecture.

See [How to use](http://package.elm-lang.org/packages/jinjor/elm-contextmenu/latest).


# TEA Parts

The boilerplace functions. See [The Elm Architecture](https://guide.elm-lang.org/architecture/) for more information.

@docs ContextMenu, Msg, init, update, subscriptions, isOpen


# Item

@docs Item, item, itemWithAnnotation, disabled, icon, shortcut


# Config

@docs Config, Direction, Overflow, Cursor, defaultConfig


# View

@docs view, open, openIf


# Advanced

@docs setOnDehover

-}

import Browser
import Browser.Dom
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font exposing (alignRight)
import Extra.ContextMenu.Styles as S
import Html.Events
import Json.Decode as Decode
import Process
import Task exposing (Task)



-- TYPES


type alias Size =
    { width : Float
    , height : Float
    }


type alias Position =
    { x : Int
    , y : Int
    }



-- MODEL


{-| The Model. Put whatever context you like, which is used to create menu items.
-}
type ContextMenu context
    = ContextMenu { openState : OpenState context, closeOnDehover : Bool }


type HoverState
    = Container
    | ItemIndex ( Int, Int )
    | None


getItemIndex : HoverState -> Maybe ( Int, Int )
getItemIndex hover =
    case hover of
        ItemIndex index ->
            Just index

        _ ->
            Nothing


type alias OpenState context =
    Maybe
        { mouse : Position
        , window : Size
        , hover : HoverState
        , context : context
        }


shouldCloseOnClick : Bool -> OpenState context -> Bool
shouldCloseOnClick closeOnDehover openState =
    case openState of
        Just { hover } ->
            if closeOnDehover then
                False

            else
                hover /= Container

        Nothing ->
            True


setHoverState : HoverState -> OpenState context -> OpenState context
setHoverState hover openState =
    openState
        |> Maybe.map
            (\{ mouse, window, context } ->
                { mouse = mouse
                , window = window
                , hover = hover
                , context = context
                }
            )


{-| This switches when the menu should be closed.

  - True: Closes when mouse leaves the menu (keeps opening on cliking)
  - False(default): Closes when somewhere in the window is clicked

-}
setOnDehover : Bool -> ContextMenu context -> ContextMenu context
setOnDehover closeOnDehover (ContextMenu model) =
    ContextMenu { model | closeOnDehover = closeOnDehover }


enterItem : ( Int, Int ) -> OpenState context -> OpenState context
enterItem index openState =
    setHoverState (ItemIndex index) openState


leaveItem : OpenState context -> OpenState context
leaveItem openState =
    setHoverState Container openState


enterContainer : OpenState context -> OpenState context
enterContainer openState =
    setHoverState Container openState


leaveContainer : OpenState context -> OpenState context
leaveContainer openState =
    setHoverState None openState



-- UPDATE


{-| The Message.
-}
type Msg context
    = NoOp
    | RequestOpen context Position
    | Open context Position Size
    | Close
    | EnterItem ( Int, Int )
    | LeaveItem
    | EnterContainer
    | LeaveContainer


{-| The init function.
-}
init : ( ContextMenu context, Cmd (Msg context) )
init =
    ( ContextMenu { openState = Nothing, closeOnDehover = False }, Cmd.none )


isOpen : ContextMenu context -> Bool
isOpen (ContextMenu model) =
    model.openState /= Nothing


close : Msg context
close =
    Close


{-| The update function.
-}
update : Msg context -> ContextMenu context -> ( ContextMenu context, Cmd (Msg context) )
update msg (ContextMenu model) =
    case msg of
        NoOp ->
            ( ContextMenu model, Cmd.none )

        RequestOpen context mouse ->
            ( ContextMenu model
            , Task.perform (Open context mouse) windowSize
            )

        Open context mouse window ->
            ( ContextMenu
                { model
                    | openState =
                        Just
                            { mouse = mouse
                            , window = window
                            , hover = None
                            , context = context
                            }
                }
            , Cmd.none
            )

        Close ->
            ( ContextMenu { model | openState = Nothing }, Cmd.none )

        EnterItem index ->
            ( ContextMenu { model | openState = enterItem index model.openState }
            , Cmd.none
            )

        LeaveItem ->
            ( ContextMenu { model | openState = leaveItem model.openState }
            , Cmd.none
            )

        EnterContainer ->
            ( ContextMenu { model | openState = enterContainer model.openState }
            , Cmd.none
            )

        LeaveContainer ->
            if model.closeOnDehover then
                update Close (ContextMenu { model | openState = leaveContainer model.openState })

            else
                ( ContextMenu { model | openState = leaveContainer model.openState }, Cmd.none )


windowSize : Task x Size
windowSize =
    Browser.Dom.getViewport
        |> Task.map
            (\v ->
                Size v.viewport.width v.viewport.height
            )


{-| The Subscription.
-}
subscriptions : ContextMenu context -> Sub (Msg context)
subscriptions (ContextMenu model) =
    Sub.batch
        [ if shouldCloseOnClick model.closeOnDehover model.openState then
            Browser.Events.onMouseDown (Decode.succeed Close)

          else
            Sub.none
        ]



-- NUMBERS AND CALCULATION


disabledTextColor : Color
disabledTextColor =
    rgb255 200 200 200


annotationTextColor : Color
annotationTextColor =
    rgb255 200 200 200


shortcutTextColor : Color
shortcutTextColor =
    rgb255 200 200 200


containerBorderWidth : Int
containerBorderWidth =
    1


containerPadding : Int
containerPadding =
    4


partitionWidth : Int
partitionWidth =
    1


partitionMargin : Int
partitionMargin =
    2


defaultItemHeight : Int
defaultItemHeight =
    20


fontSize : Int
fontSize =
    14


annotationHeight : Int
annotationHeight =
    12


annotationFontSize : Int
annotationFontSize =
    10


menuWidthWithBorders : Int -> Int
menuWidthWithBorders menuWidth =
    menuWidth + containerBorderWidth * 2


calculateMenuHeight : List (List Item) -> Int
calculateMenuHeight groups =
    let
        containerBorders =
            containerBorderWidth * 2

        containerPaddings =
            containerPadding * 2

        partitions =
            (List.length groups - 1) * (partitionMargin * 2 + partitionWidth)

        items =
            List.sum
                (List.map
                    (\items_ ->
                        List.sum (List.map (\(Item item_) -> item_.height) items_)
                    )
                    groups
                )
    in
    containerBorders + containerPaddings + partitions + items


calculateX : Direction -> Overflow -> Float -> Int -> Float -> Float
calculateX direction overflow windowWidth menuWidth x =
    Basics.max 0 <|
        case direction of
            LeftBottom ->
                if x - toFloat menuWidth < 0 then
                    if overflow == Shift then
                        0

                    else
                        x

                else
                    x - toFloat menuWidth

            RightBottom ->
                if x + toFloat menuWidth > windowWidth then
                    if overflow == Shift then
                        windowWidth - toFloat menuWidth

                    else
                        x - toFloat menuWidth

                else
                    x


calculateY : Overflow -> Float -> Int -> Float -> Float
calculateY overflow windowHeight menuHeight y =
    Basics.max 0 <|
        if y + toFloat menuHeight > windowHeight then
            if overflow == Shift then
                windowHeight - toFloat menuHeight

            else
                y - toFloat menuHeight

        else
            y



-- ITEM


{-| The menu item. You can construct it with pipe-friendly functions.

    ContextMenu.item "Take photos"
        -- This library is outdated. See README.
        -- |> ContextMenu.icon FontAwesome.camera Color.green
        |> ContextMenu.disabled True

-}
type Item
    = Item
        { height : Int
        , icon : Maybe (Bool -> Element Never)
        , content : Content
        , shortcut : String
        , disabled : Bool
        }


type Content
    = Text String
    | Custom (Bool -> Element Never)


{-| Creates a simple text item.
-}
item : String -> Item
item s =
    Item
        { height = defaultItemHeight
        , icon = Nothing
        , content = Text s
        , shortcut = ""
        , disabled = False
        }


custom : Int -> (Bool -> Element Never) -> Item
custom height content =
    Item
        { height = Basics.max defaultItemHeight height
        , icon = Nothing
        , content = Custom content
        , shortcut = ""
        , disabled = False
        }


{-| Creates an item with annotation which will displayed just below the item name.
-}
itemWithAnnotation : String -> String -> Item
itemWithAnnotation s ann =
    custom (defaultItemHeight + annotationHeight - 2) (annotationView s ann)


{-| Disables the item. True = disabled, False = enabled.
-}
disabled : Bool -> Item -> Item
disabled disabled_ (Item item_) =
    Item { item_ | disabled = disabled_ }


{-| Displays the shortcut key at the right.
-}
shortcut : String -> Item -> Item
shortcut shortcutName (Item item_) =
    Item { item_ | shortcut = shortcutName }


{-| Shows the icon.

The first argument is a function that creates an icon,
given a color string(like `#fff`, `rgb(100,200,200)`, etc.) and icon size (px).

-}
icon : (Bool -> Element Never) -> Item -> Item
icon icon_ (Item item_) =
    Item { item_ | icon = Just icon_ }



-- CONFIG


{-| Defines the styles of the menu. See [examples](https://github.com/jinjor/elm-contextmenu/blob/master/examples/Configs.elm).
-}
type alias Config =
    { width : Int
    , direction : Direction
    , overflowX : Overflow
    , overflowY : Overflow
    , containerColor : Element.Color
    , hoverColor : Element.Color
    , invertText : Bool
    , cursor : Cursor
    , rounded : Bool
    , zoom : Float
    }


{-| The default direction the menu will be shown at.
-}
type Direction
    = LeftBottom
    | RightBottom


{-| The strategies how to show the menu when it goes out of window.
-}
type Overflow
    = Shift
    | Mirror


{-| The shape of cursor during hovering on the menu.
-}
type Cursor
    = Arrow
    | Pointer


{-| The default config.

    defaultConfig =
        { width = 300
        , direction = RightBottom
        , overflowX = Mirror
        , overflowY = Mirror
        , containerColor = rgb 1 1 1
        , hoverColor = rgb255 240 240 240
        , invertText = False
        , cursor = Pointer
        , rounded = False
        }

-}
defaultConfig : Config
defaultConfig =
    { width = 300
    , direction = RightBottom
    , overflowX = Mirror
    , overflowY = Mirror
    , containerColor = rgb 1 1 1
    , hoverColor = rgb255 240 240 240
    , invertText = False
    , cursor = Pointer
    , rounded = False
    , zoom = 1
    }



-- VIEW


{-| Makes the attribute that triggers to open the menu.
This attribute is passed for each element that needs a menu.

Arguments:

1.  function to transform component's message into user's message
2.  the context which is used to create items

-}
open : (Msg context -> msg) -> context -> Attribute msg
open transform context =
    openIf True transform context


{-| Similar to `open` but only works under particular condition.

This is useful for debugging on browser.

-}
openIf : Bool -> (Msg context -> msg) -> context -> Attribute msg
openIf condition transform context =
    htmlAttribute <|
        if condition then
            Html.Events.custom
                "contextmenu"
                (position
                    |> Decode.map (RequestOpen context)
                    |> Decode.map transform
                    |> Decode.map
                        (\msg ->
                            { message = msg
                            , stopPropagation = True
                            , preventDefault = True
                            }
                        )
                )

        else
            Html.Events.on "contextmenu" (Decode.succeed (transform NoOp))


position : Decode.Decoder Position
position =
    Decode.map2 Position
        (Decode.field "clientX" (Decode.float |> Decode.map round))
        (Decode.field "clientY" (Decode.float |> Decode.map round))


{-| Shows the menu. This should be called at only one place.

Arguments:

1.  the Config
2.  function to transform component's message into user's message
3.  function to create item groups
4.  the Model

-}
view : Config -> (Msg context -> msg) -> (context -> List (List ( Item, msg ))) -> ContextMenu context -> Element msg
view config transform toItemGroups (ContextMenu model) =
    case model.openState of
        Just { mouse, window, hover, context } ->
            let
                scaledMouse =
                    { x = toFloat mouse.x / config.zoom
                    , y = toFloat mouse.y / config.zoom
                    }

                groups =
                    toItemGroups context

                itemGroups =
                    List.map (List.map Tuple.first) groups

                groupsView =
                    List.indexedMap
                        (itemGroupView config transform (getItemIndex hover))
                        groups
            in
            case joinGroupsWithPartition groupsView of
                Just items ->
                    let
                        x_ =
                            calculateX
                                config.direction
                                config.overflowX
                                window.width
                                (menuWidthWithBorders config.width)
                                scaledMouse.x

                        y_ =
                            calculateY
                                config.overflowY
                                window.height
                                (calculateMenuHeight itemGroups)
                                scaledMouse.y
                    in
                    column
                        (S.container
                            { containerColor = config.containerColor
                            , borderWidth = containerBorderWidth
                            , containerPadding = containerPadding
                            , rounded = config.rounded
                            , width = config.width
                            , left = x_
                            , top = y_
                            , fontSize = fontSize
                            }
                            ++ [ onMouseEnter (transform EnterContainer)
                               , onMouseLeave (transform LeaveContainer)
                               , Border.shadow
                                    { offset = ( 0, 4 )
                                    , size = 0
                                    , blur = 5
                                    , color = rgba255 30 30 30 0.2
                                    }
                               ]
                        )
                        items

                Nothing ->
                    Element.text ""

        Nothing ->
            Element.text ""


joinGroupsWithPartition : List (List (Element msg)) -> Maybe (List (Element msg))
joinGroupsWithPartition groups =
    List.foldr
        (\group prev ->
            case prev of
                Just items ->
                    Just (group ++ (partition :: items))

                Nothing ->
                    Just group
        )
        Nothing
        groups


partition : Element msg
partition =
    el [ height (px (partitionWidth + partitionMargin + partitionMargin)), width fill ]
        (el (S.partition partitionWidth partitionMargin) none)


itemGroupView : Config -> (Msg context -> msg) -> Maybe ( Int, Int ) -> Int -> List ( Item, msg ) -> List (Element msg)
itemGroupView config transform hoverIndex groupIndex items =
    List.indexedMap (itemView config transform hoverIndex groupIndex) items


itemView : Config -> (Msg context -> msg) -> Maybe ( Int, Int ) -> Int -> Int -> ( Item, msg ) -> Element msg
itemView config transform hoverIndex groupIndex index ( Item item_, msg ) =
    let
        hovered =
            hoverIndex == Just ( groupIndex, index )

        styles =
            S.row
                config.hoverColor
                disabledTextColor
                config.invertText
                (config.cursor == Pointer)
                (toFloat item_.height)
                hovered
                item_.disabled
                (String.trim item_.shortcut /= "")
                ++ [ paddingXY 4 0, spacing 4 ]

        events =
            if item_.disabled then
                []

            else
                [ onMouseEnter (transform <| EnterItem ( groupIndex, index ))
                , onMouseLeave (transform <| LeaveItem)
                , onMouseDown msg
                ]

        icon_ =
            case item_.icon of
                Just icon__ ->
                    Element.map never <|
                        el [ width (px fontSize), height (px fontSize) ] <|
                            -- el
                            --     [ scale ((toFloat fontSize - 0.5) / 20)
                            --     , moveUp (toFloat (24 - fontSize) / 2)
                            --     , moveLeft (toFloat (24 - fontSize) / 2)
                            --     ]
                            -- <|
                            icon__ hovered

                Nothing ->
                    el [ width (px fontSize), height (px fontSize) ]
                        none

        content =
            case item_.content of
                Text s ->
                    el (S.text item_.height) (text s)

                Custom toElement ->
                    toElement item_.disabled

        shortCut =
            el
                (S.shortcut shortcutTextColor (toFloat item_.height) hovered)
                (text item_.shortcut)
    in
    row
        (styles ++ events)
        [ icon_
        , Element.map never content
        , shortCut
        ]


annotationView : String -> String -> Bool -> Element Never
annotationView s ann disabled_ =
    column []
        [ el
            (S.text defaultItemHeight)
            (text s)
        , el
            (S.annotation
                annotationTextColor
                (toFloat annotationHeight)
                (toFloat annotationFontSize)
                disabled_
            )
            (text ann)
        ]
