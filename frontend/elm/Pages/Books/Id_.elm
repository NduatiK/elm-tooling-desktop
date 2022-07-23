module Pages.Books.Id_ exposing (Model, Msg, page)

import Api.Book exposing (Block, Book, BookID(..), Chapter, ChapterID(..), Note, NoteID(..))
import Api.DB exposing (createChapter, createNote, saveChapter, saveNote)
import Api.Data as Data exposing (Data)
import Api.Ports exposing (focusAtEndOf)
import Browser.Navigation
import Colors exposing (background, clearBackground)
import Debounce
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events exposing (onClick, onDoubleClick, onMouseDown, onMouseEnter, onMouseLeave)
import Element.Font
import Element.Input as Input
import Extra.Animate as Animate
import Extra.KeyEvents as KeyEvents
import FeatherIcons exposing (loader)
import File
import File.Select
import Font
import Gen.Params.Books.Id_ exposing (Params)
import Gen.Route as Route
import Html
import Html.Attributes exposing (class, style)
import Html.Events exposing (stopPropagationOn)
import Json.Decode
import OrderedDict exposing (OrderedDict)
import Page
import Process
import Regex
import Request
import Set
import Shared
import Task
import View exposing (View)



-- import Extra.DropZone


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init req.params.id
        , update = update req.key
        , view = view shared.fontScale
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { id : String
    , book : Data Book
    , notes : Data (OrderedDict Int Api.Book.Note)
    , debounceNotes : Debounce.Debounce ()
    , hoveringOverBlock : Maybe ( NoteID, Int )
    , chapters : Data (OrderedDict Int Api.Book.Chapter)
    , section : Section
    , newNoteText : String
    , newChapterInputText : String
    , menuIsOpen : Bool
    , linkChapterMenuOpen : Bool
    , editingChapter : Maybe ChapterID
    }


toNoteID (Api.Book.NoteID noteID) =
    noteID


toChapterID (Api.Book.ChapterID chapterID) =
    chapterID


type NoteSelectionState
    = NoneSelected
    | FirstClick NoteID
    | DoubleClick NoteID
    | DoubleClickWithPending NoteID NoteID


type Section
    = Notes NoteSelectionState
    | Chapters NoteSelectionState
    | About


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 3000
    , transform = DebounceNotesMsg
    }


init : String -> ( Model, Effect Msg )
init id =
    ( { id = id
      , book = Data.Loading
      , notes = Data.Loading
      , debounceNotes = Debounce.init
      , hoveringOverBlock = Nothing
      , chapters = Data.Loading
      , section = Notes NoneSelected
      , newNoteText = ""
      , newChapterInputText = ""
      , menuIsOpen = False
      , linkChapterMenuOpen = False
      , editingChapter = Nothing
      }
    , case String.toInt id |> Maybe.map BookID of
        Just bookId ->
            Effect.fromCmd
                (Cmd.batch
                    [ Api.Ports.snapToTop NoOp
                    , Api.DB.getBook bookId GotBook
                    , Api.DB.listNotes bookId GotNotes
                    , Api.DB.listChapters bookId GotChapters
                    ]
                )

        Nothing ->
            Effect.none
    )



-- UPDATE


type Msg
    = StartedEnteringNote (Maybe ChapterID) String
    | ClickedSection Section
      --
    | UpdateTitle String
    | UpdateAuthor String
    | UpdateDescription String
    | UpdateDataURL String
    | GotFile File.File
    | ReplaceImage
      --
    | SaveAndReturnHome
    | ReturnHome
      --
    | InsertNoteIntoMemory Note
    | UpdatedTitle NoteID String
    | UpdatedBlock NoteID Int String
    | InsertBlockAt Api.Book.Block NoteID Int
    | BlockEvent NoteID Int KeyEvents.KeyEvent
    | NavigateToChapter ChapterID
    | EditingChapter (Maybe ChapterID)
    | FinishedEditingChapter ChapterID String
    | UpdatedChapterTitle ChapterID String
      --
    | SelectedNote NoteID
    | UnSelectedNote
    | HoverOver NoteID Int
    | StopHoverOver
    | DoubleClickTimedOut NoteID
    | ToggleLinkChapter
    | LinkNoteToChapter NoteID (Maybe ChapterID)
      --
    | CreateChapter
    | NewChapter (Data Chapter)
    | ChapterInputChanged String
      --
    | GotBook (Data Book)
    | GotNotes (Data (List Note))
    | GotChapters (Data (List Chapter))
      --
    | DebounceNotesMsg Debounce.Msg
    | SaveNotes
    | MoveToTrash
    | NoOp
    | FinishedFocus Note
    | OpenMenu
    | CloseMenu


update : Browser.Navigation.Key -> Msg -> Model -> ( Model, Effect Msg )
update key msg model =
    let
        updateNote : NoteID -> (Note -> Note) -> Data (OrderedDict Int Note) -> Data (OrderedDict Int Note)
        updateNote noteId updateFn notes =
            updateNoteMaybe noteId (updateFn >> Just) notes

        updateNoteMaybe : NoteID -> (Note -> Maybe Note) -> Data (OrderedDict Int Note) -> Data (OrderedDict Int Note)
        updateNoteMaybe noteId updateFn notes =
            notes
                |> Data.map
                    (OrderedDict.update (toNoteID noteId)
                        (Maybe.andThen updateFn)
                    )
    in
    maybeScheduleSaveNotes model.notes <|
        maybeUpdateBookInDB model.book <|
            case msg of
                GotBook book ->
                    ( { model | book = book }, Effect.none )

                GotNotes notes ->
                    ( { model
                        | notes =
                            notes
                                |> Data.toMaybe
                                |> Maybe.withDefault []
                                |> List.map (\note -> ( toNoteID note.id, note ))
                                |> OrderedDict.fromList
                                |> Data.Success
                      }
                    , Effect.none
                    )

                GotChapters chapters ->
                    ( { model
                        | chapters =
                            chapters
                                |> Data.toMaybe
                                |> Maybe.withDefault []
                                |> List.map (\chapter -> ( toChapterID chapter.id, chapter ))
                                |> OrderedDict.fromList
                                |> Data.Success
                      }
                    , Effect.none
                    )

                EditingChapter chapterId ->
                    ( { model | editingChapter = chapterId }
                    , if chapterId == Nothing then
                        Effect.none

                      else
                        Effect.fromCmd
                            (Api.Ports.focusAtEndOfWithDelay (Just <| "chapterEditInput") 100 NoOp)
                    )

                FinishedEditingChapter (ChapterID chapterID) title ->
                    ( { model
                        | editingChapter = Nothing
                        , chapters =
                            model.chapters
                                |> Data.map
                                    (\chapters ->
                                        chapters
                                            |> OrderedDict.update chapterID
                                                (Maybe.map
                                                    (\chapter ->
                                                        { chapter
                                                            | title =
                                                                if title == "" then
                                                                    "Untitled Chapter"

                                                                else
                                                                    title
                                                        }
                                                    )
                                                )
                                    )
                      }
                    , model.chapters
                        |> Data.toMaybe
                        |> Maybe.andThen
                            (\chapters ->
                                chapters
                                    |> OrderedDict.get chapterID
                            )
                        |> Maybe.map (\c -> Effect.fromCmd (saveChapter c NoOp))
                        |> Maybe.withDefault Effect.none
                    )

                UpdatedChapterTitle (ChapterID chapterID) title ->
                    ( { model
                        | chapters =
                            model.chapters
                                |> Data.map
                                    (\chapters ->
                                        chapters
                                            |> OrderedDict.update chapterID (Maybe.map (\chapter -> { chapter | title = title }))
                                    )
                      }
                    , Effect.none
                    )

                UpdateTitle title ->
                    ( { model
                        | book =
                            Data.map
                                (\book ->
                                    { book | title = title }
                                )
                                model.book
                      }
                    , Effect.none
                    )

                UpdateAuthor author ->
                    ( { model
                        | book =
                            Data.map
                                (\book ->
                                    { book | author = author }
                                )
                                model.book
                      }
                    , Effect.none
                    )

                ChapterInputChanged string ->
                    ( { model
                        | newChapterInputText = string
                      }
                    , Effect.none
                    )

                ToggleLinkChapter ->
                    ( { model
                        | linkChapterMenuOpen = not model.linkChapterMenuOpen
                      }
                    , Effect.none
                    )

                LinkNoteToChapter noteId chapterId ->
                    ( { model
                        | linkChapterMenuOpen = False
                        , notes =
                            model.notes
                                |> Data.map
                                    (OrderedDict.update (toNoteID noteId) (Maybe.map (\note_ -> { note_ | chapterId = chapterId })))
                      }
                    , Effect.none
                    )

                CreateChapter ->
                    case ( model.book, model.chapters ) of
                        ( Data.Success book, Data.Success chapters_ ) ->
                            ( { model
                                | newChapterInputText = ""
                              }
                            , Effect.fromCmd
                                (createChapter book.id model.newChapterInputText (OrderedDict.size chapters_) NewChapter)
                            )

                        _ ->
                            ( model, Effect.none )

                NewChapter chapter ->
                    case ( chapter, model.chapters ) of
                        ( Data.Success chapter_, Data.Success chapters_ ) ->
                            ( { model
                                | chapters =
                                    Data.Success
                                        (chapters_
                                            |> OrderedDict.insert (toChapterID chapter_.id) chapter_
                                        )
                              }
                            , Effect.none
                            )

                        _ ->
                            ( model
                            , Effect.none
                            )

                ReplaceImage ->
                    ( model
                    , Effect.fromCmd
                        (File.Select.file mimeImages GotFile)
                    )

                GotFile file ->
                    ( model
                    , Effect.fromCmd
                        (Task.perform
                            UpdateDataURL
                            (File.toUrl file)
                        )
                    )

                UpdateDataURL dataUrl ->
                    ( { model
                        | book =
                            Data.map
                                (\book ->
                                    { book | imgUrl = dataUrl }
                                )
                                model.book
                      }
                    , Effect.none
                    )

                UpdateDescription description ->
                    ( { model
                        | book =
                            Data.map
                                (\book ->
                                    { book | about = description }
                                )
                                model.book
                      }
                    , Effect.none
                    )

                MoveToTrash ->
                    ( model
                    , case Data.toMaybe model.book of
                        Just book ->
                            Effect.fromCmd (Api.DB.deleteBook book.id SaveAndReturnHome)

                        Nothing ->
                            Effect.none
                    )

                SaveAndReturnHome ->
                    ( model
                    , Effect.fromCmd
                        (saveNotes model
                            |> Task.attempt (\_ -> ReturnHome)
                        )
                    )

                ReturnHome ->
                    ( model
                    , Effect.fromCmd (Browser.Navigation.pushUrl key (Route.toHref Route.Home_))
                    )

                NoOp ->
                    ( model, Effect.none )

                FinishedFocus note ->
                    ( { model
                        | newNoteText = ""
                        , notes =
                            model.notes
                                |> Data.map
                                    (OrderedDict.update (toNoteID note.id) (Maybe.map (\note_ -> { note_ | title = model.newNoteText })))
                        , section =
                            case model.section of
                                Chapters _ ->
                                    Chapters (DoubleClick note.id)

                                _ ->
                                    Notes (DoubleClick note.id)
                      }
                    , Effect.none
                    )

                InsertNoteIntoMemory note ->
                    ( { model
                        | notes =
                            model.notes
                                |> Data.map
                                    (OrderedDict.toList
                                        >> (\list -> ( toNoteID note.id, { note | title = model.newNoteText } ) :: list)
                                        >> OrderedDict.fromList
                                    )
                        , section =
                            case model.section of
                                Chapters _ ->
                                    Chapters (DoubleClick note.id)

                                _ ->
                                    Notes (DoubleClick note.id)
                      }
                    , Effect.batch
                        [ Effect.fromCmd
                            (saveNote note NoOp)
                        , Effect.fromCmd
                            (focusAtEndOf (Just <| "note-title-" ++ String.fromInt (toNoteID note.id)) (FinishedFocus note))
                        ]
                    )

                InsertBlockAt block noteId blockIndex ->
                    ( { model
                        | notes =
                            model.notes
                                |> updateNote noteId
                                    (\note ->
                                        { note
                                            | blocks =
                                                if blockIndex < List.length note.blocks then
                                                    note.blocks
                                                        |> List.indexedMap
                                                            (\index oldBlock ->
                                                                if index == blockIndex then
                                                                    [ block, oldBlock ]

                                                                else
                                                                    [ oldBlock ]
                                                            )
                                                        |> List.concat

                                                else
                                                    note.blocks ++ [ block ]
                                        }
                                    )
                      }
                    , Effect.fromCmd
                        (focusAtEndOf (Just <| "note-title-" ++ String.fromInt (toNoteID noteId) ++ "-block-" ++ String.fromInt blockIndex) NoOp)
                    )

                BlockEvent noteId blockIndex keyEvent ->
                    let
                        ( shouldMoveFocus, updatedNotes ) =
                            case keyEvent.key of
                                KeyEvents.Backspace ->
                                    let
                                        innerUpdatedNotes =
                                            model.notes
                                                |> updateNoteMaybe noteId
                                                    (\t ->
                                                        let
                                                            newBlocks =
                                                                t.blocks
                                                                    |> List.indexedMap
                                                                        (\sInd block ->
                                                                            if sInd == blockIndex && Api.Book.blockIsEmpty block then
                                                                                []

                                                                            else
                                                                                [ block ]
                                                                        )
                                                                    |> List.concat
                                                        in
                                                        if newBlocks == [] && t.title == "" then
                                                            Nothing

                                                        else
                                                            Just { t | blocks = newBlocks }
                                                    )

                                        innerShouldMoveFocus =
                                            let
                                                oldCount =
                                                    model.notes
                                                        |> Data.toMaybe
                                                        |> Maybe.withDefault OrderedDict.empty
                                                        |> OrderedDict.get (toNoteID noteId)
                                                        |> Maybe.map (\t -> List.length t.blocks)

                                                newCount =
                                                    innerUpdatedNotes
                                                        |> Data.toMaybe
                                                        |> Maybe.withDefault OrderedDict.empty
                                                        |> OrderedDict.get (toNoteID noteId)
                                                        |> Maybe.map (\t -> List.length t.blocks)
                                            in
                                            newCount /= oldCount
                                    in
                                    ( innerShouldMoveFocus, innerUpdatedNotes )

                                _ ->
                                    ( True, model.notes )
                    in
                    ( { model
                        | notes = updatedNotes
                      }
                    , Effect.fromCmd <|
                        Cmd.batch
                            [ if shouldMoveFocus then
                                focusAtEndOf
                                    (case keyEvent.key of
                                        KeyEvents.Backspace ->
                                            Just <| "note-title-" ++ String.fromInt (toNoteID noteId) ++ "-block-" ++ String.fromInt (blockIndex - 1)

                                        -- KeyEvents.ArrowUp ->
                                        -- if blockIndex > 0 then
                                        --     Just <| "note-title-" ++ String.fromInt (toNoteID noteId) ++ "-block-" ++ String.fromInt (blockIndex - 1)
                                        -- else
                                        --     Nothing
                                        -- KeyEvents.ArrowDown ->
                                        --     Just <| "note-title-" ++ String.fromInt (toNoteID noteId) ++ "-block-" ++ String.fromInt (blockIndex + 1)
                                        -- KeyEvents.Enter ->
                                        --     "note-title-" ++ String.fromInt(toNoteID noteId) ++ "-block-" ++ String.fromInt (blockIndex + 1)
                                        _ ->
                                            -- Enter is handled by `UpdateSubTask`
                                            Nothing
                                    )
                                    NoOp

                              else
                                Cmd.none
                            , deleteRemovedNotesFromDB model.notes updatedNotes
                            ]
                    )

                StartedEnteringNote chapterId noteTitle ->
                    case Data.toMaybe model.book of
                        Just { id } ->
                            ( { model | newNoteText = noteTitle }
                            , if model.newNoteText == "" then
                                Effect.fromCmd
                                    (createNote id
                                        chapterId
                                        (\result ->
                                            case result of
                                                Data.Success note ->
                                                    InsertNoteIntoMemory note

                                                _ ->
                                                    NoOp
                                        )
                                    )

                              else
                                Effect.none
                            )

                        Nothing ->
                            ( model, Effect.none )

                DebounceNotesMsg msg_ ->
                    let
                        ( debounceNotes, cmd ) =
                            Debounce.update
                                debounceConfig
                                (Debounce.takeLast sendSaveNotesMsg)
                                msg_
                                model.debounceNotes
                    in
                    ( { model | debounceNotes = debounceNotes }
                    , Effect.fromCmd cmd
                    )

                HoverOver noteId blockIndex ->
                    ( { model
                        | hoveringOverBlock = Just ( noteId, blockIndex )
                      }
                    , Effect.none
                    )

                StopHoverOver ->
                    ( { model
                        | hoveringOverBlock = Nothing
                      }
                    , Effect.none
                    )

                CloseMenu ->
                    ( { model
                        | menuIsOpen = False
                      }
                    , Effect.none
                    )

                OpenMenu ->
                    ( { model
                        | menuIsOpen = True
                      }
                    , Effect.none
                    )

                NavigateToChapter (ChapterID chapterId) ->
                    ( { model
                        | section = Chapters NoneSelected
                      }
                    , Effect.fromCmd
                        (Api.Ports.scrollToElement { id = "chapter-title-" ++ String.fromInt chapterId } NoOp)
                    )

                UpdatedTitle noteId title ->
                    let
                        updatedNotes =
                            model.notes
                                |> updateNoteMaybe noteId
                                    (\note ->
                                        if title == "" && List.isEmpty note.blocks then
                                            Nothing

                                        else
                                            Just { note | title = title }
                                    )
                    in
                    ( { model
                        | notes = updatedNotes
                      }
                    , Effect.fromCmd (deleteRemovedNotesFromDB model.notes updatedNotes)
                    )

                UpdatedBlock noteId blockIndex newText ->
                    ( { model
                        | notes =
                            model.notes
                                |> updateNote noteId
                                    (\note ->
                                        { note
                                            | blocks =
                                                note.blocks
                                                    |> List.indexedMap
                                                        (\index block ->
                                                            if index == blockIndex then
                                                                case block of
                                                                    Api.Book.Heading _ ->
                                                                        Api.Book.Heading newText

                                                                    Api.Book.BlockQuote _ ->
                                                                        Api.Book.BlockQuote newText

                                                                    Api.Book.Text _ ->
                                                                        Api.Book.Text newText

                                                            else
                                                                block
                                                        )
                                        }
                                    )
                      }
                    , Effect.none
                    )

                SaveNotes ->
                    ( model
                    , Effect.fromCmd (saveNotesCmd model)
                    )

                ClickedSection section ->
                    ( { model | section = section }, Effect.none )

                UnSelectedNote ->
                    case model.section of
                        Notes _ ->
                            ( { model | section = Notes NoneSelected, linkChapterMenuOpen = False }, Effect.none )

                        Chapters _ ->
                            ( { model | section = Chapters NoneSelected, linkChapterMenuOpen = False }, Effect.none )

                        _ ->
                            ( model, Effect.none )

                SelectedNote noteId ->
                    handleNoteClick model noteId

                DoubleClickTimedOut noteId ->
                    handleClickTimeOut model noteId


sendSaveNotesMsg : () -> Cmd Msg
sendSaveNotesMsg _ =
    Task.perform (\_ -> SaveNotes) (Task.succeed ())


saveNotes model =
    model.notes
        |> Data.toMaybe
        |> Maybe.map OrderedDict.values
        |> Maybe.map Api.DB.updateNotes
        |> Maybe.withDefault (Task.succeed ())


saveNotesCmd model =
    Task.attempt (\_ -> NoOp) (saveNotes model)


deleteRemovedNotesFromDB : Data (OrderedDict Int Note) -> Data (OrderedDict Int Note) -> Cmd Msg
deleteRemovedNotesFromDB original updated =
    Set.fromList
        (updated
            |> Data.map OrderedDict.keys
            |> Data.toMaybe
            |> Maybe.withDefault []
        )
        |> Set.diff
            (Set.fromList
                (original
                    |> Data.map OrderedDict.keys
                    |> Data.toMaybe
                    |> Maybe.withDefault []
                )
            )
        |> Set.toList
        |> List.map (\id -> Api.DB.deleteNote id NoOp)
        |> Cmd.batch


maybeScheduleSaveNotes : Data (OrderedDict Int Api.Book.Note) -> ( Model, Effect Msg ) -> ( Model, Effect Msg )
maybeScheduleSaveNotes oldNotes_ ( model, effect ) =
    case ( oldNotes_, model.notes ) of
        ( Data.Success oldNotes, Data.Success notes ) ->
            if oldNotes /= notes then
                let
                    -- Push your values here.
                    ( debounce, cmd ) =
                        Debounce.push debounceConfig () model.debounceNotes
                in
                ( { model
                    | debounceNotes = debounce
                  }
                , Effect.batch
                    [ effect
                    , Effect.fromCmd cmd
                    ]
                )

            else
                ( model, effect )

        _ ->
            ( model, effect )


maybeUpdateBookInDB : Data Book -> ( Model, Effect Msg ) -> ( Model, Effect Msg )
maybeUpdateBookInDB oldBook_ ( model, effect ) =
    case ( oldBook_, model.book ) of
        ( Data.Success oldBook, Data.Success book ) ->
            if oldBook /= book then
                ( model
                , Effect.batch
                    [ effect
                    , Effect.fromCmd (Api.DB.updateBook book (\_ -> NoOp))
                    ]
                )

            else
                ( model, effect )

        _ ->
            ( model, effect )


handleNoteClick : Model -> NoteID -> ( Model, Effect Msg )
handleNoteClick model noteId =
    let
        backToSection input =
            case model.section of
                Notes _ ->
                    Notes input

                Chapters _ ->
                    Chapters input

                About ->
                    About

        sectionSelection =
            case model.section of
                Notes selection ->
                    Just selection

                Chapters selection ->
                    Just selection

                About ->
                    Nothing
    in
    (\( model_, msg ) -> ( { model_ | menuIsOpen = False }, msg )) <|
        case sectionSelection of
            Just NoneSelected ->
                ( { model | section = backToSection (FirstClick noteId) }, startDoubleClickTimerFor noteId )

            Just (FirstClick currentNoteId) ->
                if currentNoteId == noteId then
                    ( { model | section = backToSection (DoubleClick noteId) }, Effect.none )

                else
                    ( { model | section = backToSection (FirstClick noteId) }, startDoubleClickTimerFor noteId )

            Just (DoubleClick currentNoteId) ->
                if currentNoteId == noteId then
                    ( model, Effect.none )

                else
                    ( { model | section = backToSection (DoubleClickWithPending currentNoteId noteId) }, startFastDoubleClickTimerFor currentNoteId )

            Just (DoubleClickWithPending currentNoteId pending) ->
                if pending == noteId then
                    ( { model | section = backToSection (DoubleClick noteId) }, Effect.none )

                else
                    ( { model | section = backToSection (DoubleClickWithPending currentNoteId noteId) }, startFastDoubleClickTimerFor noteId )

            _ ->
                ( model, Effect.none )


handleClickTimeOut : Model -> NoteID -> ( Model, Effect Msg )
handleClickTimeOut model noteId =
    case model.section of
        Notes NoneSelected ->
            ( model, Effect.none )

        Notes (FirstClick currentNoteId) ->
            if currentNoteId == noteId then
                ( { model | section = Notes NoneSelected }, Effect.none )

            else
                ( model, Effect.none )

        Notes (DoubleClick _) ->
            ( model, Effect.none )

        Notes (DoubleClickWithPending currentNoteId pending) ->
            if currentNoteId == noteId then
                ( { model | section = Notes (FirstClick pending) }, startDoubleClickTimerFor pending )

            else
                ( model, Effect.none )

        _ ->
            ( model, Effect.none )


noteIsSelected : NoteSelectionState -> Note -> Bool
noteIsSelected state note =
    case state of
        NoneSelected ->
            False

        FirstClick _ ->
            False

        DoubleClick currentNoteId ->
            if currentNoteId == note.id then
                True

            else
                False

        DoubleClickWithPending currentNoteId _ ->
            if currentNoteId == note.id then
                True

            else
                False


startFastDoubleClickTimerFor : NoteID -> Effect Msg
startFastDoubleClickTimerFor noteId =
    Effect.fromCmd
        (Process.sleep 150
            |> Task.perform (\_ -> DoubleClickTimedOut noteId)
        )


startDoubleClickTimerFor : NoteID -> Effect Msg
startDoubleClickTimerFor noteId =
    Effect.fromCmd
        (Process.sleep 500
            |> Task.perform (\_ -> DoubleClickTimedOut noteId)
        )


mimeImages : List String
mimeImages =
    [ "image/png", "image/jpg", "image/jpeg", "image/webp" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Float -> Model -> View Msg
view fontScale model device =
    { title = "Home"
    , body =
        -- case ( model.book, model.notes ) of
        --     ( Data.Success book, Data.Success notes ) ->
        none

    -- _ ->
    -- el [ centerX, centerY, Font.color Colors.black ]
    --     (loader
    --         |> FeatherIcons.toHtml []
    --         |> html
    --     )
    }
