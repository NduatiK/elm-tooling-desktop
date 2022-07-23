module Api.DB exposing (..)

import Api.Book
import Api.Data exposing (Data)
import Api.EffectsProxy.EffectsProxy as EffectsProxy
import FeatherIcons exposing (image)
import Http
import Json.Decode
import Json.Encode as Json
import Task


listBooks : (Data (List Api.Book.Book) -> msg) -> Cmd msg
listBooks onResponse =
    EffectsProxy.cmd
        { expect = Api.Data.expectJson onResponse (Json.Decode.list Api.Book.decoder)
        , functionName = "listBooks"
        , arguments = []
        }


createBook : Api.Book.Book -> (Data Api.Book.Book -> msg) -> Cmd msg
createBook book onResponse =
    EffectsProxy.cmd
        { expect = Api.Data.expectJson onResponse Api.Book.decoder
        , functionName = "createBook"
        , arguments = [ Api.Book.encoder { book | id = Api.Book.BookID 0 } ]
        }


updateBook : Api.Book.Book -> (Data Api.Book.Book -> msg) -> Cmd msg
updateBook book onResponse =
    EffectsProxy.cmd
        { expect = Api.Data.expectJson onResponse Api.Book.decoder
        , functionName = "updateBook"
        , arguments = [ Api.Book.encoder book ]
        }


getBook : Api.Book.BookID -> (Data Api.Book.Book -> msg) -> Cmd msg
getBook (Api.Book.BookID id) onResponse =
    EffectsProxy.cmd
        { expect = Api.Data.expectJson onResponse Api.Book.decoder
        , functionName = "getBook"
        , arguments = [ Json.int id ]
        }


deleteBook : Api.Book.BookID -> b -> Cmd b
deleteBook (Api.Book.BookID id) onResponse =
    EffectsProxy.cmd
        { expect = Api.Data.expectWhatever onResponse
        , functionName = "deleteBook"
        , arguments = [ Json.int id ]
        }


listNotes : Api.Book.BookID -> (Data (List Api.Book.Note) -> msg) -> Cmd msg
listNotes (Api.Book.BookID id) onResponse =
    EffectsProxy.cmd
        { expect = Api.Data.expectJson onResponse (Json.Decode.list Api.Book.noteDecoder)
        , functionName = "listNotes"
        , arguments = [ Json.int id ]
        }


createNote : Api.Book.BookID -> Maybe Api.Book.ChapterID -> (Data Api.Book.Note -> msg) -> Cmd msg
createNote (Api.Book.BookID bookId) chapterId msg =
    EffectsProxy.cmd
        { expect = Api.Data.expectJson msg Api.Book.noteDecoder
        , functionName = "createNote"
        , arguments =
            [ Json.object
                [ ( "id", Json.int 0 )
                , ( "bookId", Json.int bookId )
                , ( "chapterId"
                  , chapterId
                        |> Maybe.map (\(Api.Book.ChapterID chapterId_) -> Json.int chapterId_)
                        |> Maybe.withDefault Json.null
                  )
                , ( "blocks", Json.string <| Json.encode 0 (Json.list Api.Book.blockEncoder [ Api.Book.Text "" ]) )
                ]
            ]
        }


deleteNote : Int -> b -> Cmd b
deleteNote ids onResponse =
    EffectsProxy.cmd
        { expect = Api.Data.expectWhatever onResponse
        , functionName = "deleteNote"
        , arguments = [ Json.int ids ]
        }


saveNote : Api.Book.Note -> b -> Cmd b
saveNote note onResponse =
    EffectsProxy.cmd
        { expect = Api.Data.expectWhatever onResponse
        , functionName = "updateNote"
        , arguments = [ Api.Book.noteEncoder note ]
        }


updateNotes : List Api.Book.Note -> Task.Task Http.Error ()
updateNotes notes =
    EffectsProxy.taskReturningWhatever
        { functionName = "updateNotes"
        , arguments = [ Json.list Api.Book.noteEncoder notes ]
        , timeout = Nothing
        }


listChapters : Api.Book.BookID -> (Data (List Api.Book.Chapter) -> msg) -> Cmd msg
listChapters (Api.Book.BookID id) onResponse =
    EffectsProxy.cmd
        { expect = Api.Data.expectJson onResponse (Json.Decode.list Api.Book.chapterDecoder)
        , functionName = "listChapters"
        , arguments = [ Json.int id ]
        }


createChapter : Api.Book.BookID -> String -> Int -> (Data Api.Book.Chapter -> msg) -> Cmd msg
createChapter (Api.Book.BookID bookId) chapterTitle position msg =
    EffectsProxy.cmd
        { expect = Api.Data.expectJson msg Api.Book.chapterDecoder
        , functionName = "createChapter"
        , arguments =
            [ Json.object
                [ ( "id", Json.int 0 )
                , ( "bookId", Json.int bookId )
                , ( "title", Json.string chapterTitle )
                , ( "position", Json.int position )
                ]
            ]
        }


deleteChapter : Int -> b -> Cmd b
deleteChapter ids onResponse =
    EffectsProxy.cmd
        { expect = Api.Data.expectWhatever onResponse
        , functionName = "deleteChapter"
        , arguments = [ Json.int ids ]
        }


saveChapter : Api.Book.Chapter -> msg -> Cmd msg
saveChapter chapter onResponse =
    EffectsProxy.cmd
        { expect = Api.Data.expectWhatever onResponse
        , functionName = "updateChapter"
        , arguments = [ Api.Book.chapterEncoder chapter ]
        }


updateChapters : a -> List Api.Book.Chapter -> Cmd a
updateChapters onResponse chapters =
    EffectsProxy.cmd
        { expect = Api.Data.expectWhatever onResponse
        , functionName = "updateChapters"
        , arguments = [ Json.list Api.Book.chapterEncoder chapters ]
        }
