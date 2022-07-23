module Api.Book exposing (..)

import Iso8601
import Json.Decode as Json exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode
import Time


type BookID
    = BookID Int


type NoteID
    = NoteID Int


type ChapterID
    = ChapterID Int


type alias Book =
    { id : BookID
    , author : String
    , title : String
    , imgUrl : String
    , about : String
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    }


type alias Note =
    { id : NoteID
    , bookId : BookID
    , chapterId : Maybe ChapterID
    , title : String
    , blocks : List Block
    , createdAt : Time.Posix
    , updatedAt : Time.Posix

    -- , chapters : List String
    }


type alias Chapter =
    { id : ChapterID
    , title : String
    , chapterType : String
    , position : Int
    , bookId : BookID
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    }


type Block
    = BlockQuote String
    | Text String
    | Heading String


blockIsEmpty : Block -> Bool
blockIsEmpty block =
    case block of
        BlockQuote "" ->
            True

        BlockQuote _ ->
            False

        Text "" ->
            True

        Text _ ->
            False

        Heading "" ->
            True

        Heading _ ->
            False


splitBlock : Block -> List Block
splitBlock block =
    case block of
        BlockQuote quote ->
            String.split "\n" quote
                |> List.map BlockQuote

        Text text ->
            String.split "\n" text
                |> List.map Text

        Heading text ->
            String.split "\n" text
                |> List.indexedMap
                    (\i str ->
                        if i == 0 then
                            Heading str

                        else
                            Text str
                    )


allChapters : List { title : String }
allChapters =
    [ { title = "I Should Have Been a Statistic"
      }
    , { title = "Truth Hurts"
      }
    , { title = "The Impossible Task"
      }
    , { title = "Taking Souls"
      }
    ]


decoder : Json.Decoder Book
decoder =
    Json.succeed Book
        |> required "id" (Json.map BookID Json.int)
        |> required "author" Json.string
        |> required "title" Json.string
        |> required "imgUrl" Json.string
        -- |> hardcoded ""
        |> required "about" Json.string
        |> required "created_at" Iso8601.decoder
        |> required "updated_at" Iso8601.decoder



-- , Json.succeed (\a b c d -> Image { id = a, createdAt = b, imageId = c, desc = d, base64Image = Nothing })
--     |> required "id" Json.int
--     |> required "createdAt" Iso8601.decoder
--     |> required "imageId" Json.int
--     |> required "desc" Json.string


encoder : Book -> Json.Value
encoder book =
    let
        (BookID id) =
            book.id
    in
    Json.Encode.object
        [ ( "id", Json.Encode.int id )
        , ( "title", Json.Encode.string book.title )
        , ( "author", Json.Encode.string book.author )
        , ( "imgUrl", Json.Encode.string book.imgUrl )
        , ( "about", Json.Encode.string book.about )
        ]


noteDecoder : Json.Decoder Note
noteDecoder =
    Json.succeed Note
        |> required "id" (Json.map NoteID Json.int)
        |> required "bookId" (Json.map BookID Json.int)
        |> optional "chapterId" (Json.map (ChapterID >> Just) Json.int) Nothing
        |> required "title" Json.string
        |> required "blocks"
            (Json.map
                (Json.decodeString
                    (Json.list
                        (Json.oneOf
                            [ Json.field "type" Json.string
                                |> Json.andThen blockDecoder
                            ]
                        )
                    )
                    >> Result.toMaybe
                    >> Maybe.withDefault []
                )
                Json.string
            )
        |> required "created_at" Iso8601.decoder
        |> required "updated_at" Iso8601.decoder


noteEncoder : Note -> Json.Value
noteEncoder note =
    let
        (NoteID id) =
            note.id

        (BookID bookID) =
            note.bookId
    in
    Json.Encode.object
        [ ( "id", Json.Encode.int id )
        , ( "bookId", Json.Encode.int bookID )
        , ( "chapterId"
          , note.chapterId
                |> Maybe.map (\(ChapterID chapterId) -> Json.Encode.int chapterId)
                |> Maybe.withDefault Json.Encode.null
          )
        , ( "title", Json.Encode.string note.title )
        , ( "blocks", valueToString (Json.Encode.list blockEncoder note.blocks) )
        ]


chapterDecoder : Json.Decoder Chapter
chapterDecoder =
    Json.succeed Chapter
        |> required "id" (Json.map ChapterID Json.int)
        |> required "title" Json.string
        |> required "chapterType" Json.string
        |> required "position" Json.int
        |> required "bookId" (Json.map BookID Json.int)
        |> required "created_at" Iso8601.decoder
        |> required "updated_at" Iso8601.decoder


chapterEncoder : Chapter -> Json.Encode.Value
chapterEncoder chapter =
    let
        (ChapterID id) =
            chapter.id

        (BookID bookID) =
            chapter.bookId
    in
    Json.Encode.object
        [ ( "id", Json.Encode.int <| id )
        , ( "title", Json.Encode.string <| chapter.title )
        , ( "chapterType", Json.Encode.string <| chapter.chapterType )
        , ( "position", Json.Encode.int <| chapter.position )
        , ( "bookId", Json.Encode.int <| bookID )
        ]


valueToString : Json.Encode.Value -> Json.Encode.Value
valueToString =
    Json.Encode.encode 0 >> Json.Encode.string


blockDecoder : String -> Decoder Block
blockDecoder version =
    case version of
        "quote" ->
            Json.succeed BlockQuote
                |> required "quote" Json.string

        "text" ->
            Json.succeed Text
                |> required "text" Json.string

        "heading" ->
            Json.succeed Heading
                |> required "heading" Json.string

        _ ->
            Json.fail <|
                "Trying to decode info, but type "
                    ++ version
                    ++ " is not supported."


blockEncoder : Block -> Json.Encode.Value
blockEncoder version =
    case version of
        BlockQuote quote ->
            Json.Encode.object
                [ ( "quote", Json.Encode.string quote )
                , ( "type", Json.Encode.string "quote" )
                ]

        Text text ->
            Json.Encode.object
                [ ( "text", Json.Encode.string text )
                , ( "type", Json.Encode.string "text" )
                ]

        Heading text ->
            Json.Encode.object
                [ ( "heading", Json.Encode.string text )
                , ( "type", Json.Encode.string "heading" )
                ]
