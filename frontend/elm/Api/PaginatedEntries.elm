module Api.PaginatedEntries exposing
    ( PaginatedEntry
    , paginatedDecoder
    , singlePagePaginatedDecoder
    )

import Json.Decode as Json
import Json.Decode.Pipeline exposing (required)


type alias PaginatedEntry a =
    { entries : List a
    , pageNumber : Int
    , pageSize : Int
    , totalEntries : Int
    , totalPages : Int
    }



paginatedDecoder : Json.Decoder a -> Json.Decoder (PaginatedEntry a)
paginatedDecoder entryDecoder =
    Json.succeed PaginatedEntry
        |> required "data" (Json.list entryDecoder)
        |> required "page_number" Json.int
        |> required "page_size" Json.int
        |> required "total_entries" Json.int
        |> required "total_pages" Json.int