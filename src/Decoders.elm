module Decoders exposing (itemDecoder, itemsDecoder, linkDecoder, linksDecoder)

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, dict, int, list, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Models exposing (Item, Link)


itemDecoder : Decoder Item
itemDecoder =
    succeed Item
        |> required "id" int
        |> required "title" string
        |> required "type" string


linkDecoder : Decoder Link
linkDecoder =
    succeed Link
        |> required "id" int
        |> required "type" string
        |> required "source" int
        |> required "dest" int


itemsDecoder : Decoder (List Item)
itemsDecoder =
    list itemDecoder

linksDecoder : Decoder (List Link)
linksDecoder =
    list linkDecoder
