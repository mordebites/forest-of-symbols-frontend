module Decoders exposing (itemDecoder, itemsDecoder)

import Json.Decode exposing (Decoder, list, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Models exposing (Item)


itemDecoder : Decoder Item
itemDecoder =
    succeed Item
        |> required "title" string
        |> required "type" string


itemsDecoder : Decoder (List Item)
itemsDecoder =
    list itemDecoder
