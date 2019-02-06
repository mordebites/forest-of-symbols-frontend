module HttpRequests exposing (createItemGetRequest, createItemPostRequest, createLinkGetRequest, createLinkPostRequest)

import Decoders exposing (itemDecoder, itemsDecoder, linkDecoder, linksDecoder)
import Encoders exposing (newItemEncoder, newLinkEncoder)
import Http exposing (Body, Error, emptyBody, expectJson, jsonBody)
import Json.Decode as Decode
import Models exposing (Item, Link, Msg(..))


createRequest : String -> String -> Body -> (Result Error a -> Msg) -> Decode.Decoder a -> Cmd Msg
createRequest method url body msg decoder =
    Http.request
        { method = method
        , headers = []
        , url = url
        , body = body
        , expect = expectJson msg decoder
        , timeout = Nothing
        , tracker = Nothing
        }


createItemPostRequest : Item -> Cmd Msg
createItemPostRequest item =
    createRequest "POST" "http://localhost:8080/items" (jsonBody (newItemEncoder item)) ItemCreated itemDecoder


createLinkPostRequest : Link -> Cmd Msg
createLinkPostRequest link =
    createRequest "POST" "http://localhost:8080/links" (jsonBody (newLinkEncoder link)) LinkCreated linkDecoder


createItemGetRequest : Cmd Msg
createItemGetRequest =
    createRequest "GET" "http://localhost:8080/items" emptyBody UpdateItems itemsDecoder


createLinkGetRequest : Cmd Msg
createLinkGetRequest =
    createRequest "GET" "http://localhost:8080/links" emptyBody UpdateLinks linksDecoder