module Main exposing (init, main, update)

import Browser
import Decoders exposing (itemDecoder, itemsDecoder)
import Encoders exposing (newItemEncoder)
import Http exposing (..)
import Models exposing (Item, Model, Msg(..))
import View exposing (view)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Item "" "") [] ""
    , createGetRequest
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTitle input ->
            ( { model | item = Item input model.item.itemType }, Cmd.none )

        UpdateType input ->
            ( { model | item = Item model.item.title input }, Cmd.none )

        CreateNewItem ->
            ( Model (Item "" "") model.items "", createPostRequest model.item )

        ItemCreated result ->
            case result of
                Ok _ ->
                    ( model, createGetRequest )

                Err _ ->
                    ( { model | error = "Item Creation issues" }, Cmd.none )

        UpdateItems result ->
            case result of
                Ok list ->
                    ( Model model.item list "", Cmd.none )

                Err _ ->
                    ( { model | error = "Item List Retrieval Issues" }, Cmd.none )


createPostRequest : Item -> Cmd Msg
createPostRequest item =
    Http.post
        { url = "http://localhost:8080/items"
        , body = jsonBody (newItemEncoder item)
        , expect = Http.expectJson ItemCreated itemDecoder
        }


createGetRequest : Cmd Msg
createGetRequest =
    Http.get
        { url = "http://localhost:8080/items"
        , expect = Http.expectJson UpdateItems itemsDecoder
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
