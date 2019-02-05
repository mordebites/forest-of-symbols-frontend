module Main exposing (init, main, update)

import Browser
import Decoders exposing (itemDecoder, itemsDecoder, linkDecoder, linksDecoder)
import Encoders exposing (newItemEncoder, newLinkEncoder)
import Helpers exposing (getIdFromString, getItemTitleFromId)
import Http exposing (..)
import Models exposing (Item, Link, Model, Msg(..), missingId)
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
    ( Model (Item missingId "" "") [] (Link missingId "" missingId missingId) [] ""
    , Cmd.batch [ createItemGetRequest, createLinkGetRequest ]
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateItemTitle input ->
            ( { model | item = Item missingId input model.item.itemType }, Cmd.none )

        UpdateItemType input ->
            ( { model | item = Item missingId model.item.title input }, Cmd.none )

        UpdateLinkType input ->
            ( { model | link = Link missingId input model.link.source model.link.dest }, Cmd.none )

        UpdateLinkFrom input ->
            ( { model | link = Link missingId model.link.linkType (getIdFromString input) model.link.dest }, Cmd.none )

        UpdateLinkTo input ->
            ( { model | link = Link missingId model.link.linkType model.link.source (getIdFromString input) }, Cmd.none )

        CreateNewItem ->
            validateItem model

        CreateNewLink ->
            validateLink model

        ItemCreated result ->
            case result of
                Ok _ ->
                    ( Model (Item missingId "" "") model.items model.link model.links "", createItemGetRequest )

                Err _ ->
                    ( { model | error = "Item Creation issues" }, Cmd.none )

        LinkCreated result ->
            case result of
                Ok _ ->
                    ( Model model.item model.items (Link missingId "" missingId missingId) model.links "", createLinkGetRequest )

                Err _ ->
                    ( { model | error = "Link Creation issues" }, Cmd.none )

        UpdateItems result ->
            case result of
                Ok items ->
                    ( Model model.item items model.link model.links "", Cmd.none )

                Err _ ->
                    ( { model | error = "Item List Retrieval Issues" }, Cmd.none )

        UpdateLinks result ->
            case result of
                Ok links ->
                    ( Model model.item model.items model.link links "", Cmd.none )

                Err _ ->
                    ( { model | error = "Link List Retrieval Issues" }, Cmd.none )


createItemPostRequest : Item -> Cmd Msg
createItemPostRequest item =
    Http.post
        { url = "http://localhost:8080/items"
        , body = jsonBody (newItemEncoder item)
        , expect = Http.expectJson ItemCreated itemDecoder
        }


createLinkPostRequest : Link -> Cmd Msg
createLinkPostRequest link =
    Http.post
        { url = "http://localhost:8080/links"
        , body = jsonBody (newLinkEncoder link)
        , expect = Http.expectJson LinkCreated linkDecoder
        }


createItemGetRequest : Cmd Msg
createItemGetRequest =
    Http.get
        { url = "http://localhost:8080/items"
        , expect = Http.expectJson UpdateItems itemsDecoder
        }


createLinkGetRequest : Cmd Msg
createLinkGetRequest =
    Http.get
        { url = "http://localhost:8080/links"
        , expect = Http.expectJson UpdateLinks linksDecoder
        }


validateItem : Model -> ( Model, Cmd Msg )
validateItem model =
    if String.length model.item.title > 0 && String.length model.item.itemType > 0 then
        ( model, createItemPostRequest model.item )

    else
        ( { model | error = "Insert both item title and type" }, Cmd.none )


validateLink : Model -> ( Model, Cmd Msg )
validateLink model =
    if
        String.length model.link.linkType
            > 0
            && String.length (String.fromInt model.link.source)
            > 0
            && String.length (String.fromInt model.link.dest)
            > 0
    then
        ( model, createLinkPostRequest model.link )

    else
        ( { model | error = "Insert valid link type, source and destination" }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
