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
        UpdateItemTitle newTitle ->
            ( { model | item = Item missingId newTitle model.item.itemType }, Cmd.none )

        UpdateItemType newType ->
            ( { model | item = Item missingId model.item.title newType }, Cmd.none )

        UpdateLinkType newType ->
            ( { model | link = setTypeInLink newType model.link}, Cmd.none )

        UpdateLinkSource newSource ->
            ( { model | link = setSourceItemInLink (getIdFromString newSource) model.link }, Cmd.none )

        UpdateLinkDest newDest ->
            ( { model | link = setDestItemInLink (getIdFromString newDest) model.link }, Cmd.none )

        CreateNewItem ->
            validateItem model

        CreateNewLink ->
            validateLink model

        ItemCreated result ->
            case result of
                Ok _ ->
                    ( { model | item = Item missingId "" "" }, createItemGetRequest )

                Err _ ->
                    ( { model | error = "Item Creation issues" }, Cmd.none )

        LinkCreated result ->
            case result of
                Ok _ ->
                    ( { model | link = Link missingId "" missingId missingId }, createLinkGetRequest )

                Err _ ->
                    ( { model | error = "Link Creation issues" }, Cmd.none )

        UpdateItems result ->
            case result of
                Ok items ->
                    ( { model | items = items }, Cmd.none )

                Err _ ->
                    ( { model | error = "Item List Retrieval Issues" }, Cmd.none )

        UpdateLinks result ->
            case result of
                Ok links ->
                    ( { model | links = links }, Cmd.none )

                Err _ ->
                    ( { model | error = "Link List Retrieval Issues" }, Cmd.none )


setSourceItemInLink : Int -> Link -> Link
setSourceItemInLink newId oldLink =
    { oldLink | source = newId }


setDestItemInLink : Int -> Link -> Link
setDestItemInLink newId oldLink =
    { oldLink | dest = newId }

setTypeInLink : String -> Link -> Link
setTypeInLink newLinkType oldLink =
    {oldLink | linkType = newLinkType }

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
