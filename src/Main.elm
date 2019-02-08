module Main exposing (init, main, update)

import Browser
import Graph exposing (Edge, Node, NodeContext)
import Helpers exposing (getIdFromString)
import HttpRequests exposing (createItemGetRequest, createLinkGetRequest)
import Models exposing (Entity, Item, Link, Model, Msg(..), missingId)
import Validators exposing (validateItem, validateLink)
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
    ( Model (Item missingId "" "") (Link missingId "" missingId missingId) "" Graph.empty
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
            ( { model | link = setTypeInLink newType model.link }, Cmd.none )

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
                    ( { model | graph = Graph.fromNodesAndEdges (convertItemsToNodes items) (Graph.edges model.graph) }, Cmd.none )

                Err _ ->
                    ( { model | error = "Item List Retrieval Issues" }, Cmd.none )

        UpdateLinks result ->
            case result of
                Ok links ->
                    ( { model | graph = Graph.fromNodesAndEdges (Graph.nodes model.graph) (convertLinksToEdges links) }, Cmd.none )

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
    { oldLink | linkType = newLinkType }


convertItemsToNodes : List Item -> List (Node Entity)
convertItemsToNodes items =
    List.map (\item -> Node item.id item) items


convertLinksToEdges : List Link -> List (Edge Link)
convertLinksToEdges links =
    List.map (\link -> Link link.source link.dest link) links



{--
initializeNode : NodeContext String () -> NodeContext Entity ()
initializeNode ctx =
    { node = { label = Force.entity ctx.node.id ctx.node.label, id = ctx.node.id }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }
--}
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
