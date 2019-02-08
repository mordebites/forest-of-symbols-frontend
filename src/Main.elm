module Main exposing (init, main, update)

import Browser
import Browser.Events
import Force exposing (Force)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Helpers exposing (getIdFromString)
import Html.Events.Extra.Mouse as Mouse
import HttpRequests exposing (createItemGetRequest, createLinkGetRequest)
import Models exposing (Entity, Item, Link, Model, Msg(..), missingId)
import TypedSvg.Core exposing (Attribute)
import Validators exposing (validateItem, validateLink)
import View exposing (h, view, w)



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
    let
        link { from, to } =
            ( from, to )

        forces =
            [ Force.links <| List.map link <| Graph.edges Graph.empty
            , Force.manyBody <| List.map .id <| Graph.nodes Graph.empty
            , Force.center (w / 2) (h / 2)
            ]
    in
    ( Model (Item missingId "" "") (Link missingId "" missingId missingId) "" Graph.empty (Force.simulation forces)
    , Cmd.batch [ createItemGetRequest, createLinkGetRequest ]
    )


setForces : Graph Entity Link -> List (Force Int)
setForces graph =
    let
        link { from, to } =
            ( from, to )
    in
    [ Force.links <| List.map link <| Graph.edges graph
    , Force.manyBody <| List.map .id <| Graph.nodes graph
    , Force.center (w / 2) (h / 2)
    ]



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
                    ( { model
                        | graph = Graph.fromNodesAndEdges (convertItemsToNodes items) (Graph.edges model.graph)
                        , simulation = Force.simulation (setForces model.graph)
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = "Item List Retrieval Issues" }, Cmd.none )

        UpdateLinks result ->
            case result of
                Ok links ->
                    ( { model
                        | graph = Graph.fromNodesAndEdges (Graph.nodes model.graph) (convertLinksToEdges links)
                        , simulation = Force.simulation (setForces model.graph)
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = "Link List Retrieval Issues" }, Cmd.none )

        Tick _ ->
            let
                ( newState, list ) =
                    Force.tick model.simulation <| List.map .label <| Graph.nodes model.graph
            in
            ( { model | graph = (updateGraphWithList model.graph list), simulation = newState }, Cmd.none )


updateContextWithValue : NodeContext Entity Link -> Entity -> NodeContext Entity Link
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


updateGraphWithList : Graph Entity Link -> List Entity -> Graph Entity Link
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


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
    List.map (\item -> { id = item.id, label = Force.entity item.id item }) items


convertLinksToEdges : List Link -> List (Edge Link)
convertLinksToEdges links =
    List.map (\link -> { from = link.source, to = link.dest, label = link }) links



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    -- This allows us to save resources, as if the simulation is done, there is no point in subscribing
    -- to the rAF.
    if Force.isCompleted model.simulation then
        Sub.none

    else
        Browser.Events.onAnimationFrame Tick
