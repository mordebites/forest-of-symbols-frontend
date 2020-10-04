module Main exposing (init, main, update)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Decoders exposing (itemDecoder, itemsDecoder, linkDecoder, linksDecoder)
import Encoders exposing (newItemEncoder, newLinkEncoder)
import Force
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Helpers exposing (getIdFromString)
import Html.Events.Extra.Mouse as Mouse
import Http exposing (..)
import IntDict
import Json.Decode as Decode
import List exposing (map)
import Models exposing (Drag, Element, Entity, Item, Link, Model, Msg(..), ReadyState, State(..), elementId, missingId, mkEmptyTextBoxes)
import Task
import View exposing (view)
import Zoom exposing (Zoom)



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
    ( Model mkEmptyTextBoxes (Init Graph.empty)
    , Cmd.batch [ createItemGetRequest, createLinkGetRequest, getElementPosition ]
    )


initNode : NodeContext String String -> NodeContext Entity String
initNode ctx =
    { node =
        { label = Force.entity ctx.node.id ctx.node.label
        , id = ctx.node.id
        }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }


getGraphFromItemAndLinkLists : List Item -> List Link -> Graph String String
getGraphFromItemAndLinkLists items links =
    let
        nodes =
            map itemToNode items

        edges =
            map linkToEdge links
    in
    Graph.fromNodesAndEdges nodes edges


itemToNode : Item -> Node String
itemToNode item =
    Node item.id item.title


linkToEdge : Link -> Edge String
linkToEdge link =
    Edge link.source link.dest link.linkType


itemToNodeContext : Item -> NodeContext Entity String
itemToNodeContext item =
    { node =
        { label = Force.entity item.id item.title
        , id = item.id
        }
    , incoming = IntDict.empty
    , outgoing = IntDict.empty
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        oldTextBoxes =
            model.textBoxes
    in
    case ( msg, model ) of
        ( UpdateItemTitle newTitle, _ ) ->
            ( Model { oldTextBoxes | item = Item missingId newTitle oldTextBoxes.item.itemType } model.state, Cmd.none )

        ( UpdateItemType newType, _ ) ->
            ( Model { oldTextBoxes | item = Item missingId oldTextBoxes.item.title newType } model.state, Cmd.none )

        ( UpdateLinkType newType, _ ) ->
            ( Model { oldTextBoxes | link = setTypeInLink newType oldTextBoxes.link } model.state, Cmd.none )

        ( UpdateLinkSource newSource, _ ) ->
            ( Model { oldTextBoxes | link = setSourceItemInLink (getIdFromString newSource) oldTextBoxes.link } model.state, Cmd.none )

        ( UpdateLinkDest newDest, _ ) ->
            ( Model { oldTextBoxes | link = setDestItemInLink (getIdFromString newDest) oldTextBoxes.link } model.state, Cmd.none )

        ( CreateNewItem, _ ) ->
            validateItem model

        ( CreateNewLink, _ ) ->
            validateLink model

        ( ItemCreated result, _ ) ->
            case result of
                Ok item ->
                    let
                        newItems =
                            model.textBoxes.items ++ [ item ]
                    in
                    case model.state of
                        Init _ ->
                            ( Model { oldTextBoxes | items = newItems } model.state, getElementPosition )

                        Ready state ->
                            let
                                nodeContext =
                                    itemToNodeContext item

                                newGraph =
                                    Graph.insert nodeContext state.graph
                            in
                            ( Model
                                { oldTextBoxes | item = Item missingId "" "", items = newItems }
                                (Ready { state | graph = newGraph })
                            , getElementPosition
                            )

                Err _ ->
                    ( Model { oldTextBoxes | error = "Item Creation issues" } model.state, Cmd.none )

        ( LinkCreated result, _ ) ->
            case result of
                Ok link ->
                    let
                        newLinks =
                            model.textBoxes.links ++ [ link ]
                    in
                    case model.state of
                        Init _ ->
                            ( Model { oldTextBoxes | links = newLinks } model.state, getElementPosition )

                        Ready state ->
                            let
                                destNodeLink : Maybe (NodeContext Entity String) -> Maybe (NodeContext Entity String)
                                destNodeLink =
                                    Maybe.map (\ctx -> { ctx | incoming = IntDict.insert link.dest link.linkType ctx.incoming })

                                sourceNodeLink : Maybe (NodeContext Entity String) -> Maybe (NodeContext Entity String)
                                sourceNodeLink =
                                    Maybe.map (\ctx -> { ctx | outgoing = IntDict.insert link.source link.linkType ctx.outgoing })

                                newGraph =
                                    state.graph
                                        |> Graph.update link.source destNodeLink
                                        |> Graph.update link.dest sourceNodeLink
                            in
                            ( Model
                                { oldTextBoxes | item = Item missingId "" "", links = newLinks }
                                (Ready { state | graph = newGraph })
                            , getElementPosition
                            )

                Err _ ->
                    ( Model { oldTextBoxes | error = "Link Creation issues" } model.state, Cmd.none )

        ( UpdateItems result, _ ) ->
            case result of
                Ok items ->
                    case model.state of
                        Init _ ->
                            ( Model { oldTextBoxes | items = items } model.state, getElementPosition )

                        Ready state ->
                            let
                                newGraph =
                                    Graph.mapContexts initNode (getGraphFromItemAndLinkLists items model.textBoxes.links)
                            in
                            ( Model { oldTextBoxes | items = items } (Ready { state | graph = newGraph }), getElementPosition )

                Err _ ->
                    ( Model { oldTextBoxes | error = "Item List Retrieval Issues" } model.state, Cmd.none )

        ( UpdateLinks result, _ ) ->
            case result of
                Ok links ->
                    case model.state of
                        Init _ ->
                            ( Model { oldTextBoxes | links = links } model.state, getElementPosition )

                        Ready state ->
                            let
                                newGraph =
                                    Graph.mapContexts initNode (getGraphFromItemAndLinkLists model.textBoxes.items links)
                            in
                            ( Model { oldTextBoxes | links = links } (Ready { state | graph = newGraph }), getElementPosition )

                Err _ ->
                    ( Model { oldTextBoxes | error = "Link List Retrieval Issues" } model.state, Cmd.none )

        ( ReceiveElementPosition (Ok { element }), _ ) ->
            case model.state of
                -- When we get the svg element position and dimensions, we are
                -- ready to initialize the simulation and the zoom, but we cannot
                -- show the graph yet. If we did, we would see a noticeable jump.
                Init graph ->
                    ( Model
                        model.textBoxes
                        (Ready
                            { drag = Nothing
                            , element = element
                            , graph = graph
                            , showGraph = False
                            , simulation = initSimulation graph element.width element.height
                            , zoom = initZoom element
                            }
                        )
                    , Cmd.none
                    )

                Ready state ->
                    ( Model
                        model.textBoxes
                        (Ready
                            { drag = state.drag
                            , element = element
                            , graph = state.graph
                            , showGraph = True
                            , simulation = initSimulation state.graph element.width element.height
                            , zoom = state.zoom
                            }
                        )
                    , Cmd.none
                    )

        ( ReceiveElementPosition (Err _), _ ) ->
            ( model, Cmd.none )

        ( ZoomMsg zoomMsg, _ ) ->
            case model.state of
                Init _ ->
                    ( model, Cmd.none )

                Ready state ->
                    ( Model
                        model.textBoxes
                        (Ready { state | zoom = Zoom.update zoomMsg state.zoom })
                    , Cmd.none
                    )

        ( DragAt xy, _ ) ->
            case model.state of
                Init _ ->
                    ( model, Cmd.none )

                Ready state ->
                    ( Model model.textBoxes (Ready (handleDragAt xy state)), Cmd.none )

        ( DragEnd xy, _ ) ->
            case model.state of
                Init _ ->
                    ( model, Cmd.none )

                Ready state ->
                    case state.drag of
                        Just { index } ->
                            ( Model model.textBoxes
                                (Ready
                                    { state
                                        | drag = Nothing
                                        , graph = updateNodePosition index xy state
                                    }
                                )
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

        ( DragStart index xy, _ ) ->
            case model.state of
                Init _ ->
                    ( model, Cmd.none )

                Ready state ->
                    ( Model
                        model.textBoxes
                        (Ready { state | drag = Just { start = xy, current = xy, index = index } })
                    , Cmd.none
                    )

        ( Resize _ _, _ ) ->
            ( model, getElementPosition )

        ( Tick _, _ ) ->
            case model.state of
                Init _ ->
                    ( model, Cmd.none )

                Ready state ->
                    ( Model model.textBoxes (Ready (handleTick state)), Cmd.none )


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


{-| This function returns a command to retrieve the position of the svg element.
-}
getElementPosition : Cmd Msg
getElementPosition =
    Task.attempt ReceiveElementPosition (Dom.getElement elementId)


handleDragAt : ( Float, Float ) -> ReadyState -> ReadyState
handleDragAt xy ({ drag, simulation } as state) =
    case drag of
        Just { start, index } ->
            { state
                | drag =
                    Just
                        { start = start
                        , current = xy
                        , index = index
                        }
                , graph = updateNodePosition index xy state
                , simulation = Force.reheat simulation
            }

        Nothing ->
            state


handleTick : ReadyState -> ReadyState
handleTick readyState =
    let
        ( newSimulation, list ) =
            Force.tick readyState.simulation <|
                List.map .label <|
                    Graph.nodes readyState.graph
    in
    case readyState.drag of
        Nothing ->
            { readyState
                | graph = updateGraphWithList readyState.graph list
                , showGraph = True
                , simulation = newSimulation
            }

        Just { current, index } ->
            { readyState
                | graph =
                    Graph.update index
                        (Maybe.map
                            (updateNode
                                (shiftPosition
                                    readyState.zoom
                                    ( readyState.element.x
                                    , readyState.element.y
                                    )
                                    current
                                )
                            )
                        )
                        (updateGraphWithList readyState.graph list)
                , showGraph = True
                , simulation = newSimulation
            }


initSimulation : Graph Entity String -> Float -> Float -> Force.State NodeId
initSimulation graph width height =
    let
        link : { c | from : a, to : b } -> ( a, b )
        link { from, to } =
            ( from, to )
    in
    Force.simulation
        [ Force.xForceUniformDefaultForce (width / 2) <| List.map .id <| Graph.nodes graph
        , Force.yForceUniformDefaultForce (height / 2) <| List.map .id <| Graph.nodes graph

        -- Defines the force that pulls connected nodes together. You can use
        -- `Force.customLinks` if you need to adjust the distance and
        -- strength.
        , Force.links <| List.map link <| Graph.edges graph

        -- Defines the force that pushes the nodes apart. The default strength
        -- is `-30`, but since we are drawing fairly large circles for each
        -- node, we need to increase the repulsion by decreasing the strength to
        -- `-200`.
        , Force.manyBodyStrength -250 <| List.map .id <| Graph.nodes graph
        ]


{-| Initializes the zoom and sets a minimum and maximum zoom level.

You can also use `Zoom.translateExtent` to restrict the area in which the user
may drag, but since the graph is larger than the viewport and the exact
dimensions depend on the data and the final layout, you would either need to use
some kind of heuristic for the final dimensions here, or you would have to let
the simulation play out (or use `Force.computeSimulate` to calculate it at
once), find the min and max x and y positions of the graph nodes and use those
values to set the translate extent.

-}
initZoom : Element -> Zoom
initZoom element =
    Zoom.init { width = element.width, height = element.height }
        |> Zoom.scaleExtent 0.1 2


setSourceItemInLink : Int -> Link -> Link
setSourceItemInLink newId oldLink =
    { oldLink | source = newId }


setDestItemInLink : Int -> Link -> Link
setDestItemInLink newId oldLink =
    { oldLink | dest = newId }


setTypeInLink : String -> Link -> Link
setTypeInLink newLinkType oldLink =
    { oldLink | linkType = newLinkType }


shiftPosition : Zoom -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
shiftPosition zoom ( elementX, elementY ) ( clientX, clientY ) =
    let
        zoomRecord =
            Zoom.asRecord zoom
    in
    ( (clientX - zoomRecord.translate.x - elementX) / zoomRecord.scale
    , (clientY - zoomRecord.translate.y - elementY) / zoomRecord.scale
    )


updateContextWithValue :
    NodeContext Entity String
    -> Entity
    -> NodeContext Entity String
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


updateGraphWithList : Graph Entity String -> List Entity -> Graph Entity String
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


updateNode :
    ( Float, Float )
    -> NodeContext Entity String
    -> NodeContext Entity String
updateNode ( x, y ) nodeCtx =
    let
        nodeValue =
            nodeCtx.node.label
    in
    updateContextWithValue nodeCtx { nodeValue | x = x, y = y }


updateNodePosition : NodeId -> ( Float, Float ) -> ReadyState -> Graph Entity String
updateNodePosition index xy state =
    Graph.update
        index
        (Maybe.map
            (updateNode
                (shiftPosition
                    state.zoom
                    ( state.element.x, state.element.y )
                    xy
                )
            )
        )
        state.graph


validateItem : Model -> ( Model, Cmd Msg )
validateItem model =
    let
        oldTextBoxes =
            model.textBoxes
    in
    if String.length oldTextBoxes.item.title > 0 && String.length oldTextBoxes.item.itemType > 0 then
        ( model, createItemPostRequest oldTextBoxes.item )

    else
        ( Model { oldTextBoxes | error = "Insert both item title and type" } model.state, Cmd.none )


validateLink : Model -> ( Model, Cmd Msg )
validateLink model =
    let
        oldTextBoxes =
            model.textBoxes
    in
    if
        String.length oldTextBoxes.link.linkType
            > 0
            && String.length (String.fromInt oldTextBoxes.link.source)
            > 0
            && String.length (String.fromInt oldTextBoxes.link.dest)
            > 0
    then
        ( model, createLinkPostRequest oldTextBoxes.link )

    else
        ( Model { oldTextBoxes | error = "Insert valid link type, source and destination" } model.state, Cmd.none )



-- Subscriptions


{-| We have three groups of subscriptions:

1.  Mouse events, to handle mouse interaction with the nodes.
2.  A subscription on the animation frame, to trigger simulation ticks.
3.  Browser resizes, to update the zoom state and the position of the nodes
    when the size and position of the svg viewport change.

We want to make sure that we only subscribe to mouse events while there is
a mouse interaction in progress, and that we only subscribe to
`Browser.Events.onAnimationFrame` while the simulation is in progress.

-}
subscriptions : Model -> Sub Msg
subscriptions model =
    let
        dragSubscriptions : Sub Msg
        dragSubscriptions =
            Sub.batch
                [ Events.onMouseMove
                    (Decode.map (.clientPos >> DragAt) Mouse.eventDecoder)
                , Events.onMouseUp
                    (Decode.map (.clientPos >> DragEnd) Mouse.eventDecoder)
                , Events.onAnimationFrame Tick
                ]

        readySubscriptions : ReadyState -> Sub Msg
        readySubscriptions { drag, simulation, zoom } =
            Sub.batch
                [ Zoom.subscriptions zoom ZoomMsg
                , case drag of
                    Nothing ->
                        if Force.isCompleted simulation then
                            Sub.none

                        else
                            Events.onAnimationFrame Tick

                    Just _ ->
                        dragSubscriptions
                ]
    in
    Sub.batch
        [ case model.state of
            Init _ ->
                Sub.none

            Ready state ->
                readySubscriptions state
        , Events.onResize Resize
        ]
