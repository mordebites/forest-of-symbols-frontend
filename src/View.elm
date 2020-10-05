module View exposing (view)

import Color
import Force
import Graph exposing (Edge, Graph, Node, NodeId)
import Helpers exposing (getItemTitleFromId)
import Html exposing (Attribute, Html, div, input, li, ul)
import Html.Attributes exposing (placeholder, style, type_)
import Html.Events exposing (keyCode, on, onInput, onMouseDown, targetValue)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode exposing (andThen, fail, map2, succeed)
import Models exposing (Entity, Item, Model, Msg(..), State(..), elementId)
import TypedSvg exposing (circle, defs, g, line, marker, polygon, rect, svg, text_, title)
import TypedSvg.Attributes as Attrs exposing (class, cursor, fill, fontSize, id, markerEnd, markerHeight, markerWidth, orient, pointerEvents, points, refX, refY, stroke)
import TypedSvg.Attributes.InPx exposing (cx, cy, dx, dy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AlignmentBaseline(..), AnchorAlignment(..), Cursor(..), Length(..), Paint(..))
import Zoom


view : Model -> Html Msg
view model =
    let
        zoomEvents : List (Attribute Msg)
        zoomEvents =
            case model.state of
                Init _ ->
                    []

                Ready { zoom } ->
                    Zoom.events zoom ZoomMsg

        zoomTransformAttr : Attribute Msg
        zoomTransformAttr =
            case model.state of
                Init _ ->
                    class []

                Ready { zoom } ->
                    Zoom.transform zoom
    in
    div []
        [ div
            [ style "width" "80%"
            , style "height" "400px"
            , style "margin" "50px auto"
            , style "border" "2px solid rgba(0, 0, 0, 0.85)"
            ]
            [ svg
                [ id elementId
                , Attrs.width <| Percent 100
                , Attrs.height <| Percent 100
                ]
                [ defs [] [ arrowhead ]
                , -- This transparent rectangle is placed in the background as a
                  -- target for the zoom events. Note that the zoom transformation
                  -- are not applied to this rectangle, but to group that contains
                  -- the actual graph.
                  rect
                    ([ Attrs.width <| Percent 100
                     , Attrs.height <| Percent 100
                     , fill <| Paint <| Color.rgba 0 0 0 0
                     , cursor CursorMove
                     ]
                        ++ zoomEvents
                    )
                    []
                , g
                    [ zoomTransformAttr ]
                    [ renderGraph model ]
                ]
            ]
        , div [] [ text "Item Creation" ]
        , input [ type_ "text", placeholder "Item Title", Html.Attributes.value model.textBoxes.item.title, onInput UpdateItemTitle, onEnter CreateNewItem ] []
        , input [ type_ "text", placeholder "Item Type", Html.Attributes.value model.textBoxes.item.itemType, onInput UpdateItemType, onEnter CreateNewItem ] []
        , div [] [ text "Link Creation" ]
        , input [ type_ "text", placeholder "Link Type", Html.Attributes.value model.textBoxes.link.linkType, onInput UpdateLinkType, onEnter CreateNewLink ] []
        , input [ type_ "text", placeholder "Link Source", Html.Attributes.value (String.fromInt model.textBoxes.link.source), onInput UpdateLinkSource, onEnter CreateNewLink ] []
        , input [ type_ "text", placeholder "Link Destination", Html.Attributes.value (String.fromInt model.textBoxes.link.dest), onInput UpdateLinkDest, onEnter CreateNewLink ] []
        , div [] [ text model.textBoxes.error ]
        , div [] [ text "Items" ]
        , ul [] (List.map (\item -> li [] [ text ("Id: " ++ String.fromInt item.id ++ " | Title: " ++ item.title ++ " Type: " ++ item.itemType) ]) model.textBoxes.items)
        , div [] [ text "Links" ]
        , ul [] (List.map (\link -> li [] [ text ("Id: " ++ String.fromInt link.id ++ " | Type: " ++ link.linkType ++ " " ++ getItemTitleFromId model link.source ++ " -> " ++ getItemTitleFromId model link.dest) ]) model.textBoxes.links)
        ]


renderGraph : Model -> Svg Msg
renderGraph model =
    case model.state of
        Init _ ->
            text ""

        Ready { graph, showGraph } ->
            if showGraph then
                g
                    []
                    [ Graph.edges graph
                        |> List.map (linkElement graph)
                        |> g [ class [ "links" ] ]
                    , Graph.nodes graph
                        |> List.map nodeElement
                        |> g [ class [ "nodes" ] ]
                    ]

            else
                text ""


{-| Draws a single vertex (node).
-}
nodeElement : Node Entity -> Svg Msg
nodeElement node =
    g [ class [ "node" ] ]
        [ circle
            [ r 20
            , strokeWidth 3
            , fill (Paint Color.lightYellow)
            , stroke (Paint Color.black)
            , cursor CursorPointer

            -- The coordinates are initialized and updated by `Force.simulation`
            -- and `Force.tick`, respectively.
            , cx node.label.x
            , cy node.label.y

            -- Add event handler for starting a drag on the node.
            , onMouseDown node.id
            ]
            [ title [] [ text node.label.value ] ]
        , text_
            [ -- Align text label at the center of the circle.
              dx <| node.label.x
            , dy <| node.label.y
            , Attrs.alignmentBaseline AlignmentMiddle
            , Attrs.textAnchor AnchorMiddle

            -- styling
            , fontSize <| Px 8
            , fill (Paint Color.black)

            -- Setting pointer events to none allows the user to click on the
            -- element behind the text, so in this case the circle. If you
            -- position the text label outside of the circle, you also should
            -- do this, so that drag and zoom operations are not interrupted
            -- when the cursor is above the text.
            , pointerEvents "none"
            ]
            [ text node.label.value ]
        ]


{-| This is the event handler that handles clicks on the vertices (nodes).

The event catches the `clientPos`, which is a tuple with the
`MouseEvent.clientX` and `MouseEvent.clientY` values. These coordinates are
relative to the client area (browser viewport).

If the graph is positioned anywhere else than at the coordinates `(0, 0)`, the
svg element position must be subtracted when setting the node position. This is
handled in the update function by calling the `shiftPosition` function.

-}
onMouseDown : NodeId -> Attribute Msg
onMouseDown index =
    Mouse.onDown (.clientPos >> DragStart index)


{-| This function draws the lines between the vertices.
-}
linkElement : Graph Entity String -> Edge String -> Svg Msg
linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 "") <|
                Maybe.map (.node >> .label) <|
                    Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 "") <|
                Maybe.map (.node >> .label) <|
                    Graph.get edge.to graph
    in
    g [ class [ "link" ] ]
        [ line
            [ x1 source.x
            , y1 source.y
            , x2 target.x
            , y2 target.y
            , strokeWidth 5
            , stroke <| Paint <| Color.white
            ]
            []
        , line
            [ x1 source.x
            , y1 source.y
            , x2 target.x
            , y2 target.y
            , strokeWidth 1
            , stroke edgeColor
            , markerEnd "url(#arrowhead)"
            ]
            []
        ]


{-| This is the definition of the arrow head that is displayed at the end of
the edges.

It is a child of the svg `defs` element and can be referenced by its id with
`url(#arrowhead)`.

-}
arrowhead : Svg msg
arrowhead =
    marker
        [ id "arrowhead"
        , orient "auto"
        , markerWidth <| Px 8.0
        , markerHeight <| Px 6.0
        , refX "29"
        , refY "3"
        ]
        [ polygon
            [ points [ ( 0, 0 ), ( 8, 3 ), ( 0, 6 ) ]
            , fill edgeColor
            ]
            []
        ]


edgeColor : Paint
edgeColor =
    Paint <| Color.rgb255 180 180 180


onEnter : Msg -> Attribute Msg
onEnter tagger =
    let
        isEnter code =
            if code == 13 then
                succeed "Enter pressed"

            else
                fail "Not Enter"

        decode_Enter =
            andThen isEnter keyCode
    in
    on "keydown" (map2 (\_ _ -> tagger) decode_Enter targetValue)
