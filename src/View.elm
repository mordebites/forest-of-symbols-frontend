module View exposing (view)

import Color
import Force
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (keyCode, on, onInput, targetValue)
import Json.Decode exposing (andThen, fail, map2, succeed)
import Models exposing (Item, Model, Msg(..), missingId)
import TypedSvg exposing (circle, g, line, svg, text_)
import TypedSvg.Attributes exposing (class, fill, stroke, textAnchor, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, fontSize, r, strokeWidth, x, x1, x2, y, y1, y2)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..))


w : Float
w =
    990


h : Float
h =
    504


view : Model -> Html Msg
view model =
    let
        msgs =
            [ Graph.edges model.graph |> List.map (linkElement model.graph) |> g [ class [ "links" ] ]
            , Graph.nodes model.graph |> List.map nodeElement |> g [ class [ "nodes" ] ]
            , Graph.nodes model.graph |> List.map nodeText |> g [ class [ "texts" ] ]
            ]
    in
    div
        []
        [ div [] [ text "Item Creation" ]
        , input [ type_ "text", placeholder "Item Title", Html.Attributes.value model.item.title, onInput UpdateItemTitle, onEnter CreateNewItem ] []
        , input [ type_ "text", placeholder "Item Type", Html.Attributes.value model.item.itemType, onInput UpdateItemType, onEnter CreateNewItem ] []
        , div [] [ text "Link Creation" ]
        , input [ type_ "text", placeholder "Link Type", Html.Attributes.value model.link.linkType, onInput UpdateLinkType, onEnter CreateNewLink ] []
        , input [ type_ "text", placeholder "Link Source", Html.Attributes.value (String.fromInt model.link.source), onInput UpdateLinkSource, onEnter CreateNewLink ] []
        , input [ type_ "text", placeholder "Link Destination", Html.Attributes.value (String.fromInt model.link.dest), onInput UpdateLinkDest, onEnter CreateNewLink ] []
        , div [] [ text model.error ]
        , svg [ viewBox 0 0 w h ] msgs
        ]


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


linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 (Item missingId "" "")) <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 ((Item missingId "" ""))) <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
    line
        [ strokeWidth 1
        , stroke (Color.rgb255 170 170 170)
        , x1 source.x
        , y1 source.y
        , x2 target.x
        , y2 target.y
        ]
        []


nodeElement node =
    circle
        [ r 5
        , fill (Fill Color.black)
        , stroke (Color.rgba 0 0 0 0)
        , strokeWidth 7
        , cx node.label.x
        , cy node.label.y
        ]
        [ text node.value.title ]


nodeText node =
    text_
        [ x node.label.x
        , y (node.label.y + 20)
        , fontSize 10
        , textAnchor AnchorMiddle
        ]
        [ text node.label.title ]
