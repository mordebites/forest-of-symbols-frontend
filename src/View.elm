module View exposing (view)

import Html exposing (Attribute, Html, div, input, li, text, ul)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (keyCode, on, onInput, targetValue)
import Json.Decode exposing (andThen, fail, map2, succeed)
import Models exposing (Item, Model, Msg(..))


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Item Creation" ]
        , input [ type_ "text", placeholder "Item Title", Html.Attributes.value model.item.title, onInput UpdateItemTitle, onEnter CreateNewItem ] []
        , input [ type_ "text", placeholder "Item Type", Html.Attributes.value model.item.itemType, onInput UpdateItemType, onEnter CreateNewItem ] []
        , div [] [ text "Link Creation" ]
        , input [ type_ "text", placeholder "Link Type", Html.Attributes.value model.link.linkType, onInput UpdateLinkType, onEnter CreateNewLink ] []
        , input [ type_ "text", placeholder "Link Source", Html.Attributes.value (String.fromInt model.link.source), onInput UpdateLinkFrom, onEnter CreateNewLink ] []
        , input [ type_ "text", placeholder "Link Destination", Html.Attributes.value (String.fromInt model.link.dest), onInput UpdateLinkTo, onEnter CreateNewLink ] []
        , div [] [ text model.error ]
        , div [] [ text "Items" ]
        , ul [] (List.map (\item -> li [] [ text ("Id: " ++ (String.fromInt item.id) ++ " | Title: " ++ item.title ++ " Type: " ++ item.itemType) ]) model.items)
        , div [] [ text "Links" ]
        , ul [] (List.map (\link -> li [] [ text ("Id: " ++ (String.fromInt link.id) ++ " | Type: " ++ link.linkType ++ " ") ]) model.links)
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
