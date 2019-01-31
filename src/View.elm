module View exposing (view)

import Html exposing (Attribute, Html, div, input, li, text, ul)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (keyCode, on, onInput, targetValue)
import Json.Decode exposing (andThen, fail, map2, succeed)
import Models exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Title", Html.Attributes.value model.item.title, onInput UpdateTitle, onEnter CreateNewItem ] []
        , input [ type_ "text", placeholder "Type", Html.Attributes.value model.item.itemType, onInput UpdateType, onEnter CreateNewItem ] []
        , div [] [ text model.error ]
        , ul [] (List.map (\item -> li [] [ text ("Title: " ++ item.title ++ " Type: " ++ item.itemType) ]) model.items)
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
