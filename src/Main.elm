module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput, onSubmit, targetValue)
import Json.Decode as Json exposing (andThen, fail, map2, succeed)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { input : String
    , item : List String
    }


init : Model
init =
    Model "" []



-- UPDATE


type Msg
    = Write String
    | AddItem String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Write input ->
            { model | input = input }

        AddItem input ->
            Model "" (input :: model.item)


-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Item", value model.input, onInput Write, onEnter AddItem ] []
        , ul [] (List.map (\foo -> li [] [ text foo ]) model.item)
        ]


onEnter : (String -> msg) -> Attribute msg
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
    on "keydown" (map2 (\key value -> tagger value) decode_Enter targetValue)