module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onInput, targetValue)
import Http exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { item : Item
    , items : List Item
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Item "" "") [], Cmd.none )


type alias Item =
    { title : String
    , itemType : String
    }



-- UPDATE


type Msg
    = UpdateTitle String
    | UpdateType String
    | CreateNewItem
    | UpdateItems (Result Http.Error Item)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTitle input ->
            ( { model | item = Item input model.item.itemType }, Cmd.none )

        UpdateType input ->
            ( { model | item = Item model.item.title input }, Cmd.none )

        CreateNewItem ->
            ( { model | item = Item "" "" }, createPostRequest model.item )

        UpdateItems result ->
            case result of
                Ok item ->
                    ( { model | items = item :: model.items }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Title", Html.Attributes.value model.item.title, onInput UpdateTitle, onEnter CreateNewItem ] []
        , input [ type_ "text", placeholder "Type", Html.Attributes.value model.item.itemType, onInput UpdateType, onEnter CreateNewItem ] []
        , ul [] (List.map (\item -> li [] [ text ("Title: " ++ item.title ++ " Type: " ++ item.itemType) ]) model.items)
        ]



-- HELPER


createPostRequest : Item -> Cmd Msg
createPostRequest item =
    Http.post
        { url = "http://localhost:8080/items"
        , body = jsonBody (newItemEncoder item)
        , expect = Http.expectJson UpdateItems itemDecoder
        }


--createGetRequest : Cmd Msg
--createGetRequest =
--    Http.get
--        { url = "http://localhost:8080/items"
--        , expect = Http.expectJson UpdateItems itemDecoder
--        }

newItemEncoder : Item -> Json.Encode.Value
newItemEncoder item =
    Json.Encode.object
        [ ( "title", Json.Encode.string item.title )
        , ( "type", Json.Encode.string item.itemType )
        ]


itemDecoder : Decoder Item
itemDecoder =
    succeed Item
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "type" Json.Decode.string

--itemListDecoder : Decoder List Item
--itemListDecoder =
--    succeed List Item
--        |> Json.Decode.Pipeline.required "title" Json.Decode.string
--        |> Json.Decode.Pipeline.required "type" Json.Decode.string


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
