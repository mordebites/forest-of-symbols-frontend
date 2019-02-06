module Validators exposing (validateItem, validateLink)

import HttpRequests exposing (createItemPostRequest, createLinkPostRequest)
import Models exposing (Model, Msg)


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
