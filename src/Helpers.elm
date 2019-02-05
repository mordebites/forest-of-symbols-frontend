module Helpers exposing (getItemTitleFromId, getIdFromString)

import List exposing (filter, head)
import Models exposing (Item, Model, missingId)


getItemTitleFromId : Model -> Int -> String
getItemTitleFromId model itemId =
    let
        isSameId : Item -> Bool
        isSameId item =
            item.id == itemId
    in
    case head (filter isSameId model.items) of
        Just item ->
            String.fromInt item.id

        Nothing ->
            ""


getIdFromString : String -> Int
getIdFromString idString =
    case String.toInt idString of
        Just idInt ->
            idInt

        Nothing ->
            missingId
