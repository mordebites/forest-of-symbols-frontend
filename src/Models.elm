module Models exposing (Item, Msg(..), Model)

import Http


type alias Model =
    { item : Item
    , items : List Item
    , error : String
    }


type Msg
    = UpdateTitle String
    | UpdateType String
    | CreateNewItem
    | ItemCreated (Result Http.Error Item)
    | UpdateItems (Result Http.Error (List Item))


type alias Item =
    { title : String
    , itemType : String
    }
