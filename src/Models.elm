module Models exposing (Item, Link, Model, Msg(..), missingId, Entity)

import Force
import Graph exposing (Graph, NodeId)
import Http

missingId =
    -1


type alias Model =
    { item : Item
    , link : Link
    , error : String
    , graph : Graph Entity Link
    }


type Msg
    = UpdateItemTitle String
    | UpdateItemType String
    | UpdateLinkType String
    | UpdateLinkSource String
    | UpdateLinkDest String
    | CreateNewItem
    | CreateNewLink
    | ItemCreated (Result Http.Error Item)
    | LinkCreated (Result Http.Error Link)
    | UpdateItems (Result Http.Error (List Item))
    | UpdateLinks (Result Http.Error (List Link))


type alias Item =
    { id : Int
    , title : String
    , itemType : String
    }


type alias Link =
    { id : Int
    , linkType : String
    , source : Int
    , dest : Int
    }

type alias Entity =
    Force.Entity NodeId Item