module Models exposing (Drag, Element, Entity, Item, Link, Model, Msg(..), ReadyState, State(..), elementId, missingId, mkEmptyTextBoxes)

import Browser.Dom as Dom
import Force
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Http
import Time
import Zoom exposing (OnZoom, Zoom)


missingId =
    -1


elementId : String
elementId =
    "exercise-graph"


type alias Model =
    { textBoxes : TextBoxes
    , state : State
    }


type State
    = Init (Graph Entity String)
    | Ready ReadyState


type alias ReadyState =
    { drag : Maybe Drag
    , graph : Graph Entity String
    , simulation : Force.State NodeId
    , zoom : Zoom

    -- The position and dimensions of the svg element.
    , element : Element

    -- If you immediately show the graph when moving from `Init` to `Ready`,
    -- you will briefly see the nodes in the upper left corner before the first
    -- simulation tick positions them in the center. To avoid this sudden jump,
    -- `showGraph` is initialized with `False` and set to `True` with the first
    -- `Tick`.
    , showGraph : Bool
    }


type alias TextBoxes =
    { item : Item
    , items : List Item
    , link : Link
    , links : List Link
    , error : String
    }


mkEmptyTextBoxes : TextBoxes
mkEmptyTextBoxes =
    { item = Item missingId "" ""
    , items = []
    , link = Link missingId "" missingId missingId
    , links = []
    , error = ""
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
    | DragAt ( Float, Float )
    | DragEnd ( Float, Float )
    | DragStart NodeId ( Float, Float )
    | ReceiveElementPosition (Result Dom.Error Dom.Element)
    | Resize Int Int
    | Tick Time.Posix
    | ZoomMsg OnZoom


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


type alias Drag =
    { current : ( Float, Float )
    , index : NodeId
    , start : ( Float, Float )
    }


type alias Element =
    { height : Float
    , width : Float
    , x : Float
    , y : Float
    }


type alias Entity =
    Force.Entity NodeId { value : String }
