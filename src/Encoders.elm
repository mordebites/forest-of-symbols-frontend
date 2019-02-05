module Encoders exposing (newItemEncoder, newLinkEncoder)

import Json.Encode exposing (Value, int, object, string)
import Models exposing (Item, Link)


newItemEncoder : Item -> Value
newItemEncoder item =
    object
        [ ( "title", string item.title )
        , ( "type", string item.itemType )
        ]


newLinkEncoder : Link -> Value
newLinkEncoder link =
    object
        [ ( "type", string link.linkType )
        , ( "source", int link.source )
        , ( "dest", int link.dest )
        ]
