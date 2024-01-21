BeginPackage["JerryI`Misc`Events`"]; 

(* 
    A kernel event system package 
    following KISS principle 

   can be patterns as well and delayed as well


*)


EventObject::usage = "a representation of a simple event. can hold an extra information"

EventJoin::usage = "join sequence of many EventObjects to a new one"
EventClone::usage = "dublicate an event object keeping all handlers"

EventRemove::usage = "remove the bond from EventObject"

EventFire::usage = "manually fire an event object"

EventBind::usage = "legacy method to bind events"

EventsRack::usage = "depricated!"

EmittedEvent::usage = "internal function called by the frontend to fire an event on a kernel"
EventHandlers::usage = "internal function, which hold the binded function"

EventListener::usage = "internal command"

EventPacket::usage = "just handy wrapper"

Begin["`Private`"]; 

Unprotect[EventHandler]
ClearAll[EventHandler]

(* old alias *)
EventBind[any_, handler_Function] := EventHandler[any, handler]

EventHandler[EventObject[a_Association], f_] := With[{},
    EventHandler[a["id"], f];
    EventObject[a]
]

EventHandler[a_String, f_Function] := With[{},
    EventHandler[a, {_String -> f}];
    a
]

EventHandler[a_String, f_List] := With[{},
    If[!AssociationQ[EventHandlers[a] ], EventHandlers[a] = <||>];
    EventHandlers[a] = Join[EventHandlers[a], Association[f] ];
    a 
]

EventRemove[a_String, part_] := (EventHandlers[a] = Join[EventHandlers[a], <|part -> Null|>]);
EventRemove[a_String] := (EventHandlers[a] = .)

EventRemove[EventObject[a_Association] ] := EventRemove[ a["id"] ]
EventRemove[EventObject[a_Association], t_] := EventRemove[ a["id"], t ]

EventObject /: Delete[EventObject[a_Association], opts___] := EventRemove[EventObject[a], opts]
EventObject /: DeleteObject[EventObject[a_Association], opts___] := EventRemove[EventObject[a], opts]

EventFire[EventObject[a_Association] ] := With[{uid = a["id"]}, 
    If[KeyExistsQ[a, "Initial"],
        EventFire[ uid, a["Initial"] ]
    ,
        EventFire[ uid, Null ]
    ];

    EventObject[a]
]

EventFire[EventObject[a_Association], part_, data_] := With[{uid = a["id"]}, 
    If[KeyExistsQ[a, "Initial"],
        EventFire[ uid, part, a["Initial"] ]
    ,
        EventFire[ uid, part, Null ]
    ];

    EventObject[a]
]

EventFire[uid_String, part_, data_] := EventFire[EventHandlers[uid], part, data]

EventFire[assoc_Association, part_, data_] := With[{replacements = assoc},
    (part /. Normal[replacements])[data]
]

EventFire[router_EventRouter, part_, data_] := With[{},
    EventFire[#, part, data] &/@ router[[1]]
]

EventFire[uid_String, data_] := EventFire[uid, "!_-_!", data]

EventRouter /: Append[EventRouter[data_List], uid_String] := EventRouter[Join[data, {uid}]];

EventClone[assocId_String] := (
    With[{t = EventHandlers[assocId], id = assocId, cuid = CreateUUID[]}, 
        Switch[Head[t],
            EventRouter,

            (*Print["Events >> adding new event to an existing chain"];*)
            t = Append[t, cuid];
        ,
            EventHandlers,

            (*Print["Events >> making a router from an empty event object"];*)
            t = EventRouter[{cuid}];
        ,
            Association,

            (*Print["Events >> reroute existing handlers"];*)
            With[{nid = CreateUUID[]},
                EventHandlers[nid] = t;
                EventHandlers[assocId] = EventRouter[{nid, cuid}];
            ];
        ,
            _,
            Print[StringTemplate["Events >> Internal error! Head `` is not valid"][Head[t] ] ];
            Return[$Failed];
        ];

        EventObject[<|"id" -> cuid|>]
    ]
)

EventClone[EventObject[assoc_]] := EventObject[Join[assoc, EventClone[assoc["id"] ][[1]] ] ]

EventJoin[seq__] := With[{list = List[seq], joined = CreateUUID[]},
Module[{handler, data = <||>},
    With[{},
        Switch[Head[#],
            String,
            Null;
        ,   
            EventObject,
            If[KeyExistsQ[#[[1]], "Initial"], 
                (* check if types convertion is needed *)
                (* associations will be merged together *)
                (* the rest will be stored as id=value pair *)
                If[!AssociationQ[#[[1]]["Initial"]], data[#[[1]]["id"]] = #[[1]]["Initial"], data = Join[data, #[[1]]["Initial"]]];
            ];
        ];
        
        With[{cloned = EventClone[#]},
            EventHandler[cloned, Function[d,
                handler[cloned[[1]]["id"], d];
            ]];
        ];
    ]&/@list;

    handler = Function[{id, d},
        If[AssociationQ[d], data = Join[data, d], data = Join[data, <|id -> d|>] ];
        EventFire[joined, data]
    ];

    EventObject[<|"id" -> joined, "Initial" -> data, "storage" -> Hold[data], "handler" -> Hold[handler]|>]
] ] 

EventObject /: Join[evs__EventObject] := EventJoin[evs]


End[];

EndPackage[];

