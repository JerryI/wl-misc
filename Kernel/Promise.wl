BeginPackage["JerryI`Misc`Events`Promise`", {"JerryI`Misc`Events`"}]; 

Promise::usage = "Create an generic promise object"
PromiseQ::usage = "Check if it a promise object"

Reject::usage = ""
Resolve::usage = ""

Then::usage = ""

Begin["`Private`"]


resolved = <||>;
earlyBird[uid_, resolveqq_][data_] := (resolved[uid] = <|"Data"->data, "Type"->resolveqq|>);
ResolvedQ[Promise[uid_] ] := KeyExistsQ[resolved, uid]



Promise /: WaitAll[ Promise[uid_], Timeout_:15 ] := Module[{timeout = 500 Timeout / 15.0},
    (*Echo[">> Waiting for promise to be resolved ... "];*)
    While[!KeyExistsQ[resolved, uid] && timeout > 0,
        timeout--;
        Pause[0.03];
    ];
    If[timeout > 0,
        resolved[uid]["Data"]
    ,
        Echo["Promise >> Timeout!"];
        $Failed
    ]
] 

Promise[] := With[{uid = CreateUUID[]}, 
    EventHandler[uid, {
        Resolve -> earlyBird[uid, True],
        Reject  -> earlyBird[uid, False]
    }];

    Promise[uid] 
] 
Promise /: EventHandler[Promise[uid_], any_ ] := EventHandler[uid, any]

Promise /: EventFire[Promise[uid_], Resolve, data_] := With[{},
    EventFire[uid, Resolve, data];
    EventRemove[uid];
]

Promise /: EventFire[Promise[uid_], Reject, data_] := With[{},
    EventFire[uid, Reject, data];
    EventRemove[uid];
]

Then[any_, resolve_, reject_] := resolve[any]
Then[any_, resolve_] := Then[any, resolve, Null]

Then[p_Promise, resolve_] := Then[p, resolve, Null]
Then[p_Promise, resolve_, reject_] := With[{},
 
    If[!ResolvedQ[p],
        EventHandler[p, {
            Resolve -> resolve,
            Reject  -> reject
        }]
    ,
       
        With[{result = resolved[p // First], u = p // First},
            resolved[u] = .;
            If[result["Type"],
                resolve[result["Data"] ]
            ,
                reject[ result["Data"] ]
            ]
            
        ]
    ]
]

Then[list_List, resolve_] := Then[list, resolve, Null]

Then[list_List, resolve_, reject_] := Module[{results = ConstantArray[Null, Length[list] ], fired = 0, check},
    check := With[{},
        If[fired == Length[list],
            resolve[results];
        ];
    ];

    MapIndexed[With[{index = #2[[1]], promise = #1},
        Then[promise, Function[data,
            fired++;
            results[[index]] = data;

            check;
        ], reject];
    ] &,  list];
]

PromiseQ[_] = False
PromiseQ[_Promise] = True

End[]
EndPackage[]
