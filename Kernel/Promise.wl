BeginPackage["JerryI`Misc`Events`Promise`", {"JerryI`Misc`Events`"}]; 

Promise::usage = "Create an generic promise object"
PromiseQ::usage = "Check if it a promise object"

Reject::usage = ""
Resolve::usage = ""

Then::usage = ""

Begin["`Private`"]

resolved = <||>;
earlyBird[uid_][data_] := (resolved[uid] = data);
ResolvedQ[Promise[uid_] ] := KeyExistsQ[resolved, uid]

Promise /: WaitAll[ Promise[uid_] ] := Module[{timeout = 500},
    Echo[">> Waiting for promise to be resolved ... "];
    While[!KeyExistsQ[resolved, uid] && timeout > 0,
        timeout--;
        Pause[0.01];
    ];
    If[timeout > 0,
        resolved[uid]
    ,
        Echo[">> Promise >> Timeout!"];
        $Failed
    ]
] 

Promise[] := With[{uid = CreateUUID[]}, 
    EventHandler[uid, {
        Resolve -> earlyBird[uid]
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

Then[_, resolve_, reject_] := resolve[Null]
Then[_, resolve_] := Then[Null, resolve, Null]

Then[p_Promise, resolve_] := Then[p, resolve, Null]
Then[p_Promise, resolve_, reject_] := With[{},
    Echo["Subscribe!"];
    If[!ResolvedQ[p],
        EventHandler[p, {
            Resolve -> resolve,
            Reject  -> reject
        }]
    ,
        Echo["Promise >> already resolved!"];
        With[{result = resolved[p // First], u = p // First},
            resolved[u] = .;
            resolve[result]
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