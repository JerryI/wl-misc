BeginPackage["JerryI`Misc`Events`Promise`", {"JerryI`Misc`Events`"}]; 

Promise::usage = "Promise[] create promise. Use p = Promise[] with EventFire[p, Resolve, data] or EventFire[p, Reject, data]"
PromiseQ::usage = "PromiseQ[expr] check if expression is a promise"

Reject::usage = "Reject a symbol, that represents promise rejection"
Resolve;

Then::usage = "Then[_Promise | expr, resolve] or  Then[_Promise | expr, resolve, reject] assigns resolve and reject handler functions to expr or Promise"

AsyncFunction::usage = "AsyncFunction[args, body] is a pure (or \"anonymous\") async function. Returns Promise"
Await::usage = "Await[expr] is used in AsyncFunction to pause the execution until expr is resolved"

PauseAsync::usage = "Async version of Pause[n], that returns promise"

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

asyncTransform;
SetAttributes[asyncTransform, HoldFirst]

asyncReturn;

asyncTransform[a_] := a

asyncTransform[CompoundExpression[b_]] := asyncTransform[b]

asyncTransform[Await[a_]] := asyncReturn[a]

asyncTransform[Module[vars_, body_]] := Module[vars, asyncTransform[body]]
asyncTransform[With[vars_, body_]] := With[vars, asyncTransform[body]]

asyncTransform[If[cond_, a_]] := asyncTransform[If[cond, a, Null]]

asyncTransform[If[cond_, a_, b_]] := With[{condition = asyncTransform[cond]},
  If[MatchQ[condition, _asyncReturn],
    Module[{cp = Promise[]},
      Then[Extract[cp, 1], Function[result,
        EventFire[cp, Resolve, asyncTransform[If[result, a, b]]];
      ], Function[Null,
        EventFire[cp, Reject, $Failed];
      ]];

      asyncReturn[cp]
    ]
  ,
    If[TrueQ[condition],
      With[{ares = asyncTransform[a]},
        If[MatchQ[ares, _asyncReturn],
          Module[{cap = Promise[]},
          
            Then[Extract[ares, 1], Function[result,
              EventFire[cap, Resolve, result];
            ], Function[Null,
              EventFire[cap, Reject, $Failed];
            ]];
            
            asyncReturn[cap]
          ]
        ,
          ares
        ]
      ]
    ,
      With[{bres = asyncTransform[b]},
        If[MatchQ[bres, _asyncReturn],
          Module[{cbp = Promise[]},
          
            Then[Extract[bres, 1], Function[result,
              EventFire[cbp, Resolve, result];
            ], Function[Null,
              EventFire[cbp, Reject, $Failed];
            ]];
            
            asyncReturn[cbp]
          ]
        ,
          bres
        ]
      ]    
    ]
  ]
]

asyncTransform[Set[a_, b_]] := With[{res = asyncTransform[b]},
  If[MatchQ[res, _asyncReturn],
    Module[{p5 = Promise[]},
      Then[Extract[res, 1], Function[resolved,
        EventFire[p5, Resolve, Set[a, resolved] ];
      ], Function[Null,
        EventFire[p5, Reject, $Failed];
      ]];
      
      asyncReturn[p5]
    ]
  ,
    Set[a, res]
  ]
]

asyncTransform[CompoundExpression[a_, b__]] := With[{first = asyncTransform[a]},
  If[MatchQ[first, _asyncReturn],
    Module[{p = Promise[]},
      Then[Extract[first, 1], Function[Null,
        With[{rest = asyncTransform[CompoundExpression[b]]},
          If[MatchQ[rest, _asyncReturn],
            Then[Extract[rest, 1], Function[result,
              EventFire[p, Resolve, result];
            ], Function[Null,
              EventFire[p, Reject, $Failed];
            ]]
          ,
            EventFire[p, Resolve, rest];
          ];
        ];
      ], Function[Null,
            EventFire[p, Reject, $Failed];
      ]];

      asyncReturn[p]
    ]
  ,
    asyncTransform[CompoundExpression[b]]
  ]
]

AsyncFunction[vars_, body_] := Function[vars, 
  With[{return = asyncTransform[body]},
    If[MatchQ[return, _asyncReturn],
      Module[{mainPromise = Promise[]},
        Then[Extract[return, 1], Function[result,
          EventFire[mainPromise, Resolve, result];
        ], Function[Null,
          EventFire[mainPromise, Reject, $Failed];
        ]];
        
        mainPromise
      ]
    ,
      return
    ]
  ]
]

SetAttributes[AsyncFunction, HoldAll]

PauseAsync[n_Real | n_Integer] := With[{p = Promise[]}, 
    SessionSubmit[ScheduledTask[EventFire[p, Resolve, True];, {Quantity[n, "Seconds"]}] ];
    p
]

End[]
EndPackage[]
