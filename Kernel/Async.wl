BeginPackage["JerryI`Misc`Async`", {"JerryI`Misc`Events`", "JerryI`Misc`Events`Promise`"}]; 

SetTimeout::usage = "SetTimeout[expr, milliseconds_Number] async scheldued task once after period"
SetInterval::usage = "SetInterval[expr, milliseconds_Number] async scheldued task every period"
CancelTimeout::usage = "CancelTimeout[task] cancel the timer"
CancelInterval::usage = "CancelInterval[task] cancel the timer"


AsyncFunction::usage = "AsyncFunction[args, body] is a pure (or \"anonymous\") async function. Returns Promise"
Await::usage = "Await[expr] is used in AsyncFunction to pause the execution until expr is resolved"

PauseAsync::usage = "Async version of Pause[n], that returns promise"


Looper;

Begin["`Private`"]; 


SetTimeout[expr_, timeout_] := obj = SessionSubmit[ScheduledTask[expr, {Quantity[timeout/1000, "Seconds"]}] ]
CancelTimeout[t_TaskObject] := TaskRemove[t]

SetInterval[expr_, timeout_] := obj = SessionSubmit[ScheduledTask[expr, Quantity[timeout/1000, "Seconds"]]]
CancelInterval[t_TaskObject] := TaskRemove[t]

SetAttributes[SetTimeout, HoldFirst]
SetAttributes[SetInterval, HoldFirst]


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
      ], Function[null0,
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
            ], Function[null0,
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
            ], Function[null0,
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
      ], Function[null0,
        EventFire[p5, Reject, $Failed];
      ] ];
      
      asyncReturn[p5]
    ]
  ,
    Set[a, res]
  ]
]

asyncTransform[CompoundExpression[a_, b__]] := With[{first = asyncTransform[a]},
  If[MatchQ[first, _asyncReturn],
    Module[{p = Promise[]},
      Then[Extract[first, 1], Function[null0,
        With[{rest = asyncTransform[CompoundExpression[b] ]},
          If[MatchQ[rest, _asyncReturn],
            Then[Extract[rest, 1], Function[result,
              EventFire[p, Resolve, result];
            ], Function[null1,
              EventFire[p, Reject, $Failed];
            ] ]
          ,
            EventFire[p, Resolve, rest];
          ];
        ];
      ], Function[null2,
            EventFire[p, Reject, $Failed];
      ] ];

      asyncReturn[p]
    ]
  ,
    asyncTransform[CompoundExpression[b] ]
  ]
]

AsyncFunction[vars_, body_] := Function[vars, 
  With[{return = asyncTransform[body]},
    If[MatchQ[return, _asyncReturn],
      Module[{mainPromise = Promise[]},
        Then[Extract[return, 1], Function[result,
          EventFire[mainPromise, Resolve, result];
        ], Function[null0,
          EventFire[mainPromise, Reject, $Failed];
        ] ];
        
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


(* LEGACY *)

Looper[] := While[True,
    If[!Looper`Flag, Pause[0.01] ];
    If[Keys[Looper`Tasks] === {}, Looper`Flag = False,
        With[{task = Looper`Tasks[#]},
            task["Expr"]; 
            If[!task["Continuous"], Looper`Tasks[#] = ., Pause[0.01] ]; 
        ] &/@ Keys[Looper`Tasks];
    ];
]

Looper`Submit[expr_, OptionsPattern[] ] := With[{uid = CreateUUID[] },
    Looper`Tasks[ uid ] = <|"Expr" :> expr, "Continuous" -> OptionValue["Continuous"]|>;
    Looper`Flag = True;
    Looper`Task[uid]
]

Looper`Task /: Delete[Looper`Task[uid_String] ] := Looper`Tasks[uid] = .;

SetAttributes[Looper`Submit, HoldFirst]

Options[Looper`Submit] = {"Continuous" -> False}

Looper`Tasks = <||>
Looper`Flag = True

End[];

EndPackage[];

