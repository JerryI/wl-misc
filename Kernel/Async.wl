BeginPackage["JerryI`Misc`Async`"]; 

SetTimeout::usage = "SetTimeout[expr, milliseconds_Number] async scheldued task once after period"
SetInterval::usage = "SetInterval[expr, milliseconds_Number] async scheldued task every period"
CancelTimeout::usage = "CancelTimeout[task] cancel the timer"
CancelInterval::usage = ""

Looper::usage = ""

Begin["`Private`"]; 


Looper[] := While[True,
    If[!Looper`Flag, Pause[0.1] ];
    If[Keys[Looper`Tasks] === {}, Looper`Flag = False,
        With[{task = Looper`Tasks[#]},
            task["Expr"]; 
            If[!task["Continuous"], Looper`Tasks[#] = ., Pause[0.1] ]; 
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

SetTimeout[expr_, timeout_] := obj = SessionSubmit[ScheduledTask[expr, {Quantity[timeout/1000, "Seconds"]}]]
CancelTimeout[t_TaskObject] := TaskRemove[t]

SetInterval[expr_, timeout_] := obj = SessionSubmit[ScheduledTask[expr, Quantity[timeout/1000, "Seconds"]]]
CancelInterval[t_TaskObject] := TaskRemove[t]

SetAttributes[SetTimeout, HoldFirst]
SetAttributes[SetInterval, HoldFirst]

End[];

EndPackage[];

