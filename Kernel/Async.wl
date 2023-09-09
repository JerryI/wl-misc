BeginPackage["JerryI`Misc`Async`"]; 

SetTimeout::usage = "SetTimeout[expr, milliseconds_Number] async scheldued task once after period"
SetInterval::usage = "SetInterval[expr, milliseconds_Number] async scheldued task every period"
CancelTimeout::usage = "CancelTimeout[task] cancel the timer"

Begin["`Private`"]; 

SetTimeout[expr_, timeout_] := obj = SessionSubmit[ScheduledTask[expr, {Quantity[timeout/1000, "Seconds"]}]]
CancelTimeout[t_TaskObject] := TaskRemove[t]

SetInterval[expr_, timeout_] := obj = SessionSubmit[ScheduledTask[expr, Quantity[timeout/1000, "Seconds"]]]
CancelInterval[t_TaskObject] := TaskRemove[t]

SetAttributes[SetTimeout, HoldFirst]
SetAttributes[SetInterval, HoldFirst]

End[];

EndPackage[];

