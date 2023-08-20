BeginPackage["JerryI`Misc`Language`"]; 

LeakyModule::usage = "Module, that leaks on purpose"

Begin["`Private`"]; 

Garbage = {}

ExtractFirst[x_, y___] := FakeHold[x];
SetAttributes[ExtractFirst, HoldFirst];
SetAttributes[FakeHold, HoldFirst];

LeakyModule[vars_, expr_, OptionsPattern[]] := With[{garbage = OptionValue[Automatic, Automatic, "Garbage", Unevaluated]},
    Module[vars, CompoundExpression[
        AppendTo[garbage, (Hold[vars] /. {Set :> ExtractFirst} // ReleaseHold) /. {FakeHold -> Hold}],
        expr
    ]]
]

SetAttributes[LeakyModule, HoldAll]
Options[LeakyModule] = {"Garbage" :> Garbage}

End[];

EndPackage[];
