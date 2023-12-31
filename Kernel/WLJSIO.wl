BeginPackage["JerryI`Misc`WLJS`Transport`", {"KirillBelov`WebSocketHandler`"}]; 

WLJSTransportHandler::usage = ""
WLJSTransportScript::usage = ""
WLJSAliveQ::usage = ""

Begin["`Private`"]

WLJSTransportHandler[cl_, data_ByteArray] := Block[{Global`client = cl},
    ToExpression[data//ByteArrayToString];
]

$DefaultSerializer = ExportByteArray[#, "ExpressionJSON"]&

Global`NotebookAddTracking[symbol_] := With[{cli = Global`client, name = SymbolName[Unevaluated[symbol]]},
    WLJSTransportHandler["AddTracking"][symbol, name, cli, Function[{client, value},
        WebSocketSend[client, Global`FrontUpdateSymbol[name, value] // $DefaultSerializer]
    ]]
]

SetAttributes[Global`NotebookAddTracking, HoldFirst]

Global`NotebookGetSymbol[uid_, params_][expr_] := With[{client = Global`client},
    WLJSTransportHandler["GetSymbol"][expr, client, Function[result,
        WebSocketSend[client, Global`PromiseResolve[uid, result] // $DefaultSerializer] 
    ]]
];

IDCards = <||>;
Global`WLJSIDCardRegister[uid_String] := (Print["Transport registered as "<>uid]; IDCards[uid] = Global`client)

WLJSAliveQ[uid_String] := (
    If[KeyExistsQ[IDCards, uid],
        With[{res = !FailureQ[WebSocketSend[IDCards[uid], Global`SlientPing // $DefaultSerializer]]},
            If[!res, IDCards[uid] = .];
            res
        ]
    ,
        Missing[]
    ]
)

WLJSTransportScript[OptionsPattern[]] := If[
    NumberQ[OptionValue["Port"]],
    ScriptTemplate[OptionValue["Port"], OptionValue["Regime"], If[OptionValue["Secret"] // StringQ, OptionValue["Secret"], Null]],
    "Specify a mode and a port!"
]

Options[WLJSTransportScript] = {"Port"->Null, "Regime"->"Standalone", "Secret"->Null}

ScriptTemplate[port_, "Standalone", secret_] := 
If[secret =!= Null,
    Print["Secret script: "<>secret];
    StringTemplate["
        <script type=\"module\">
            var socket = new WebSocket(\"ws://\"+window.location.hostname+':'+``);
            socket.onopen = function(e) {
              console.log(\"[open]\");
              server.kernel.socket = socket;
              server.init(socket);
              socket.send('WLJSIDCardRegister[\"``\"]');
            }; 

            socket.onmessage = function(event) {
              //create global context
              //callid
              const uid = Date.now() + Math.floor(Math.random() * 100);
              var global = {call: uid};
              interpretate(JSON.parse(event.data), {global: global});
            };

            socket.onclose = function(event) {
              console.log(event);
              alert('Connection lost. Please, update the page to see new changes.');
            }; 

            core.SlientPing = () => {
                console.log('Ppspsp... server is there');
            }
        </script>
    "][port, secret]
,

    StringTemplate["
        <script type=\"module\">
            var socket = new WebSocket(\"ws://\"+window.location.hostname+':'+``);
            socket.onopen = function(e) {
              console.log(\"[open]\");
              server.kernel.socket = socket;
              server.init(socket);
            }; 

            socket.onmessage = function(event) {
              //create global context
              //callid
              const uid = Date.now() + Math.floor(Math.random() * 100);
              var global = {call: uid};
              interpretate(JSON.parse(event.data), {global: global});
            };

            socket.onclose = function(event) {
              console.log(event);
              alert('Connection lost. Please, update the page to see new changes.');
            }; 
        </script>
    "][port]
]

End[];

EndPackage[];