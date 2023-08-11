BeginPackage["JerryI`Misc`WLJS`Transport`", {"KirillBelov`WebSocketHandler`"}]; 

WLJSTransportHandler::usage = ""
WLJSTransportScript::usage = ""

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

WLJSTransportScript[port_, template_] := If[
    NumberQ[Global`Port],
    ScriptTemplate[Global`Port, Global`Regime],
    ScriptTemplate[port, template]
]

WLJSTransportScript[] := If[
    NumberQ[Global`Port],
    ScriptTemplate[Global`Port, Global`Regime],
    "Specify a mode and a port!"
]

ScriptTemplate[port_, "Standalone"] := StringTemplate["
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

End[];

EndPackage[];