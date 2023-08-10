BeginPackage["JerryI`Misc`WLJS`Transport`", {"KirillBelov`WebSocketHandler`"}]; 

WLJSIOHandler::usage = ""
WLJSIOConnect::usage = ""

Begin["`Private`"]

WLJSIOHandler[client_, data_ByteArray] := Block[{Global`client = cl},
    ToExpression[data//ByteArrayToString];
]

$DefaultSerializer = ExportByteArray[#, "ExpressionJSON"]&

NotebookAddTracking[symbol_] := With[{cli = Global`client, name = SymbolName[Unevaluated[symbol]]},
    WLJSIOHandler["AddTracking"][symbol, name, cli, Function[{client, value},
        WebSocketSend[client, Global`FrontUpdateSymbol[name, value] // $DefaultSerializer]
    ]]
]

SetAttributes[NotebookAddTracking, HoldFirst]

NotebookGetSymbol[uid_, params_][expr_] := With[{client = Global`client},
    WLJSIOHandler["GetSymbol"][expr, client, Function[result,
        WebSocketSend[client, Global`PromiseResolve[uid, result] // $DefaultSerializer] 
    ]]
];

WLJSIOConnect[port_, template_] := If[
    StringQ[Global`Mode],
    ScriptTemplate[Global`Port, Global`Mode],
    ScriptTemplate[port, template]
]

ScriptTemplate[port_, "Standalone"] = StringTemplate["
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