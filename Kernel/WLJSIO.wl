BeginPackage["JerryI`Misc`WLJS`Transport`", {"KirillBelov`WebSocketHandler`"}]; 

WLJSTransportHandler::usage = ""
WLJSTransportScript::usage = ""
WLJSAliveQ::usage = ""

WLXEmbed::usage = ""

WLJSTransportSend::usage = ""

Offload::usage = "Hold expression to be evaluated on a frontend"

Begin["`Private`"]

SetAttributes[Offload, HoldFirst]

WLJSTransportHandler[cl_, data_ByteArray] := Block[{Global`$Client = cl},
    ToExpression[data//ByteArrayToString];
]

WLJSTransportSend[expr_, client_] := WebSocketSend[client, expr // $DefaultSerializer]

$DefaultSerializer = ExportByteArray[#, "ExpressionJSON"]&

Global`WLJSIOAddTracking[symbol_] := With[{cli = Global`$Client, name = SymbolName[Unevaluated[symbol]]},
    WLJSTransportHandler["AddTracking"][symbol, name, cli, Function[{client, value},
        WebSocketSend[client, Global`WLJSIOUpdateSymbol[name, value] // $DefaultSerializer]
    ]]
]

SetAttributes[Global`WLJSIOAddTracking, HoldFirst]

Global`WLJSIOGetSymbol[uid_, params_][expr_] := With[{client = Global`$Client},
    WLJSTransportHandler["GetSymbol"][expr, client, Function[result,
        WebSocketSend[client, Global`WLJSIOPromiseResolve[uid, result] // $DefaultSerializer] 
    ]]
];

Global`WLJSIOPromise[uid_, params_][expr_] := With[{client = Global`$Client},
    Print["WLJS promise >> get with id "<>uid];
    WebSocketSend[client, Global`WLJSIOPromiseResolve[uid, expr] // $DefaultSerializer];
];

IDCards = <||>;
Global`WLJSIDCardRegister[uid_String] := (Print["Transport registered as "<>uid]; IDCards[uid] = Global`$Client)

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

WLJSTransportScript[OptionsPattern[]] := If[NumberQ[OptionValue["Port"]],
    Switch[{OptionValue["TwoKernels"], OptionValue["Secret"]},
        {False, Null},
        ScriptTemplate[OptionValue["Port"], "server.init({socket: socket})"]
    ,
        {True, Null},
        ScriptTemplate[OptionValue["Port"], "server.init({socket: socket, kernel: true})"]
    ,
        {False, _},
        ScriptTemplate[OptionValue["Port"], "server.init({socket: socket}); server.send('WLJSIDCardRegister[\""<>OptionValue["Secret"]<>"\"]')"]
    ,
        {True, _},
        ScriptTemplate[OptionValue["Port"], "server.init({socket: socket, kernel: true}); server.send('WLJSIDCardRegister[\""<>OptionValue["Secret"]<>"\"]')"]
    ]
,
    "Specify a mode and a port!"
]

Options[WLJSTransportScript] = {"Port"->Null, "Regime"->"Standalone", "Secret"->Null, "TwoKernels" -> False}

assets = $InputFileName // DirectoryName // ParentDirectory;

commonScript = StringRiffle[{
    Import[FileNameJoin[{assets, "Assets", "ServerAPI.js"}], "String"],
    Import[FileNameJoin[{assets, "Assets", "InterpreterExtension.js"}], "String"]
}, "\n"];

ScriptTemplate[port_, initCode_] := 
    StringTemplate["
        <script type=\"module\">
            ``
            var socket = new WebSocket(\"ws://\"+window.location.hostname+':'+``);
            window.server = new Server('Master Kernel');

            socket.onopen = function(e) {
              console.log(\"[open]\");
              
              ``;
            }; 

            socket.onmessage = function(event) {
              //create global context
              //callid
              const uid = Math.floor(Math.random() * 100);
              var global = {call: uid};
              interpretate(JSON.parse(event.data), {global: global});
            };

            socket.onclose = function(event) {
              console.log(event);
              alert('Connection lost. Please, update the page to see new changes.');
            }; 

            
        </script>
    "][commonScript, port, initCode]


End[];

EndPackage[];