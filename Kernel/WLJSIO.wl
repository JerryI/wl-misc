BeginPackage["JerryI`Misc`WLJS`Transport`", {"KirillBelov`WebSocketHandler`"}]; 

WLJSTransportHandler::usage = ""
WLJSTransportScript::usage = ""
WLJSAliveQ::usage = ""

WLJSTransportSend::usage = ""

Offload::usage = "Hold expression to be evaluated on a frontend"

Begin["`Private`"]


System`WLJSIOUpdateSymbol;
System`WLJSIOAddTracking;
System`WLJSIOGetSymbol;
System`WLJSIOPromise;
System`WLJSIOPromiseResolve;
System`WLJSIDCardRegister;

System`SlientPing;

SetAttributes[Offload, HoldFirst]

WLJSTransportHandler[cl_, data_ByteArray] := Block[{Global`$Client = cl},
    ToExpression[data//ByteArrayToString];
]

WLJSTransportSend[expr_, client_] := WebSocketSend[client, expr // $DefaultSerializer]

$DefaultSerializer = ExportByteArray[#, "ExpressionJSON", Compact->0]&

WLJSIOAddTracking[symbol_] := With[{cli = Global`$Client, name = SymbolName[Unevaluated[symbol]]},
    WLJSTransportHandler["AddTracking"][symbol, name, cli, Function[{client, value},
        WebSocketSend[client, WLJSIOUpdateSymbol[name, value] // $DefaultSerializer]
    ]]
]

SetAttributes[WLJSIOAddTracking, HoldFirst]

WLJSIOGetSymbol[uid_, params_][expr_] := With[{client = Global`$Client},
    WLJSTransportHandler["GetSymbol"][expr, client, Function[result,
        WebSocketSend[client, WLJSIOPromiseResolve[uid, result] // $DefaultSerializer] 
    ]]
];

WLJSIOPromise[uid_, params_][expr_] := With[{client = Global`$Client},
    (*Print["WLJS promise >> get with id "<>uid];*)
    WebSocketSend[client, WLJSIOPromiseResolve[uid, expr] // $DefaultSerializer];
];

IDCards = <||>;
WLJSIDCardRegister[uid_String] := (Print["Transport registered as "<>uid]; IDCards[uid] = Global`$Client)

WLJSAliveQ[uid_String] := (
    If[KeyExistsQ[IDCards, uid],
        With[{res = !FailureQ[WebSocketSend[IDCards[uid], SlientPing // $DefaultSerializer]]},
            If[!res, IDCards[uid] = .];
            res
        ]
    ,
        Missing[]
    ]
)

WLJSTransportScript[OptionsPattern[] ] := If[NumberQ[OptionValue["Port"] ],
    Switch[{OptionValue["TwoKernels"], OptionValue["Event"], OptionValue["Host"]},
        {False, Null, Null},
        ScriptTemplate[OptionValue["Port"], "server.init({socket: socket})"]
    ,
        {True, Null, Null},
        ScriptTemplate[OptionValue["Port"], "server.init({socket: socket, kernel: true})"]
    ,
        {False, _String, Null},
        ScriptTemplate[OptionValue["Port"], "server.init({socket: socket}); server.emitt('"<>OptionValue["Event"]<>"', 'True', 'Connected');"]
    ,
        {True, _, Null},
        ScriptTemplate[OptionValue["Port"], "server.init({socket: socket, kernel: true}); "]
    ,
        {False, Null, _String},
        ScriptTemplate[OptionValue["Port"], OptionValue["Host"], "server.init({socket: socket}); "]
    ,
        {True, Null, _String},
        ScriptTemplate[OptionValue["Port"], OptionValue["Host"], "server.init({socket: socket, kernel: true}); "]        
    ]
,
    "Specify a mode and a port!"
]

Options[WLJSTransportScript] = {"Port"->Null, "Host"->Null, "Regime"->"Standalone", "Event"->Null, "TwoKernels" -> False}

assets = $InputFileName // DirectoryName // ParentDirectory;

commonScript = StringRiffle[{
    Import[FileNameJoin[{assets, "Assets", "ServerAPI.js"}], "String"],
    Import[FileNameJoin[{assets, "Assets", "InterpreterExtension.js"}], "String"]
}, "\n"];

ScriptTemplate[port_, initCode_] := 
    StringTemplate["
        <script type=\"module\">
            ``
            ;
            const wport = ``;
            var socket = new WebSocket((window.location.protocol == \"https:\" ? \"wss://\" : \"ws://\")+window.location.hostname+':'+wport);
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
              if (wport == 0) return;
              tryreload(() => {
                alert('Connection lost. Please, update the page to see new changes.')
              });
            }; 

            
        </script>
    "][commonScript, port, initCode]

ScriptTemplate[port_, host_, initCode_] := 
    StringTemplate["
        <script type=\"module\">
            ``
            ;
            const wport = ``;
            var socket = new WebSocket((window.location.protocol == \"https:\" ? \"wss://\" : \"ws://\")+'``'+':'+wport);
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
              if (wport == 0) return;
              tryreload(() => {
                alert('Connection lost. Please, update the page to see new changes.')
              });
            }; 

            
        </script>
    "][commonScript, port, host, initCode]    


End[];

EndPackage[];

System`WLXEmbed;
