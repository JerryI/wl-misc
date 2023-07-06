(* ::Package:: *)

(* ::Chapter:: *)
(*Extensions*)


BeginPackage["JerryI`Misc`Channels`", {"KirillBelov`WebSocketHandler`"}]; 


WebSocketChannel::usage = 
"
WebSocketChannel[\"channel name\"][\"Subscribe\", client_SocketObject] subscribe client to a channel
WebSocketChannel[\"channel name\"][\"Publish\", expr_] publish an expression to a channel
WebSocketChannel[\"channel name\"][\"Serializer\"] = ToString define a serializer function for a channel

WebSocketChannel[Automatic][\"Push\", client_SocketObject, expr_] send a message to a particular client
"; 

Begin["`Private`"];

Channels = <||>

WebSocketChannel[name_]["Subscribe", client_SocketObject] := (
    If[!KeyExistsQ[Channels, name], Channels[name] = <||>];
    Channels[name][client // First] = True;
    Print["Client "<>client[[1]]<>" subscribed to a channel "<>name];
)

WebSocketChannel[name_]["Subscribe"] := (
    If[!KeyExistsQ[Channels, name], Channels[name] = <||>];
    Channels[name][Global`client // First] = True;
    Print["Client "<>Global`client[[1]]<>" subscribed to a channel "<>name];
)

WebSocketChannel[name_]["Publish", expr_] := With[{data = expr // (WebSocketChannel[name]["Serializer"])},
    If[FailureQ[WebSocketSend[SocketObject[#], data]], Channels[name, #] = .; Print["Channel failed. "<>#<>" was removed from "<>name];] &/@ Keys[Channels[name]]
]

WebSocketChannel[name_]["Push", expr_] := With[{data = expr // (WebSocketChannel[name]["Serializer"])},
    Print["Publishing... for"];
    Print[ Channels[name]];
    If[FailureQ[Print[#]; Print[data]; Print[WebSocketSend[SocketObject[#], data]]], Channels[name, #] = .; Print["Channel failed. "<>#<>" was removed from "<>name];] &/@ Keys[Channels[name]]
]

WebSocketChannel[Automatic]["Push", expr_] := WebSocketSend[Global`client, expr // (WebSocketChannel[Automatic]["Serializer"])]
WebSocketChannel[Automatic]["Push", client_SocketObject, expr_] := WebSocketSend[client, expr // (WebSocketChannel[Automatic]["Serializer"])]
WebSocketChannel[Automatic]["Serializer"] = $DefaultSerializer

$DefaultSerializer := ToString

End[]

EndPackage[]