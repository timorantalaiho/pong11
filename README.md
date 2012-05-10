Getting started
================

Run server

    nc -l 8080

Run huskybot

    huskybot

Paste r.json contents to nc:
    
    { "msgType": "chessboard", "data": { "pieces": [ {"color": "black", "position": "d2"}, {"color": "white", "position": "c3"} ] } }


=======
HuskyBot
========

Starting point for building a Haskell algorithm competition bot for the upcoming Reaktor code camp.

Competition Protocol
====================

- JSON over TCP/IP
- Single JSON object message per line (separated by `\n`)
- Message structure consists of `msgType` and `data`
- Client starts with a JOIN message:

~~~ json
     {"msgType":"join","data":"lollerström"}
~~~

- Client waits until server starts a competition
- Start message is a list of player names:

~~~ json
     {"msgType":"gameStarted","data":["lollerström", "ökytunkkaaja"]}
~~~
