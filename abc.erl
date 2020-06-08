-module(abc).
-export([reduce/3]).

reduce(_Fun,[],Acc)->Acc;
reduce(Fun,[Head|Tail],Acc)->reduce(Fun,Tail,Fun(Head,Acc)).
