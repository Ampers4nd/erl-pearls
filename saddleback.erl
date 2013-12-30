-module(saddleback).

-export([invertSimple/2, invertBSearch/2, invertDoubleBSearch/2, doTest/0]).

-record(counter_entry, {id, nextid=1}).

%%%%
%%
%% Assume f(x, y) -> z is a strictly increasing function on natural numbers x, y, and returns natural number z
%% Given z, we want to find all pairs (x, y) s.t. f(x, y) = z
%% This is the invert f z function described in Bird, Ch. 3
%% and can be solved using saddleback search -- three implementations follow
%%
%%%% 

%%%%
%%
%% I've added a CounterID parameter to each function that evaluates f(x,y) 
%% in order to test performance
%%
%%%%

%%%%
%%
%% Simple saddleback, Bird's "Anne" version
%% Move one row at a time 
%%
%%%% 	 
invertSimple(Fn, ZZ) ->
	find(Fn, ZZ, ZZ, {0, ZZ}, 0).

%% start in top left (0, z) and work toward bottom right,
%% stop when u > zbound (far right edge) or v < 0 (bottom edge)
find(_Fn, _ZTarget, ZBound, {UU, VV}, _CounterID)  when  ((VV < 0) or (UU > ZBound)) -> 
	% io:format("Find base case: ~p ~p ~p ~p~n", [_Fn, _ZTarget, ZBound, {UU, VV}]),
	[];
find(Fn, ZTarget, ZBound, {UU, VV}, CounterID) ->
	% io:format("Doing find: ~p ~p ~p ~p~n", [Fn, ZTarget, ZBound, {UU, VV}]),
	incrCounter(CounterID), %%count function calls to test performance
	FUV = Fn(UU, VV),
	if 
		(FUV < ZTarget) -> 		%% f(u, v) < z, so move one column to the right
			find(Fn, ZTarget, ZBound, {UU + 1, VV}, CounterID);
		(FUV == ZTarget) -> 		%% f(u, v) = z, keep (u,v) and move one row down, one column right
			[{UU, VV} | find(Fn, ZTarget, ZBound, {UU + 1, VV - 1}, CounterID)];
		(FUV > ZTarget) -> 		%% f(u, v) > z, move down one row
			find(Fn, ZTarget, ZBound, {UU, VV - 1}, CounterID)
	end.


%%%%
%%
%% Saddleback with binary search in one direction, Bird's "Theo" version
%% same as simple version, but with box bounds computed via binary search
%%
%%%% 	
invertBSearch(Fn, ZZ) ->
	MM = bSearch(fun(YY) -> Fn(0, YY) end, {-1, ZZ + 1}, ZZ, 1), %y-bound
	NN = bSearch(fun(XX) -> Fn(XX, 0) end, {-1, ZZ + 1}, ZZ, 1), %x-bound
	find(Fn, ZZ, NN, {0, MM}, 1).


bSearch(_Fn, {UU, VV}, _ZZ, _CounterID) when (UU + 1 == VV) ->
	UU;
bSearch(Fn, {UU, VV}, ZZ, CounterID) ->
	MidPoint = (UU + VV) div 2,
	incrCounter(CounterID),
	FMid = Fn(MidPoint),
	case FMid =< ZZ of
		true ->
			bSearch(Fn, {MidPoint, VV}, ZZ, CounterID);
		false ->
			bSearch(Fn, {UU, MidPoint}, ZZ, CounterID)
	end.

%%%%
%%
%% Simple saddleback with simultaneous binary search in 2-D, Bird's "Mary" version
%% Divide and conquer, split rectangle 
%%
%%%% 	 

invertDoubleBSearch(Fn, ZZ) ->
	MM = bSearch(fun(YY) -> Fn(0, YY) end, {-1, ZZ + 1}, ZZ, 2), %y-bound
	NN = bSearch(fun(XX) -> Fn(XX, 0) end, {-1, ZZ + 1}, ZZ, 2), %x-bound
	find2 (Fn, ZZ, {0, MM}, {NN, 0}, 2).

find2(_Fn, _ZZ, {UU, VV}, {RR, SS}, _CounterID) when ((UU > RR) or (VV < SS)) ->
	% io:format("Find base case: ~p ~p ~p ~p~n", [_Fn, _ZZ, {UU, VV}, {RR, SS}]),
	[];
find2(Fn, ZZ, {UU, VV}, {RR, SS}, CounterID) when ((VV - SS) =< (RR - UU)) ->
	QQ = (VV + SS) div 2,	
	PP = bSearch(fun(XX) -> Fn(XX, QQ) end, {UU - 1, RR + 1}, ZZ, CounterID),
	% io:format("Find, case 1: ~p ~p ~p ~p ~p ~p~n", [ZZ, {UU, VV}, {RR, SS}, {PP, QQ}, VV - SS, RR - UU]),
	incrCounter(CounterID), %%count function calls to test performance
	FPQ = Fn(PP, QQ),
	Part1 = case (FPQ == ZZ) of
		true ->
			% io:format("true: FPQ: ~p, ZZ: ~p~n", [FPQ, ZZ]),
			[{PP, QQ} | find2(Fn, ZZ, {UU, VV}, {PP - 1, QQ + 1}, CounterID)];
		false ->
			% io:format("false: FPQ ~p, ZZ: ~p~n", [FPQ, ZZ]),
			find2(Fn, ZZ, {UU, VV}, {PP, QQ + 1}, CounterID)
	end,
	Part2 = find2(Fn, ZZ, {PP + 1, QQ - 1}, {RR, SS}, CounterID),
	Part1 ++ Part2;
find2(Fn, ZZ, {UU, VV}, {RR, SS}, CounterID) ->
	PP = (UU + RR) div 2,
	QQ = bSearch(fun(YY) -> Fn(PP, YY) end, {SS - 1, VV + 1}, ZZ, CounterID),
	% io:format("Find, case 2: ~p ~p ~p ~p ~p ~p~n", [ZZ, {UU, VV}, {RR, SS}, {PP, QQ}, VV - SS, RR - UU]),
	Part1 = find2(Fn, ZZ, {UU, VV}, {PP - 1, QQ + 1}, CounterID),
	incrCounter(CounterID), %%count function calls to test performance
	FPQ = Fn(PP, QQ),
	Part2 = case (FPQ == ZZ) of 
		true ->
			[{PP, QQ} | find2(Fn, ZZ, {PP + 1, QQ - 1}, {RR, SS}, CounterID)];
		false ->
			find2(Fn, ZZ, {PP + 1, ZZ}, {RR, SS}, CounterID)
	end,
	Part1 ++ Part2.


%% this (mostly) reproduces the results seen in Bird's Fig. 3.2
doTest() ->
	F0 = fun(XX, YY) -> (pow(2, YY) * (2 * XX + 1)) - 1 end,
	F1 = fun(XX, YY) -> (XX * pow(2, XX) + YY * pow(2,YY)) + 2 * XX + YY end,
	F2 = fun(XX, YY) -> 3 * XX + 27 * YY + YY * YY end,
	F3 = fun(XX, YY) -> XX * XX + YY * YY + XX + YY end,
	F4 = fun(XX, YY) -> XX + pow(2, YY) + YY - 1 end,
	ZZ = 5000,
	initCounter(0),
	Fns = [F0, F1, F2, F3, F4],
	% Fns = [F0, F2, F4],
	% Fns = [F1],
	doTests(Fns, ZZ, []).

doTests([], _ZZ, Acc) ->
	lists:reverse(Acc);
doTests(Fns, ZZ, Acc) ->
	[Fn | TF] = Fns,
	resetCounter(0),	
	Result0 = invertSimple(Fn, ZZ),
	resetCounter(1), 	
	Result1 = invertBSearch(Fn, ZZ),
	resetCounter(2),
	Result2 = invertDoubleBSearch(Fn, ZZ),
	NumPairs = length(Result0),
	Result = ((NumPairs == length(Result1)) and (NumPairs == length(Result2))),
	Counts = {lookupCount(0), lookupCount(1), lookupCount(2)},
	doTests(TF, ZZ, [{Result, NumPairs, Counts} | Acc]).


%% calculates integer N ^ Exp to arbitrary precision for integer NN, integer Exp >= 0
pow(0, _) ->
	0;
pow(_, 0) ->
	1;
pow(NN, Exp) ->
	NN * pow(NN, (Exp - 1)).

%% use an ets table to count function calls
initCounter(CounterID) ->
	case ets:info(t_counts) of
		undefined ->
    		ets:new(t_counts, [set, {keypos, 2}, public, named_table]);
    	_ ->
    		%% do nothing
    		table_exists
    end,
    resetCounter(CounterID).

incrCounter(CounterID) ->
	case ets:info(t_counts) of
		undefined ->
			%% do nothing
			no_table;
		_ ->
			ets:update_counter(t_counts, CounterID, {3, 1})
	end.

resetCounter(CounterID) ->
	case ets:info(t_counts) of
		undefined ->
			%% do nothing
			no_table;
		_ ->
			ets:insert(t_counts, #counter_entry{id=CounterID, nextid=0})
	end.

lookupCount(CounterID) ->
	[Counter] = ets:lookup(t_counts, CounterID),
	Counter#counter_entry.nextid.
