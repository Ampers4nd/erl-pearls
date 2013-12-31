-module(selection).

-export([smallest/2, smallestBF/2, doTest/0]).

%%%%%%%%%
%%
%% Returns the kth smallest (0-based) element from the union of XA, YA
%% where XA and YA are disjoint sorted arrays
%%
%% Taken from Bird, Ch. 4, but NOTE that there is an error in the published algorithm (1st edition)
%% This implementation is based on the errata found here: http://www.haskell.org/pipermail/haskell/2011-April/022763.html 
%%
%% Note: For a counterexample of the published (1st ed.) algorithm, use {L1, L2} = {[14,17,24], [7,23]}, K = 1 
%%
%%%%%%%%%%

%%brute force implementation, join lists, sort, and grab kth element, O(|XA| + |YA|)
%%assuming erlang implementation of lists:merge is O(|XA| + |YA|)
smallestBF(KK, {XA, YA}) ->
	MergedList =  lists:merge(array:to_list(XA), array:to_list(YA)),
	% io:format("Merged List: ~p~n", [MergedList]),
	lists:nth(KK + 1, MergedList). %% K + 1 b/c lists are 1-based, arrays are 0-based

%%use direct access to array elements for faster implementation, O(log(X) + log(Y))
%%assuming erlang's array lookup is constant time

smallest(KK, {XA, YA}) ->
	search(KK, {0, array:size(XA), XA}, {0, array:size(YA), YA}).

search(KK, {LeftX, RightX, _XA}, {LeftY, _RightY, YA}) when (LeftX == RightX) ->
	% io:format("Base case X, K = ~p~n", [KK + LeftY]),
	array:get(KK + LeftY, YA);
search(KK, {LeftX, _RightX, XA}, {LeftY, RightY, _YA}) when (LeftY == RightY) ->
	% io:format("Base case Y, K = ~p~n", [KK]),
	array:get(KK + LeftX, XA);
search(KK, {LeftX, RightX, XA}, {LeftY, RightY, YA}) ->
	MidX = (LeftX + RightX) div 2,
	MidY = (LeftY + RightY) div 2,
	% io:format("KK:~p {LX, RX}: ~p, {LY, RY}: ~p Mx: ~p My: ~p~n", [KK, {LeftX, RightX}, {LeftY, RightY}, MidX, MidY]),	
	case {array:get(MidX, XA) < array:get(MidY, YA), KK =< ((MidX - LeftX) + (MidY - LeftY))} of
		{true, true} ->
			% io:format("Case 1~n~n"),
			search(KK, {LeftX, RightX, XA}, {LeftY, MidY, YA});
		{true, false} ->
			% io:format("Case 2~n~n"),
			search(KK - (MidX - LeftX)- 1, {MidX + 1, RightX, XA}, {LeftY, RightY, YA});
		{false, true} ->
			% io:format("Case 3~n~n"),
			search(KK, {LeftX, MidX, XA}, {LeftY, RightY, YA});
		{false, false} ->
			% io:format("Case 4~n~n"),
			search(KK - (MidY - LeftY) - 1, {LeftX, RightX, XA}, {MidY + 1, RightY, YA})
	end.

%% To test, I generate a series of semi-random pairs of disjoint, ascending arrays of varying lengths
%% and compare the results of the 'smallest' implementation with the results of the brute force implementation
%% Result will be 'true' if all tests pass, Results is a list of the results of individual tests (hopefully all true)
doTest() ->
	NumTests = 100,
	{Results, Result} = doTests(NumTests, [], true),
	case Result of 
		true ->
			io:format("All tests passed:-)~n");
		false ->
			io:format("Tests Failed. Results: ~p~n", [Results])
	end,
	ok.

doTests(0, Acc, ResultAcc) ->
	{lists:reverse(Acc), ResultAcc};
doTests(NN, Acc, ResultAcc) ->
	NumElems = random:uniform(100),
	{L1, L2} = randomLists(NumElems),
	A1 = array:fix(array:from_list(L1)),
	A2 = array:fix(array:from_list(L2)),
	KK = random:uniform(NumElems) - 1,
	BF = smallestBF(KK, {A1, A2}),
	SM = smallest(KK, {A1, A2}),
	Result = (BF == SM),
	case Result of
		true ->
			% io:format("Test passed.~n");
			pass;
		false ->
			io:format("Test failed.~nA1: ~p~nA2: ~p~nKK: ~p~nBF: ~p, SM: ~p~n", [A1, A2, KK, BF, SM]),
			fail
	end,
	doTests(NN - 1, [Result | Acc], Result and ResultAcc).

%%generate two disjoint ascending lists, random-ish
randomLists(NN) ->
	randomLists(NN, [], [], 0).

randomLists(0, Acc1, Acc2, _MaxElem) ->
	{lists:reverse(Acc1), lists:reverse(Acc2)};
randomLists(NN, Acc1, Acc2, MaxElem) ->
	Next = MaxElem + random:uniform(10),
	case random:uniform(2) of
		1 ->
			randomLists(NN - 1, [Next | Acc1], Acc2, Next);
		2 ->
			randomLists(NN - 1, Acc1, [Next | Acc2], Next)
	end.
