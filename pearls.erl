-module(pearls).

-export[smallestFree/1, msc/1].

%%%%
%%
%% Returns smallest positive integer that is not in the list NotAvailable
%%
%% Divide and conquer, O(n), adapted from Bird, Ch. 1
%%
%%%% 
smallestFree(NotAvailable) when is_list(NotAvailable)->
	smallestFree(0, NotAvailable).

smallestFree(Acc, []) ->
	% io:format("Base case, Acc: ~p~n", [Acc]),
	Acc;
smallestFree(Acc, NotAvailable) ->
	% Midpoint = Acc + (length(NotAvailable) div 2),
	MidPlus1 = Acc + (length(NotAvailable) div 2) + 1,
	{Smaller, Larger} = lists:partition(fun(X) -> X < MidPlus1 end, NotAvailable),	
	% io:format("Non-base case, Acc: ~p, Mid: ~p, Smaller: ~p, Larger: ~p~n", [Acc, Midpoint, Smaller, Larger]),
	case (length(Smaller) == MidPlus1 - Acc) of
		true ->
			% io:format("Case 1: ~p~n", [Larger]), 
			smallestFree(MidPlus1, Larger);
		false ->
			% io:format("Case 2: ~p~n", [Smaller]),
			smallestFree(Acc, Smaller)
	end.

%%%%
%%
%% Returns maximum surpasser count of SomeList
%% where surpasser j of element i satisfies i < j and x[i] < x[j]
%% and surpasser count of an element is the number of its surpassers
%%
%% Divide and conquer, O(n log n), adapted from Bird, Ch. 2
%% 
%%%%
msc(SomeList) when is_list(SomeList) ->
	SCounts = countTable(SomeList),
	[SHead | _Tail] = SCounts,
	lists:foldl(fun(A, B) ->  largerCount(A, B) end, SHead, SCounts).

countTable([]) ->
    error;
countTable(SomeList) ->
	case(length(SomeList) == 1) of
		true ->
			[H|_T] = SomeList,
			[{H, 0}];
		false ->
			ListLength = length(SomeList),
			Mid = ListLength div 2,
			{Left, Right} = lists:split(Mid, SomeList),
			joinCounts(ListLength - Mid, countTable(Left), countTable(Right))
	end.

joinCounts(0, XS, []) ->
	XS;
joinCounts(_NN, [], YS) ->
	YS;
joinCounts(NN, XS, YS) ->
	[{XX, XCount} | XTail] = XS,
	[{YY, YCount} | YTail] = YS,
	case XX < YY of
		true ->
			[{XX, XCount + NN} | joinCounts(NN, XTail, YS)];
		false ->
			[{YY, YCount} | joinCounts(NN - 1, XS, YTail)]
	end.

largerCount({X1, Y1}, {X2, Y2}) ->
	case (Y2 > Y1) of
		true ->
			{X2, Y2};
		false ->
			{X1, Y1}
	end.
