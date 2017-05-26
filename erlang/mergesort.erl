-module(mergesort).
-compile(export_all).

merge_sequential(Xs, []) -> Xs;
merge_sequential([], Ys) -> Ys;
merge_sequential([X|Xs], [Y|Ys]) ->
  if
    X < Y ->
      [X|merge_sequential(Xs, [Y|Ys])];
    true ->
      [Y|merge_sequential([X|Xs], Ys)]
  end.

% Important: XS length must be a power of 2!
bitonic_merge(Xs, Cutoff) ->
  L = length(Xs),
  if
    L =< Cutoff ->
      bitonic_merge_sequential(Xs);
    true ->
      {Left, Right} = lists:split(L div 2, Xs),
      Mins = [min(A,B) || {A,B} <- lists:zip(Left, Right)],
      Maxs = [max(A,B) || {A,B} <- lists:zip(Left, Right)],
      Parent = self(),
      R = make_ref(),
      spawn_link(fun() ->
        Result = bitonic_merge(Maxs, Cutoff),
        Parent ! {R, Result}
      end),
      MinsMerged = bitonic_merge(Mins, Cutoff),
      MaxsMerged = receive
        {R, Result} -> Result
      end,
      lists:append (MinsMerged, MaxsMerged)
end.

bitonic_merge_sequential([]) -> [];
bitonic_merge_sequential([X]) -> [X];
bitonic_merge_sequential(Xs) ->
  {Left, Right} = lists:split(length(Xs) div 2, Xs),
  Mins = [min(A,B) || {A,B} <- lists:zip(Left, Right)],
  Maxs = [max(A,B) || {A,B} <- lists:zip(Left, Right)],
  MinsMerged = bitonic_merge_sequential(Mins),
  MaxsMerged = bitonic_merge_sequential(Maxs),
  lists:append (MinsMerged, MaxsMerged).


mergesort_sequential([]) -> [];
mergesort_sequential([X]) -> [X];
mergesort_sequential(Xs) ->
  {Left, Right} = lists:split(length(Xs) div 2, Xs),
  merge_sequential (mergesort_sequential(Left), mergesort_sequential(Right)).

mergesort(Xs, Cutoff) ->
  L = length(Xs),
  if
    L =< Cutoff ->
      mergesort_sequential(Xs);
    true ->
      {Left, Right} = lists:split(L div 2, Xs),
      Parent = self(),
      R = make_ref(),
      spawn_link(fun() ->
        Result = mergesort(Right, Cutoff),
        Parent ! {R, Result}
      end),
      LeftSorted = mergesort(Left, Cutoff),
      RightSorted = receive
        {R, Result} ->
          Result
      end,
      merge_sequential(LeftSorted, RightSorted)
  end.

% Important: XS length must be a power of 2!
% more precisely, the length must be of the form Cutoff * 2^n for some n
bitonic_mergesort(Xs, Cutoff) ->
  L = length(Xs),
  if
    L =< Cutoff ->
      mergesort_sequential(Xs);
    true ->
      {Left, Right} = lists:split(L div 2, Xs),
      Parent = self(),
      R = make_ref(),
      spawn_link(fun() ->
        Result = lists:reverse (bitonic_mergesort(Right, Cutoff)),
        Parent ! {R, Result}
      end),
      LeftSorted = bitonic_mergesort(Left, Cutoff),
      RightSorted = receive
        {R, Result} -> Result
      end,
      bitonic_merge (lists:append(LeftSorted, RightSorted), Cutoff)
  end.

bitonic_mergesort_sequential([]) -> [];
bitonic_mergesort_sequential([X]) -> [X];
bitonic_mergesort_sequential(Xs) ->
  L = length(Xs),
  {Left, Right} = lists:split(L div 2, Xs),
  LeftSorted = bitonic_mergesort_sequential(Left),
  RightSorted = lists:reverse(bitonic_mergesort_sequential(Right)),
  bitonic_merge_sequential (lists:append(LeftSorted, RightSorted)).
