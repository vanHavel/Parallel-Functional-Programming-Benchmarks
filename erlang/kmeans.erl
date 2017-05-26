-module(kmeans).
-compile(export_all).

assign(Means, Data, Chunksize) ->
  L = length(Data),
  if
    L =< Chunksize ->
      assign_sequential(Means, Data);
    true ->
      {Part, Rest} = lists:split(Chunksize, Data),
      Parent = self(),
      R = make_ref(),
      spawn_link(fun() ->
        Parent ! {R, assign_sequential(Means, Part)}
      end),
      RestRes = assign(Means, Rest, Chunksize),
      PartRes = receive {R, Res} -> Res end,
      lists:append(PartRes, RestRes)
  end.

assign_sequential(Means, Data) ->
  lists:map(fun(Point) ->
    closest(lists:map(fun(Mean) -> distance(Point, Mean) end, Means))
  end, Data).

closest(Ds) ->
  element(2, lists:min(
    lists:zip(Ds, lists:seq(0, length(Ds) - 1)))
  ).

distance({Px, Py}, {Qx, Qy}) ->
  math:pow(Px - Qx, 2) + math:pow(Py - Qy, 2).
