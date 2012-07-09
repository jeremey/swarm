%%
%% Copyright (C) 2012 Jeremey Barrett <jlb@rot26.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining
%% a copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to
%% permit persons to whom the Software is furnished to do so, subject to
%% the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
%% LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
%% OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
%% WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%

-module(swarm_listener).

-export([start_link/5]).


start_link(Name, AcceptorCount, Transport, TransOpts, {M, F, A}) ->
    Pid = spawn_link(fun() ->
                             run(Name, AcceptorCount, Transport, TransOpts, {M, F, A})
                     end),
    {ok, Pid}.


run(Name, AcceptorCount, Transport, TransOpts, {M, F, A}) ->
    {ok, LSock} = Transport:listen(TransOpts),

    Self = self(),
    SpawnFun = fun() ->
                       spawn(fun() -> acceptor(Self, Name, LSock, Transport, {M, F, A}) end)
               end,
    
    [SpawnFun() || _X <- lists:seq(1, AcceptorCount)],

    loop(Name, SpawnFun).


loop(Name, SpawnFun) ->
    receive
        accepted ->
            SpawnFun(),
            loop(Name, SpawnFun);
        _ ->
            loop(Name, SpawnFun)
    end.


acceptor(LPid, Name, LSock, Transport, {M, F, A}) ->
    case Transport:accept(LSock) of
        {ok, S} ->
            LPid ! accepted,
            erlang:apply(M, F, [S, Name, Transport] ++ A);
        {error, closed} ->
            ok
    end.

