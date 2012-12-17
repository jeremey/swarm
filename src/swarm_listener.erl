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

-include("../include/swarm.hrl").

start_link(Name, AcceptorCount, Transport, TransOpts, {M, F, A}) ->
    Pid = spawn_link(fun() ->
                             run(Name, AcceptorCount, Transport, TransOpts, {M, F, A})
                     end),
    {ok, Pid}.


run(Name, AcceptorCount, Transport, TransOpts, {M, F, A}) ->
    LogModule = swarm:get_env(log_module),
    
    %% trap exits
    process_flag(trap_exit, true),

    {ok, LSock} = Transport:listen(TransOpts),

    Self = self(),
    SpawnFun = fun() ->
                       spawn_link(fun() -> acceptor(Self, Name, LSock, Transport, LogModule, {M, F, A}) end)
               end,

    [SpawnFun() || _X <- lists:seq(1, AcceptorCount)],

    loop(Name, LogModule, SpawnFun, AcceptorCount, 0, 0, 0).


loop(Name, LogModule, SpawnFun, Acceptors, Count, RunningCount, ErrorCount) ->
    LogModule:debug("~s configured acceptors: ~p, actual: ~p, running: ~p, errored: ~p", [Name, Acceptors, Count, RunningCount, ErrorCount]),
    receive
        listening ->
            loop(Name, LogModule, SpawnFun, Acceptors, Count+1, RunningCount, ErrorCount);

        accepted ->
            SpawnFun(),
            loop(Name, LogModule, SpawnFun, Acceptors, Count-1, RunningCount, ErrorCount);

        {'EXIT', FromPid, normal} ->
            LogModule:debug("~s child pid ~p died normally", [Name, FromPid]),                % temporarily info
            loop(Name, LogModule, SpawnFun, Acceptors, Count, RunningCount-1, ErrorCount);

        {'EXIT', FromPid, Reason} ->
            LogModule:info("~s child pid ~p died with reason ~p", [Name, FromPid, Reason]),  % temporarily info
            loop(Name, LogModule, SpawnFun, Acceptors, Count, RunningCount-1, ErrorCount+1);

        _ ->
            loop(Name, LogModule, SpawnFun, Acceptors, Count, RunningCount, ErrorCount)
    end.


acceptor(LPid, Name, LSock, Transport, LogModule, {M, F, A}) ->
    %% eprof:start_profiling([self()]),
    LPid ! listening,
    Accept = Transport:accept(LSock),
    LPid ! accepted,
    case Accept of
        {ok, S} ->
            erlang:apply(M, F, [S, Name, Transport, get_info(Transport, S)] ++ A);
        {error, closed} ->
            LogModule:debug("~s Transport:accept received {error, closed}", [Name]),
            ok;
        Error ->
            LogModule:error("~s Transport:accept error ~p", [Name, Error]),
            Error
    end,
    %% eprof:stop_profiling(),
    %% eprof:analyze(),
    ok.


get_info(Transport, Socket) ->
    {ok, {Addr, Port}} = Transport:peername(Socket),
    DN = Transport:dn(Socket),
    #swarm_info{peer_addr = Addr,
                peer_port = Port,
                peer_dn = DN}.


