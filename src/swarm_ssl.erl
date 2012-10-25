%%
%% Copyright (C) 2012 Jeremey Barrett <jlb@rot26.com>
%%
%% Based on ranch_ssl, Copyright (c) 2011-2012, Loïc Hoguin <essen@ninenines.eu>
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

-module(swarm_ssl).

-export([connect/4, listen/1, accept/1, accept/2]).
-export([recv/3, send/2, close/1]).
-export([setopts/2, controlling_process/2, peername/1, sockname/1]).
-export([subject_name/1, dn/1, type/0]).

-include("../include/swarm.hrl").


connect(Host, Port, Opts, Timeout) ->
    ssl:connect(Host, Port,
		Opts ++ [binary, {active, false}, {packet, raw}], Timeout).

listen(Opts) ->
    {port, Port} = lists:keyfind(port, 1, Opts),         % ensure exists
    Backlog = proplists:get_value(backlog, Opts, 1024),  % ensure reasonable
    {certfile, _} = lists:keyfind(certfile, 1, Opts),    % ensure exists
    {keyfile, _} = lists:keyfind(keyfile, 1, Opts),      % ensure exists

    ListenOpts = Opts ++ [binary, 
                          {active, false},
                          {backlog, Backlog}, 
                          {packet, raw}, 
                          {recbuf, 8192},
                          {reuseaddr, true}],

    ssl:listen(Port, ListenOpts).

accept(LSocket) ->
    accept(LSocket, infinity).

accept(LSocket, Timeout) ->
    case ssl:transport_accept(LSocket, Timeout) of
        {ok, CSocket} ->
            ssl_accept(CSocket, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

recv(Socket, Length, Timeout) ->
    ssl:recv(Socket, Length, Timeout).

send(Socket, Packet) ->
    ssl:send(Socket, Packet).

setopts(Socket, Opts) ->
    ssl:setopts(Socket, Opts).

controlling_process(Socket, Pid) ->
    ssl:controlling_process(Socket, Pid).

peername(Socket) ->
    ssl:peername(Socket).

close(Socket) ->
    ssl:close(Socket).

sockname(Socket) ->
    ssl:sockname(Socket).

subject_name(Socket) ->
    swarm_x509:dn_oneline(dn(Socket)).

dn(Socket) ->
    case ssl:peercert(Socket) of
        {ok, DER} ->
            swarm_x509:dn_record(DER);
        _ ->
            #swarm_dn{}
    end.

type() ->
    ssl.


%% Internal ssl:ssl_accept wrapper

ssl_accept(Socket, Timeout) ->
    case ssl:ssl_accept(Socket, Timeout) of
        ok ->
            {ok, Socket};
        {error, Reason} ->
            {error, Reason}
    end.

