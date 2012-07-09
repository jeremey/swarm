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

-module(swarm).
-compile([export_all]).

-define(APPLICATION, ?MODULE).


%% Primary public API

-spec start_listener(atom(), 
                     non_neg_integer(), 
                     module(), 
                     list(), 
                     {module(), function(), list()}) -> 
                            {ok, pid()}.
start_listener(Name, AcceptorCount, Transport, TransOpts, {M, F, A}) ->
    supervisor:start_child(swarm_sup, child_spec(Name, AcceptorCount,
                                                 Transport, TransOpts, {M, F, A})).

-spec child_spec(atom(), 
                 non_neg_integer(), 
                 module(), 
                 list(),
                 {module(), function(), list()}) -> 
                        supervisor:child_spec().
child_spec(Name, AcceptorCount, Transport, TransOpts, {M, F, A}) ->
    {{swarm_listener, Name}, 
     {swarm_listener, start_link, 
      [Name, AcceptorCount, Transport, TransOpts, {M, F, A}]}, 
     permanent, 5000, worker, [swarm_listener]}.


%% Start/stop/utility API

start() ->
    start(?APPLICATION).


stop() ->
    application:stop(?APPLICATION).


start(App) ->
    start(App, undefined).


start(App, PrevDep) ->
    Res = application:start(App),
    case Res of
        ok -> ok;
        {error, {already_started, App}} -> ok;
        {error, {not_started, PrevDep}} ->
            erlang:error({not_started, PrevDep});  % dependency failed
        {error, {not_started, Dep}} ->
            start(Dep, undefined),                 % start dependency
            ok = start(App, Dep)                   % try again
    end.


get_env(Key) ->
    get_env(Key, undefined).


get_env(Key, Default) ->
    case application:get_env(?APPLICATION, Key) of
        {ok, X} -> X;
        undefined -> Default
    end.


set_env(Key, Value) ->
    application:set_env(?APPLICATION, Key, Value).

