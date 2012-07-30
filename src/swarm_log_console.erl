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

%% NOTE: This module is intended purely for testing / development!

-module(swarm_log_console).
-compile([export_all]).

critical(Fmt, Args) ->
    io:format("swarm CRITICAL " ++ Fmt, Args).

error(Fmt, Args) ->
    io:format("swarm ERROR " ++ Fmt ++ "\n", Args).

warning(Fmt, Args) ->
    io:format("swarm WARNING " ++ Fmt ++ "\n", Args).

info(Fmt, Args) ->
    io:format("swarm INFO " ++ Fmt ++ "\n", Args).

debug(Fmt, Args) ->
    io:format("swarm DEBUG " ++ Fmt ++ "\n", Args).

