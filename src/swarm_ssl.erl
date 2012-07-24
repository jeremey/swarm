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
-export([subject_name/1, dn/1]).

-include_lib("public_key/include/public_key.hrl"). 

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
    case ssl:peercert(Socket) of
        {ok, DER} ->
            get_subject_name(DER);
        _ -> <<>>
    end.

dn(Socket) ->
    case ssl:peercert(Socket) of
        {ok, DER} ->
            get_dn_record(DER);
        _ ->
            #swarm_dn{}
    end.


%% Internal ssl:ssl_accept wrapper

ssl_accept(Socket, Timeout) ->
    case ssl:ssl_accept(Socket, Timeout) of
        ok ->
            {ok, Socket};
        {error, Reason} ->
            {error, Reason}
    end.


%% X.509/PKIX parsing

get_dn_record(DER) ->
    Cert = public_key:pkix_decode_cert(DER, otp),
    OTPCert = Cert#'OTPCertificate'.tbsCertificate,
    get_dn_parts(OTPCert#'OTPTBSCertificate'.subject).

get_dn_parts(Part) ->
    {rdnSequence, Parts} = Part,
    get_dn_parts(Parts, #swarm_dn{}).

get_dn_parts([H|T], Record) ->
    R1 = get_dn_part(H, Record),
    get_dn_parts(T, R1).

get_dn_part({'AttributeTypeAndValue', OID, Value}, Record) ->
    case OID of
        ?'id-at-countryName' ->
            Record#swarm_dn{c = Record#swarm_dn.c ++ [dn_string(Value)]};
        ?'id-at-stateOrProvinceName' ->
            Record#swarm_dn{st = Record#swarm_dn.st ++ [dn_string(Value)]};
        ?'id-at-localityName' ->
            Record#swarm_dn{l = Record#swarm_dn.l ++ [dn_string(Value)]};
        ?'id-at-organizationName' ->
            Record#swarm_dn{o = Record#swarm_dn.o ++ [dn_string(Value)]};
        ?'id-at-commonName' ->
            Record#swarm_dn{cn = Record#swarm_dn.cn ++ [dn_string(Value)]};
        ?'id-at-organizationalUnitName' ->
            Record#swarm_dn{ou = Record#swarm_dn.ou ++ [dn_string(Value)]};
        ?'id-emailAddress' ->
            Record#swarm_dn{email = Record#swarm_dn.email ++ [dn_string(Value)]};
        _ ->
            Record
    end.
    


get_subject_name(DER) ->
    Cert = public_key:pkix_decode_cert(DER, otp),
    OTPCert = Cert#'OTPCertificate'.tbsCertificate,
    list_to_binary(name_to_string(OTPCert#'OTPTBSCertificate'.subject)).

name_to_string(DN) ->
    {rdnSequence, DNParts} = DN,
    decode_dn_part(DNParts, "").

decode_dn_part([H|T], Acc) ->
    decode_dn_part(T, decode_dn_part(H, Acc));

decode_dn_part([], Acc) ->
    Acc;

decode_dn_part({'AttributeTypeAndValue', OID, Value}, Acc) ->
    Acc ++ "/" ++ oid_name(OID) ++ "=" ++ dn_string(Value);

decode_dn_part(_, Acc) ->
    Acc.

dn_string({'printableString', S}) ->
    S;
dn_string(S) ->
    S.

oid_name(OID) ->
    case OID of
        ?'id-at-countryName' ->
            "C";
        ?'id-at-stateOrProvinceName' ->
            "ST";
        ?'id-at-localityName' ->
            "L";
        ?'id-at-organizationName' ->
            "O";
        ?'id-at-commonName' ->
            "CN";
        ?'id-at-organizationalUnitName' ->
            "OU";
        ?'id-emailAddress' ->
            "emailAddress";
        _ ->
            ""
    end.
            

