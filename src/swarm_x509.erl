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

-module(swarm_x509).

-export([dn_record/1, dn_oneline/1]).

-include_lib("public_key/include/public_key.hrl"). 

-include("../include/swarm.hrl").


%% API

dn_record(DER) ->
    Cert = public_key:pkix_decode_cert(DER, otp),
    OTPCert = Cert#'OTPCertificate'.tbsCertificate,
    get_dn_parts(OTPCert#'OTPTBSCertificate'.subject).

dn_oneline(#swarm_dn{c = C, st = ST, l = L, o = O, ou = OU, cn = CN, email = EM}) ->
    list_to_binary(interleave(<<"/">>, 
                              [X || [_|_] = X <- 
                                        [dn_oneline_fmt(c,C),
                                         dn_oneline_fmt(st,ST),
                                         dn_oneline_fmt(l,L),
                                         dn_oneline_fmt(o,O),
                                         dn_oneline_fmt(ou,OU),
                                         dn_oneline_fmt(cn,CN),
                                         dn_oneline_fmt(email,EM)]])).


%% Internal functions

get_dn_parts(Part) ->
    {rdnSequence, Parts} = Part,
    get_dn_parts(Parts, #swarm_dn{}).

get_dn_parts([], Record) ->
    Record;
get_dn_parts([H|T], Record) ->
    R1 = get_dn_part(H, Record),
    get_dn_parts(T, R1).

get_dn_part([{'AttributeTypeAndValue', OID, Value}], Record) ->
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
    
dn_string({'printableString', S}) ->
    S;
dn_string(S) ->
    S.


dn_oneline_fmt(Type, Values) ->
    interleave(<<"/">>, [dn_atom_name(Type) ++ "=" ++ X || X <- Values]).

dn_atom_name(Type) ->
    case Type of
        c ->
            "C";
        st ->
            "ST";
        l ->
            "L";
        o ->
            "O";
        cn ->
            "CN";
        ou ->
            "OU";
        email ->
            "emailAddress";
        _ ->
            ""
    end.
            
interleave(X, L) ->
    lists:reverse(interleave(X, L, [])).

interleave(_, [], Acc) ->
    Acc;
interleave(_, [A], Acc) ->
    [A|Acc];
interleave(X, [A|T], Acc) ->
    interleave(X, T, [X,A|Acc]).
    
