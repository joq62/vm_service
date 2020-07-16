%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(loader_test). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
-define(GIT_URL,"https://github.com/joqerlang/").
-export([start/0,start_app/1]).

%% ====================================================================
%% External functions
%% ====================================================================
start_app(ServiceId)->
    loader:start(ServiceId,git,?GIT_URL).

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start()->
    ?debugMsg("start one service"),
    ?assertEqual(ok,one_service()),

    ?debugMsg("start second service"),
    ?assertEqual(ok,second_service()),
    
    ?debugMsg("try to start the first again"),
    ?assertEqual(ok,try_start_first_again()), 

    ?debugMsg("stop services"),
    ?assertEqual(ok,stop_services()), 

    ?debugMsg("try to start non existing"),
    ?assertEqual(ok,start_non_existing()), 

    ok.

one_service()->
    ServiceId="adder_service",
    ?assertEqual({ok,ServiceId},loader:start(ServiceId,git,?GIT_URL)),
    ?assertEqual(42,adder:add(20,22)),
    ok.

second_service()->
    ServiceId="multi_service",
    ?assertEqual({ok,ServiceId},loader:start(ServiceId,git,?GIT_URL)),
    ?assertEqual(420,multi:multi(42,10)),
    ok.

try_start_first_again()->
    ServiceId="adder_service",
    ?assertEqual({error,["adder_service"]},loader:start(ServiceId,git,?GIT_URL)),
    ?assertEqual(42,adder_service:add(20,22)),
    ok.	 

stop_services()->
    ServiceId="adder_service",
    ?assertEqual(ok,loader:stop(ServiceId)),
    ?assertMatch({badrpc,_},rpc:call(node(),adder_service,add,[20,22])),
    ?assertEqual(420,multi_service:multi(42,10)),
    ?assertEqual(ok,loader:stop("multi_service")),
    ?assertMatch({badrpc,_},rpc:call(node(),multi_service,multi,[20,22])),
    ok.
    
start_non_existing()->
    ?assertMatch({badrpc,_},rpc:call(node(),loader,start,["glurk",git,?GIT_URL])),
    ok.
