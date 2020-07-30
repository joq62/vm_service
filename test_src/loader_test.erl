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

-export([start/0]).

%% ====================================================================
%% External functions
%% ====================================================================


% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start()->
    ?debugMsg("start check intial sd_service"),
    ?assertEqual(ok,initial_sd_service()),


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


initial_sd_service()->
    ?assertMatch([{_vm_service,vm_test@asus},
		  {_config_service,vm_test@asus},
		  {_log_service,vm_test@asus},
		  {_sd_service,vm_test@asus}],sd_service:fetch_all(all)),
    
    ok.

one_service()->
    
    ServiceId="adder_service",
    ?assertEqual({ok,ServiceId},vm_service:start_service(ServiceId)),
    ?assertEqual(42,adder:add(20,22)),
    ?assertEqual([vm_test@asus],sd_service:fetch_service(ServiceId)),
    
    ok.

second_service()->
    ServiceId="multi_service",
    ?assertEqual({ok,ServiceId},vm_service:start_service(ServiceId)),
    ?assertEqual(420,multi:multi(42,10)),
    ?assertEqual([vm_test@asus],sd_service:fetch_service(ServiceId)),
    ok.

try_start_first_again()->
    ServiceId="adder_service",
    ?assertEqual({error,["adder_service"]},vm_service:start_service(ServiceId)),
    ?assertEqual(42,adder_service:add(20,22)),
    ?assertEqual([vm_test@asus],sd_service:fetch_service(ServiceId)),
    ok.	 

stop_services()->
    ServiceId="adder_service",
    ?assertEqual(ok,vm_service:stop_service(ServiceId)),
    ?assertEqual([],sd_service:fetch_service(ServiceId)),
    ?assertMatch({badrpc,_},rpc:call(node(),adder_service,add,[20,22])),

    ?assertEqual(420,multi_service:multi(42,10)),
    ?assertEqual(ok,vm_service:stop_service("multi_service")),
    ?assertEqual([],sd_service:fetch_service("multi_service")),
    ?assertMatch({badrpc,_},rpc:call(node(),multi_service,multi,[20,22])),
    ok.
    
start_non_existing()->
    ?assertMatch({badrpc,_},rpc:call(node(),vm_service,start_service,[glurk])),
    ok.
