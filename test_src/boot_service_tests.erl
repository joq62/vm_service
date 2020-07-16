%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Create1d : 10 dec 2012
%%% -------------------------------------------------------------------
-module(boot_service_tests). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

-ifdef(worker).
-define(LOADED_SERVICES,[{"kernel",worker_boot_test@asus},
			 {"stdlib",worker_boot_test@asus}]).
-else.
-define(LOADED_SERVICES,[]).
-endif.




%% --------------------------------------------------------------------
%% External exports
-export([start/0]).






%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()->
    spawn(fun()->eunit:test({timeout,1*60,boot_service}) end).

cases_test()->
    ?debugMsg("Test system setup"),
    setup(),
    %% Start application tests
    ?debugMsg("check_loaded_services test"),    
    ?assertEqual(ok,loaded_services()),


    ?debugMsg("test loader"),    
    ?assertEqual(ok,loader_test:start()),

      %% End application tests
  
  %  cleanup(),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
setup()->
    boot_service:boot(),
    ok.
cleanup()->
    init:stop(),
    ok.

loaded_services()->
    ?assertMatch(?LOADED_SERVICES
		,dns_service:all()),  

    ok.

