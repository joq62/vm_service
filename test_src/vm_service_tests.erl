%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Create1d : 10 dec 2012
%%% -------------------------------------------------------------------
-module(vm_service_tests). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").




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
    spawn(fun()->eunit:test({timeout,1*60,vm_service}) end).

cases_test()->
    ?debugMsg("Test system setup"),
    setup(),
    %% Start application tests

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
    ?assertEqual(ok,application:start(vm_service)),
    ?assertMatch({pong,_,_},log_service:ping()),    
    ?assertMatch({pong,_,_},config_service:ping()),  
    ?assertMatch({pong,_,_},sd_service:ping()),  
    timer:sleep(500),
    ok.
cleanup()->
    init:stop(),
    ok.
