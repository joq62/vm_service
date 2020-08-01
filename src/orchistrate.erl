%%% -------------------------------------------------------------------
%%% @author : joqerlang
%%% @doc : ets dbase for master service to manage app info , catalog  
%%%
%%% -------------------------------------------------------------------
-module(orchistrate).
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.hrl").

%-compile(export_all).
-export([simple_campaign/0]).




%% ====================================================================
%% External functions
%% ====================================================================


%% --------------------------------------------------------------------
%% 
%%
%% --------------------------------------------------------------------
%% 1 Check missing services - try to start them
simple_campaign()->
    AppInfo=config_service:get_info(app_info),
    CurrentNode=node(),
    Locals=[{ServiceId,Node}||{ServiceId,Node}<-AppInfo,
			      Node==CurrentNode],
    RunningServices=[{rpc:call(node(),loader,running,[ServiceId]),ServiceId}
		     ||{ServiceId,_Node}<-Locals],
    _StartedServices=[rpc:call(node(),loader,start,[ServiceId])||{R,ServiceId}<-RunningServices,
								R==true],
%    io:format("StartedServices ~p~n",[{?MODULE,?LINE,StartedServices}]),
    LocalsRunning=sd_service:fetch_all(local_services),
%   io:format( "LocalsRunning~p~n",[{?MODULE,?LINE,LocalsRunning}]),
    Missing=[{ServiceId,Node}||{ServiceId,Node}<-Locals,
			       false==lists:member({ServiceId,Node},LocalsRunning)],
%    io:format("Missing ~p~n",[{?MODULE,?LINE,Missing}]),
    case Missing of
	[]->
	    FailedStarts=[],
	    ok;
	Missing->
	    ?LOG_INFO(event,['Missing services ',Missing]),
	    % io:format("StartInfoMissing ~p~n",[{?MODULE,?LINE,StartInfoMissing}]),
	    StartResult=[rpc:call(node(),loader,start,[ServiceId])||{ServiceId,_Node}<-Missing],
	    %io:format("StartResult ~p~n",[{?MODULE,?LINE,StartResult}]),
	    FailedStarts=[{Node,{error,Err}}||{Node,{error,Err}}<-StartResult],
	    case FailedStarts of
		[]->
		    ok;
		 FailedStarts->
		     %io:format("FailedStarts ~p~n",[{?MODULE,?LINE,FailedStarts}]),
		    ?LOG_INFO(error,['failed to start',FailedStarts])
	    end
    end,
    
    Obsolite=[{ServiceId,Node}||{ServiceId,Node}<-LocalsRunning,
			    false==lists:member({ServiceId,Node},Locals)],	
 %   io:format("Obsolite ~p~n",[{?MODULE,?LINE,Obsolite}]),
    case Obsolite of
	[]->
	    ok;
	Obsolite->
	    ?LOG_INFO(event,['obsolite services',Obsolite]),
	    [rpc:call(node(),loader,stop,[ServiceId])||{ServiceId,_Node}<-Obsolite]
    end,

    {Missing,Obsolite,FailedStarts}.
