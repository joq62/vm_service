%%% -------------------------------------------------------------------
%%% @author  : uabjle
%%% @doc : support functions boot_service
%%% -------------------------------------------------------------------
-module(loader).
  


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-export([start/3,stop/1
	]).
	 


%% ====================================================================
%% External functions
%% ====================================================================
start(ServiceId,git,GitUrl)->
    Result=case [ServiceId||{Application,_,_}<-application:loaded_applications(),
			    list_to_atom(ServiceId)==Application] of
	       []->
		   EbinDir=filename:join(ServiceId,"ebin"),
		   stop(ServiceId),
		   os:cmd("git clone "++GitUrl++ServiceId++".git"),
		   true=code:add_path(EbinDir),
		   ok=application:start(list_to_atom(ServiceId)),
		   {ok,ServiceId};
	       Err ->
		   {error,Err}
	   end,
    Result.
 
stop(ServiceId)->
    EbinDir=filename:join(ServiceId,"ebin"),
    application:stop(list_to_atom(ServiceId)),
    application:unload(list_to_atom(ServiceId)),
    code:del_path(EbinDir),      
    os:cmd("rm -rf "++ServiceId),
    ok.    
