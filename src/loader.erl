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
-export([start/1,stop/1
	]).
	 


%% ====================================================================
%% External functions
%% ===================================================================
%% Cases to handled
%% 1. Not running, No files, not loaded, not registered => load and register
%% 2. Running , file, loaded , registered => do nothing 
%% 3. Running , no file, loaded , registered => do nothing 
%% 3. Running , no file, loaded , not registered => registered
%% 4. Not running ,files, loaded, not registered => start and register
%% 5. Not running ,files, not loaded, not registered> start and register
%%
start(ServiceId)->
    CatalogInfo=rpc:call(node(),config_service,get_info,[catalog_info]),

    Result=case lists:keyfind(ServiceId,1,CatalogInfo) of
	       {ServiceId,Type,Source}->
		  CurrentNode=node(),
		   Loaded=[ServiceId||{Application,_,_}<-application:loaded_applications(),
				    list_to_atom(ServiceId)==Application],
		   Running=[ServiceId||{Application,_,_}<-application:which_applications(),
				    list_to_atom(ServiceId)==Application],
		   IsDir=filelib:is_dir(ServiceId),
		   Registered=[Node||Node<-sd_service:fetch_service(ServiceId),
				     Node==CurrentNode],
		   
		   case {Running,Loaded,IsDir,Registered} of
		       {[ServiceId],[ServiceId],true,[CurrentNode]}->
			   {error,[already_loaded,ServiceId]};
		       _->
			   EbinDir=filename:join(ServiceId,"ebin"),
			   stop(ServiceId),
			   case Type of
			       git->
				   os:cmd("git clone "++Source++ServiceId++".git");
			       dir->
				   os:cmd("cp -r "++Source++"/"++ServiceId++" "++".")
			   end,
			   true=code:add_path(EbinDir),
			   ok=application:start(list_to_atom(ServiceId)),
			   sd_service:add_service(ServiceId),
			   {ok,ServiceId}
		   end;
	       []->
		   {error,[eexists, ServiceId,?MODULE,?LINE]}
	   end,
    Result.



stop(ServiceId)->
    EbinDir=filename:join(ServiceId,"ebin"),
    application:stop(list_to_atom(ServiceId)),
    application:unload(list_to_atom(ServiceId)),
    code:del_path(EbinDir),      
    os:cmd("rm -rf "++ServiceId),
    sd_service:remove_service(ServiceId),
   ok.    
