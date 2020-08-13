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
-export([running/1,start/1,stop/1
	]).
	 


%% ====================================================================
%% External functions
%% ===================================================================
running(ServiceId)->
    CurrentNode=node(),
    Loaded=[ServiceId||{Application,_,_}<-application:loaded_applications(),
		       list_to_atom(ServiceId)==Application],
    Running=[ServiceId||{Application,_,_}<-application:which_applications(),
			list_to_atom(ServiceId)==Application],
    IsDir=filelib:is_dir(ServiceId),
    Registered=[Node||Node<-rpc:call(node(),sd_service,fetch_service,[ServiceId]),
		      Node==CurrentNode],
    Result=case {Running,Loaded,IsDir,Registered} of
	       {[ServiceId],[ServiceId],true,[CurrentNode]}->
		   true;
	       _->
		   false
	   end,
    Result.
    
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
		   Registered=[Node||Node<-rpc:call(node(),sd_service,fetch_service,[ServiceId]),
		      Node==CurrentNode],
		   
		   case {Running,Loaded,IsDir,Registered} of
		       {[ServiceId],[ServiceId],true,[CurrentNode]}->
			   {error,[already_loaded,ServiceId]};
		       _->
			   EbinDir=filename:join(ServiceId,"ebin"),
			   stop(ServiceId),
			   case Type of
			       git->
				   os:cmd("git clone "++Source++ServiceId++".git"),
				   %% - Compile
				   os:cmd("rm -rf include"),
				   os:cmd("git clone "++Source++"include"++".git");
			       dir->
				   os:cmd("cp -r "++Source++"/"++ServiceId++" "++"."),
				    %% - Compile
				   os:cmd("rm -rf include"),
				   os:cmd("cp -r "++Source++"/"++"include"++" "++".")
				   
			   end,
			   %% -- Compile
			   compile(ServiceId),
			   %% -- end compile
			   true=code:add_path(EbinDir),
			   ok=application:start(list_to_atom(ServiceId)),
			   rpc:call(node(),sd_service,add_service,[ServiceId]),
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
    rpc:call(node(),sd_service,remove_service,[ServiceId]),
   ok.    


compile(ServiceId)->
    PathSrc=filename:join([ServiceId,"src"]),
    PathEbin=filename:join([ServiceId,"ebin"]),
    
    %Get erl files that shall be compiled
    Result=case rpc:call(node(),file,list_dir,[PathSrc]) of
	       {ok,Files}->
		   FilesToCompile=[filename:join(PathSrc,File)||File<-Files,filename:extension(File)==".erl"],
		   % clean up ebin dir
		   case rpc:call(node(),os,cmd,["rm -rf "++PathEbin++"/*"]) of
		       []->
			   CompileResult=[{rpc:call(node(),c,c,[ErlFile,[{outdir,PathEbin},{i,"include"}]]),ErlFile}||ErlFile<-FilesToCompile],
			   case [{R,File}||{R,File}<-CompileResult,error==R] of
			       []->
				   AppFileSrc=filename:join(PathSrc,ServiceId++".app"),
				   AppFileDest=filename:join(PathEbin,ServiceId++".app"),
				   case rpc:call(node(),os,cmd,["cp "++AppFileSrc++" "++AppFileDest]) of
				       []->
					 %  io:format("~p~n",[{AppFileSrc,AppFileDest,?FILE,?LINE}]),
					   ok;
				       {badrpc,Err} ->
					   {error,[badrpc,node(),ServiceId,Err,?MODULE,?LINE]};
				       Err ->
					   {error,[undefined_error,node(),ServiceId,Err,?MODULE,?LINE]}
				   end;
			       CompilerErrors->
				   {error,[compiler_error,CompilerErrors,?MODULE,?LINE]}
			   end;
		       {badrpc,Err} ->
			   {error,[badrpc,node(),ServiceId,Err,?MODULE,?LINE]};
		       Err ->
			   {error,[undefined_error,node(),ServiceId,Err,?MODULE,?LINE]}
		   end;
	       {badrpc,Err} ->
		   {error,[badrpc,node(),ServiceId,Err,?MODULE,?LINE]};
	       Err ->
		   {error,[undefined_error,node(),ServiceId,Err,?MODULE,?LINE]}
	   end,
    Result.
