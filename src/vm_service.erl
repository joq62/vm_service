%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc : represent a logical vm  
%%% 
%%% Supports the system with standard erlang vm functionality, load and start
%%% of an erlang application (downloaded from git hub) and "dns" support 
%%% 
%%% Make and start the board start SW.
%%%  boot_service initiates tcp_server and l0isten on port
%%%  Then it's standby and waits for controller to detect the board and start to load applications
%%% 
%%%     
%%% -------------------------------------------------------------------
-module(vm_service). 

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.hrl").
-include("timeout.hrl").
-include("config.hrl").
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{service_list}).


	  
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================

-export([get_instance/1,
	update_service_list/1]).

-export([]).
%% server interface
-export([boot/0,
	 start_service/1,
	 start_service/3,
	 stop_service/1 
	]).




-export([ping/0,
	 heart_beat/1,
	 start/0,
	 stop/0
	 ]).
%% internal 
%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

boot()->
    application:start(?MODULE).

%% Asynchrounus Signals
%boot_strap()->
 %   PortStr=atom_to_list(PortArg),
 %   Port=list_to_integer(PortStr),
   % application:set_env([{boot_service,{port,Port}}]),
%    application:start(boot_service).
	
%% Gen server function

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


%%----------------------------------------------------------------------
ping()->
    gen_server:call(?MODULE,{ping},infinity).

get_instance(ServiceId)->
    gen_server:call(?MODULE,{get_instance,ServiceId},infinity).

start_service(ServiceId)->    
    gen_server:call(?MODULE,{start_service,ServiceId},infinity).
start_service(ServiceId,Type,Source)->    
    gen_server:call(?MODULE,{start_service,ServiceId,Type,Source},infinity).
stop_service(ServiceId)->    
    gen_server:call(?MODULE,{stop_service,ServiceId},infinity).

%%___________________________________________________________________
heart_beat(Interval)->
    gen_server:cast(?MODULE, {heart_beat,Interval}).

update_service_list(ServiceList)->
    gen_server:cast(?MODULE, {update_service_list,ServiceList}).

%%-----------------------------------------------------------------------


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------
init([]) ->
    case application:get_all_env() of
	[]->
	    ok;
	Env->
	    {services,ServicesAtom}=lists:keyfind(services,1,Env),
	    ServiceIdList=string:tokens(atom_to_list(ServicesAtom),"X"),
	    [application:start(list_to_atom(ServiceId))||ServiceId<-ServiceIdList],
	    sys:log(log_service,{true,?LOG_BUFFER})
    end,
    ?LOG_INFO(event,{?MODULE,'started'}),
    spawn(fun()->heart_beat(?VM_HEARTBEAT) end),
    {ok, #state{}}.
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------
handle_call({start_service,ServiceId}, _From, State) ->
    Reply=case file:consult(filename:join(?CATALOG_CONFIG_DIR,?CATALOG_CONFIG_FILE)) of
	      {ok,Info}->
		  case lists:keyfind(ServiceId,1,Info) of
		      {ServiceId,Type,Source}->
			  rpc:call(node(),loader,start,[ServiceId,Type,Source],5000);
		      []->
			  {error,[eexists, ServiceId,?MODULE,?LINE]}
		  end;
	      Err->
		  {error,[Err,?MODULE,?LINE]}
	  end,
    {reply, Reply, State};

handle_call({start_service,ServiceId,Type,Source}, _From, State) ->
    Reply=rpc:call(node(),loader,start,[ServiceId,Type,Source],5000),
    {reply, Reply, State};
handle_call({stop_service,ServiceId}, _From, State) ->
    Reply=loader:stop(ServiceId),
    {reply, Reply, State};

handle_call({get_instance,all}, _From, State) ->
    Reply=State#state.service_list,
    {reply, Reply, State};

handle_call({get_instance,ServiceId}, _From, State) ->
     Reply=dns:get(ServiceId,State#state.service_list),
    {reply, Reply, State};


handle_call({ping}, _From, State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,?LINE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({heart_beat,Interval}, State) ->
    spawn(fun()->h_beat(Interval) end),    
    {noreply, State};

handle_cast({update_service_list,ServiceList}, State) ->
    NewState=State#state{service_list=ServiceList},
    {noreply, NewState};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)

handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
h_beat(Interval)->
    dns:update_info(?CATALOG_CONFIG_URL,?CATALOG_CONFIG_DIR,?CATALOG_CONFIG_FILE),
    ServiceList=dns:update(),
    vm_service:update_service_list(ServiceList),
    timer:sleep(Interval),
    rpc:cast(node(),?MODULE,heart_beat,[Interval]).


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
