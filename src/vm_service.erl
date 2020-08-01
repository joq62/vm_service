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
%-include("timeout.hrl").
%-include("config.hrl").
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{}).


	  
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================

-export([]).

-export([]).
%% server interface
-export([boot/0,
	 start_service/1,
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

start_service(ServiceId)->    
    gen_server:call(?MODULE,{start_service,ServiceId},infinity).
stop_service(ServiceId)->    
    gen_server:call(?MODULE,{stop_service,ServiceId},infinity).

%%___________________________________________________________________
heart_beat(Interval)->
    gen_server:cast(?MODULE, {heart_beat,Interval}).


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
    
    {ok,HbInterval}= application:get_env(hb_interval),
    {ok,AppsToStart}=application:get_env(apps_to_start),
    StartResult=[{application:start(App),atom_to_list(App)}||App<-AppsToStart],
    sys:log(log_service,{true,?LOG_BUFFER}),
    case [{R,ServiceId}||{R,ServiceId}<-StartResult,R/=ok] of 
	[]->
	    [rpc:call(node(),sd_service,add_service,[ServiceId])||{ok,ServiceId}<-[{ok,"vm_service"}|StartResult]];
	Err->
	    ?LOG_INFO(error,{Err})
    end,
	    
    ?LOG_INFO(event,{?MODULE,'started'}),
    spawn(fun()->heart_beat(HbInterval) end),
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
    Reply=rpc:call(node(),loader,start,[ServiceId]),
    {reply, Reply, State};

handle_call({stop_service,ServiceId}, _From, State) ->
    Reply=loader:stop(ServiceId),
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

handle_cast({heart_beat,HbInterval}, State) ->
    spawn(fun()->h_beat(HbInterval) end),    
    {noreply, State};

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
h_beat(HbInterval)->
   % [rpc:call(node(),loader,start,[ServiceId])||{ServiceId,_}<-sd_service:fetch_all(local_services)],
    timer:sleep(HbInterval),
    case rpc:call(node(),orchistrate,simple_campaign,[],15000) of
	{[],[],[]}->
	    ok;
	Info->
	    io:format("Campaign result Missing ,Obsolite, FailedToStart ~p~n",[{?MODULE,?LINE,Info}])
    end,
%    ok=rpc:call(node(),sd_service,trade_services,[]),

    rpc:cast(node(),?MODULE,heart_beat,[HbInterval]).


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
