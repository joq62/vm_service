%%% -------------------------------------------------------------------
%%% @author : joqerlang
%%% @doc : ets dbase for master service to manage app info , catalog  
%%%
%%% -------------------------------------------------------------------
-module(dns).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("config.hrl").
-include("log.hrl").
%-compile(export_all).
-export([update_info/3,all/1,get/2,update/0,add/3,delete/3]).

%% ====================================================================
%% External functions
%% ====================================================================

update_info(GitUrl,Dir,FileName)->
    os:cmd("rm -rf "++Dir),
    os:cmd("git clone "++GitUrl),
    timer:sleep(100),
    {R,Info}=file:consult(filename:join(Dir,FileName)),
    {R,Info}.

%% --------------------------------------------------------------------
%% 
%% 
%% {"master_sthlm_1",'master_sthlm_1@asus'}
%% --------------------------------------------------------------------
%% @doc: get(ServiceId) returns of a list of nodes that have ServiceId all running applications

-spec(all(DnsInfo::[{ServiceId::string(),Node::atom()}])->[{ServiceId::string(),Node::atom()}]|[]).
all(DnsInfo)->
    DnsInfo.


%% @doc: get(ServiceId) returns of a list of nodes that have ServiceId all running applications
-spec(get(ServiceId::string(),DnsInfo::[{ServiceId::string(),Node::atom()}])->[{ServiceId::string(),Node::atom()}]|[]).
get(WantedServiceId,ServiceList)->
    ActiveServices=[Node||{ServiceId,Node}<-ServiceList,
				      WantedServiceId==ServiceId],
    ActiveServices.

%% @doc: update(Catalog) update the dns list

-spec(update()->[{ServiceId::string(),Node::atom()}]| []).
update()->
    {ok,NodeConfig}=file:consult(filename:join(?NODE_CONFIG_DIR,?NODE_CONFIG_FILE)),
    {ok,CatalogConfig}=file:consult(filename:join(?CATALOG_CONFIG_DIR,?CATALOG_CONFIG_FILE)),
    R1=[{net_kernel:connect_node(Node),Node}||{_,Node}<-NodeConfig],
    R2=[Node||{true,Node}<-R1],
    L1=[{rpc:call(Node,application,which_applications,[]),Node}||Node<-R2],
    
    AllApplications=lists:append([create_list(AppInfo,Node,[])||{AppInfo,Node}<-L1]),
    ServiceList=[{ServiceId,Node}||{ServiceId,Node}<-AllApplications,
				   lists:keymember(ServiceId,1,CatalogConfig)],
    ServiceList.


create_list([],_Node,ServiceList)->
    ServiceList;
create_list([{Service,_Desc,_Vsn}|T],Node,Acc)->
    NewAcc=[{atom_to_list(Service),Node}|Acc],
    create_list(T,Node,NewAcc);
create_list([{badrpc,Err}|T],Node,Acc) ->
    ?LOG_INFO(error,[Node,badrpc,Err]),
    create_list(T,Node,Acc).
 

%% @doc: add(ServiceId,Node,DnsInfo) -> New DnsInfo list

-spec(add(ServiceId::string(),Node::atom(),DnsInfo::[tuple()])->DnsInfo::[tuple()]|[]).
add(ServiceId,Node,DnsInfo)->
    Removed=[{S1,N1}||{S1,N1}<-DnsInfo,
		      {ServiceId,Node}/={S1,N1}],
    NewDnsInfo=[{ServiceId,Node}|Removed],
    {ok,NewDnsInfo}.


%% @doc:delete(ServiceId,Node,DnsInfo) -> New DnsInfo list

-spec(delete(ServiceId::string(),Node::atom(),DnsInfo::[tuple()])->DnsInfo::[tuple()]|[]).
delete(ServiceId,Node,DnsInfo)->
    NewDnsInfo=[{S1,N1}||{S1,N1}<-DnsInfo,
		      {ServiceId,Node}/={S1,N1}],
    {ok,NewDnsInfo}.
