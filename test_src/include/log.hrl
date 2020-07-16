%-define(LOG_INFO(Type,Msg),log_service:msg({Type,[node(),?MODULE,?FILE,?LINE,date(),time(),Msg]})).


-define(LOG_BUFFER,512).

-record(log,{
	  log_node,
	  type,
	  node,
	  module,
	  file,
	  line,
	  date,
	  time,
	  msg}).
-define(LOG_INFO(Type,Msg),log_service:msg(#log{type=Type,node=node(),module=?MODULE,file=?FILE,line=?LINE,
						date=date(),time=time(),msg=Msg})).
