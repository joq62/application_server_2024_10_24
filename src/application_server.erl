%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%% 
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(application_server). 
 
-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").

-include("application.hrl").
-include("catalog.rd").


%% API

-export([
	 start_node/1,
	 stop_node/1,
	 load/1,
	 start/1,
	 stop/1,
	 unload/1
	 
	]).
%% admin




-export([
	 start/0,
	 ping/0,
	 stop/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
		     
-record(state, {
		specs_dir,
		application_maps
	        
	       }).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% 
%% @end
%%--------------------------------------------------------------------
-spec start_node(Filename :: string()) -> 
	  {ok,Node::node()} | {error,Reason::term()}.
start_node(Filename) ->
    gen_server:call(?SERVER,{start_node,Filename},infinity).

%%--------------------------------------------------------------------
%% @doc
%%  
%%  
%% 
%% @end
%%--------------------------------------------------------------------
-spec stop_node(Filename :: string()) -> 
	  ok | {error,Reason::term()}.
stop_node(Filename) ->
    gen_server:call(?SERVER,{stop_node,Filename},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
-spec load(ApplicationMap::map()) -> 
	  {ok,ApplicationMap2::map()} | {error,Reason::term()}.
load(ApplicationMap) ->
    gen_server:call(?SERVER,{load,ApplicationMap},infinity).


%%--------------------------------------------------------------------
%% @doc
%% 
%% 
%% 
%% @end
%%--------------------------------------------------------------------
-spec start(ApplicationMap::map()) -> 
	  {ok,ApplicationMap2::map()} | {error,Reason::term()}.
start(ApplicationMap) ->
    gen_server:call(?SERVER,{start,ApplicationMap},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Based on info Application file git clone the repo and 
%% extract the tar file  
%% 
%% @end
%%--------------------------------------------------------------------
-spec stop(ApplicationMap::map()) -> 
	  {ok,ApplicationMap2::map()} | {error,Reason::term()}.
stop(ApplicationMap) ->
    gen_server:call(?SERVER,{stop,ApplicationMap},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Based on info Application file git clone the repo and 
%% extract the tar file  
%% 
%% @end
%%--------------------------------------------------------------------
-spec unload(ApplicationMap::map()) -> 
	  {ok,ApplicationMap2::map()} | {error,Reason::term()}.
unload(ApplicationMap) ->
    gen_server:call(?SERVER,{unload,ApplicationMap},infinity).


%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec ping() -> pong | Error::term().
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%stop()-> gen_server:cast(?SERVER, {stop}).
stop()-> gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.

init([]) ->
    
    {ok, #state{
	    specs_dir=?SpecsDir,
	    application_maps=[]
	    
	   },0}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.

handle_call({start_node,Filename}, _From, State) ->
    SpecFile=filename:join(State#state.specs_dir,Filename),
    Reply=case lib_application:start_node(SpecFile,State#state.application_maps) of
	      {ok,Node,NewApplcationMaps}->
		  NewState=State#state{application_maps=NewApplcationMaps},
		  {ok,Node};
	      {error,Reason}->
		  NewState=State,
		  {error,Reason}
	  end,
    {reply, Reply,NewState};


handle_call({stop_node,Filename}, _From, State) ->
    SpecFile=filename:join(State#state.specs_dir,Filename),
    Reply=case lib_application:stop_node(SpecFile,State#state.application_maps) of
	      {ok,NewApplcationMaps}->
		  NewState=State#state{application_maps=NewApplcationMaps},
		  ok;
	      {error,Reason}->
		  NewState=State,
		  {error,Reason}
	  end,
    {reply, Reply,NewState};



%%--------------------------------------------------------------------



handle_call({ping}, _From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(UnMatchedSignal, From, State) ->
   ?LOG_WARNING("Unmatched signal",[UnMatchedSignal]),
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal, From,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,UnMatchedSignal, From]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------


handle_cast({stop}, State) ->
    
    {stop,normal,ok,State};

handle_cast(UnMatchedSignal, State) ->
    ?LOG_WARNING("Unmatched signal",[UnMatchedSignal]),
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(timeout, State) ->
  
    ?LOG_NOTICE("Server started ",[?MODULE]),
    {noreply, State};


handle_info(Info, State) ->
    ?LOG_WARNING("Unmatched signal",[Info]),
    io:format("unmatched_signal ~p~n",[{Info,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
