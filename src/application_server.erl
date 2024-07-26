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
	 load_app/1,
	 start_app/1,
	 stop_app/1,
	 unload_app/1,
	 is_app_loaded/1,
	 is_app_started/1

	 
	]).

-export([

	 all_filenames/0,
	 check_update_repo/0,
	 update/0
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
		application_dir,
		repo_dir,
		git_path
	        
	       }).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Based on info Application file git clone the repo and 
%% extract the tar file  
%% 
%% @end
%%--------------------------------------------------------------------
-spec load_app(Filename :: string()) -> 
	  ok | {error,Reason::term()}.
load_app(Filename) ->
    gen_server:call(?SERVER,{load_app,Filename},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Based on info Application file starts the application. 
%% 
%% @end
%%--------------------------------------------------------------------
-spec start_app(Filename :: string()) -> 
	  ok | {error,Reason::term()}.
start_app(Filename) ->
    gen_server:call(?SERVER,{start_app,Filename},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Based on info Application file stops the application. 
%% 
%% @end
%%--------------------------------------------------------------------
-spec stop_app(Filename :: string()) -> 
	  ok | {error,Reason::term()}.
stop_app(Filename) ->
    gen_server:call(?SERVER,{stop_app,Filename},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Based on info Application file unload the application. 
%% 
%% @end
%%--------------------------------------------------------------------
-spec unload_app(Filename :: string()) -> 
	  ok | {error,Reason::term()}.
unload_app(Filename) ->
    gen_server:call(?SERVER,{unload_app,Filename},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Based on info Application file check if application is loaded. 
%% 
%% @end
%%--------------------------------------------------------------------
-spec is_app_loaded(Filename :: string()) -> 
	  IsLoaded::boolean()| {error,Reason::term()}.
is_app_loaded(Filename) ->
    gen_server:call(?SERVER,{is_app_loaded,Filename},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Based on info Application file check if application is started. 
%% 
%% @end
%%--------------------------------------------------------------------
-spec is_app_started(Filename :: string()) -> 
	  IsStarted::boolean()| {error,Reason::term()}.
is_app_started(Filename) ->
    gen_server:call(?SERVER,{is_app_started,Filename},infinity).

%%--------------------------------------------------------------------
%% @doc
%% This is the recurring function that checks if the repo needs to be 
%% updated. If the repo 
%%     - doesnt exists -> a git clone
%%     - exists but behind main branch -> pull
%%     - sync with main branch -> no action
%%
%% @end
%%--------------------------------------------------------------------
-spec check_update_repo() -> ok.

check_update_repo() ->
    gen_server:cast(?SERVER,{check_update_repo}).


%%--------------------------------------------------------------------
%% @doc
%% Reads the filenames in the RepoDir   
%% 
%% @end
%%--------------------------------------------------------------------
-spec all_filenames() -> 
	  {ok,FileNames::term()} | {error,Reason :: term()}.

all_filenames() ->
    gen_server:call(?SERVER,{all_filenames},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec update() -> ok | Error::term().
update()-> 
    gen_server:call(?SERVER, {update},infinity).



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
	    repo_dir=?RepoDir,
	    git_path=?RepoGit
	    
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


handle_call({load_app,Filename}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try lib_application:load_app(RepoDir,Filename) of
	       ok->
		   ok;
	       Error->
		   Error
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		  ok;
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply,State};

handle_call({start_app,Filename}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try lib_application:start_app(RepoDir,Filename) of
	       ok->
		   ok;
	       Error->
		   Error
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		  ok;
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply,State};

handle_call({stop_app,Filename}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try lib_application:stop_app(RepoDir,Filename) of
	      ok->
		   ok;
	       Error->
		   Error
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		  ok;
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply,State};

handle_call({unload_app,Filename}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try lib_application:unload_app(RepoDir,Filename) of
	       ok->
		   ok;
	       Error->
		   Error
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok ->
		  ok;
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply,State};

handle_call({is_app_loaded,Filename}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try lib_application:is_app_loaded(RepoDir,Filename) of
	       true->
		   true;
	       false->
		   false
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      true->
		  true;
	      false->
		  false;
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply,State};

handle_call({is_app_started,Filename}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try lib_application:is_app_started(RepoDir,Filename) of
	       true->
		   true;
	       false->
		   false
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      true->
		  true;
	      false->
		  false;
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply,State};


handle_call({all_filenames}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try git_handler:all_filenames(RepoDir) of
	       {ok,R}->
		    {ok,R};
	       Error->
		   Error
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,AllFileNames}->
		  {ok,AllFileNames};
	      ErrorEvent->
		% io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  ErrorEvent
	  end,
    {reply, Reply,State};

handle_call({read_file,FileName}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try git_handler:read_file(RepoDir,FileName) of
	       {ok,R}->
		    {ok,R};
	       Error->
		   Error
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,Info}->
		  {ok,Info};
	      ErrorEvent->
		 ErrorEvent
	  end,
    {reply, Reply,State};

handle_call({update}, _From, State) ->
    RepoDir=State#state.repo_dir,
    GitPath=State#state.git_path,    
    Reply=lib_application:update(RepoDir,GitPath),
    {reply, Reply, State};


handle_call({update_repo_dir,RepoDir}, _From, State) ->
    NewState=State#state{repo_dir=RepoDir},
    Reply=ok,
    {reply, Reply, NewState};

handle_call({update_git_path,GitPath}, _From, State) ->
    NewState=State#state{git_path=GitPath},
    Reply=ok,
    {reply, Reply, NewState};


%%--------------------------------------------------------------------



handle_call({ping}, _From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(UnMatchedSignal, From, State) ->
   ?LOG2_WARNING("Unmatched signal",[UnMatchedSignal]),
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal, From,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,UnMatchedSignal, From]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------


handle_cast({check_update_repo}, State) ->
    RepoDir=State#state.repo_dir,
    GitPath=State#state.git_path,    
    try lib_application:update(RepoDir,GitPath) of
	{ok,"Cloned the repo"}->
	    ?LOG2_NOTICE("Cloned the repo",[]),
	    ?LOG_NOTICE("Cloned the repo",[]);
	{ok,"Pulled a new update of the repo"}->
	    ?LOG2_NOTICE("Pulled a new update of the repo",[]),
	    ?LOG_NOTICE("Pulled a new update of the repo",[]);
	{ok,"Repo is up to date"}->
	    ok
    catch
	Event:Reason:Stacktrace ->
	    {Event,Reason,Stacktrace,?MODULE,?LINE}
    end,
    spawn(fun()->lib_application:timer_to_call_update(?Interval) end),
    {noreply, State};



handle_cast({stop}, State) ->
    
    {stop,normal,ok,State};

handle_cast(UnMatchedSignal, State) ->
    ?LOG2_WARNING("Unmatched signal",[UnMatchedSignal]),
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
    RepoDir=State#state.repo_dir,
    GitPath=State#state.git_path,
   try lib_application:init(RepoDir,GitPath) of
       ok->
	   ok;
       {error,Reason}->
	   ?LOG2_WARNING("Init failed",[Reason]),
	   ?LOG_WARNING("Init failed",[Reason]),
	   {error,Reason}
   catch
       Event:Reason:Stacktrace ->
	   ?LOG2_WARNING("Init failed",[Event,Reason,Stacktrace]),
	   ?LOG_WARNING("Init failed",[Event,Reason,Stacktrace]),
	   {Event,Reason,Stacktrace,?MODULE,?LINE}
   end,
    spawn(fun()->lib_application:timer_to_call_update(?Interval) end),
    ?LOG2_NOTICE("Server started",[?MODULE]),
    ?LOG_NOTICE("Server started ",[?MODULE]),
    {noreply, State};


handle_info(Info, State) ->
    ?LOG2_WARNING("Unmatched signal",[Info]),
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
