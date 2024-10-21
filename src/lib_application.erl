%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_application).

-include("log.api").  
-include("application.hrl").

 
%% API

-export([
	 
	 get_application_dirs/1,
	  get_mnesia_dirs/1,

	 load_start/2,
	 stop_unload/2,
	 get_wanted_applications/1,	 
	 get_active_applications/1,

	 start_node/2,
	 stop_node/2,
	 load/2,
	 unload/2,
	 start/2,
	 stop/2
	]).

-export([
	 init/2,
	 update/2,
	 timer_to_call_update/1
	]).

-export([

	

	]).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
get_application_dirs(Files)->
    [File||File<-Files,
	   filelib:is_dir(File),
	   "container"=:=lists:last(string:lexemes(File,"_"))].


%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
get_mnesia_dirs(Files)->
    [File||File<-Files,
	   filelib:is_dir(File),
	   "Mnesia"=:=lists:nth(1,string:lexemes(File,"."))].

    
    



%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
load_start(SpecFile,ApplicationMaps)->
    {ok,Node,ApplicationMaps_1}=start_node(SpecFile,ApplicationMaps),
    pong=net_adm:ping(Node),
    {ok,ApplicationMaps_2}=load(SpecFile,ApplicationMaps_1),   
    {ok,ApplicationMaps_3}=start(SpecFile,ApplicationMaps_2),
    {ok,Node,ApplicationMaps_3}.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
stop_unload(SpecFile,ApplicationMaps)->
    {ok,ApplicationMaps_1}=stop(SpecFile,ApplicationMaps),
    {ok,ApplicationMaps_2}=unload(SpecFile,ApplicationMaps_1),
    {ok,ApplicationMaps_3}=stop_node(SpecFile,ApplicationMaps_2),
    {stopped,ApplicationMaps_3}.


%%--------------------------------------------------------------------
%% @doc
%% 
%%   
%%   
%% @end
%%--------------------------------------------------------------------
get_wanted_applications(SpecDir)->
    {ok,Files}=file:list_dir(SpecDir),
    FilesPath=[filename:join(SpecDir,SpecFile)||SpecFile<-Files],
    L1=[{filename:basename(SpecFile),file:consult(SpecFile)}||SpecFile<-FilesPath,
							      ?FileExt=:=filename:extension(SpecFile)],
    L2=
    {ok,Host}=net:gethostname(), 
    WantedFiles=[File||{File,{ok,[Map]}}<-L1,
	   Host=:=maps:get(host,Map)],
    {ok,WantedFiles}.	     
    
    
%%--------------------------------------------------------------------
%% @doc
%% 
%%   
%%   
%% @end
%%--------------------------------------------------------------------
get_active_applications(ApplicationMaps)->
    File_Nodes_Apps=[{maps:get(filename,Map),maps:get(node,Map),maps:get(app,Map)}||Map<-ApplicationMaps,
							started=:=maps:get(status,Map)],
    Active=[File||{File,Node,App}<-File_Nodes_Apps,
		  pong=:=net_adm:ping(Node),
		  pong=:=rpc:call(Node,App,ping,[],5000)],
    {ok,Active}.     
    
    
    
%%--------------------------------------------------------------------
%% @doc
%% 
%%   
%%   
%% @end
%%--------------------------------------------------------------------
start(SpecFile,ApplicationMaps)->
    case get_application_map(SpecFile,ApplicationMaps) of
	undefined->
	    {error,["Node not started"]};
	{ok,Map}->
	    case is_loaded(Map) of
		false->
		    {error,["Not loaded",SpecFile]};
		true->
		    % case is_started(SpecFile,ApplicationMaps) of
		  
		 %% Add paths
		    Libs=maps:get(libs,Map), 
		    ApplicationDir=maps:get(application_dir,Map),
		    Node=maps:get(node,Map),
		    ApplLibs=filename:join(ApplicationDir,Libs),
		    {ok,Applications}=file:list_dir(ApplLibs),
		    ApplicationList=[filename:join(ApplLibs,Application)||Application<-Applications],
		    ok=add_path(ApplicationList,Node),
		    %% Start applications
		    ApplicationsToStart=maps:get(apps_to_start,Map), 
		    StartResult=[{App,rpc:call(Node,application,start,[App],5000)}||App<-ApplicationsToStart],
		    case [{App,R}||{App,R}<-StartResult,ok=/=R] of
			[]->
			    Map1=maps:update(status,started,Map),
			    {ok,[Map1|lists:delete(Map,ApplicationMaps)]};
			Error->
			    {error,["Failed to start ",SpecFile,Error]}
		    end
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% 
%%   
%%   
%% @end
%%--------------------------------------------------------------------
stop(SpecFile,ApplicationMaps)->
    case get_application_map(SpecFile,ApplicationMaps) of
	undefined->
	    {error,["Node not started"]};
	{ok,Map}->
	    case is_loaded(Map) of
		false->
		    {error,["Not loaded",SpecFile]};
		true->
		    Node=maps:get(node,Map),
		    ApplicationsToStart=maps:get(apps_to_start,Map), 
		    StopResult=[{App,rpc:call(Node,application,stop,[App],5000)}||App<-ApplicationsToStart],
		    case [{App,R}||{App,R}<-StopResult,ok=/=R] of
			[]->
			    Map1=maps:update(status,stopped,Map),
			    {ok,[Map1|lists:delete(Map,ApplicationMaps)]};
			Error->
			    {error,["Failed to stop ",SpecFile,Error]}
		    end
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% 
%%   
%%   
%% @end
%%--------------------------------------------------------------------
add_path([],_Vm)->
    ok;
add_path([ApplicationRoot|T],Vm)->
    {ok,SubDirs}=file:list_dir(ApplicationRoot),
    SubDirsPaths=[filename:join(ApplicationRoot,SubDir)||SubDir<-SubDirs],
    rpc:call(Vm,code,add_paths,[SubDirsPaths],5000),
    add_path(T,Vm).

%%--------------------------------------------------------------------
%% @doc
%% 
%%   
%%   
%% @end
%%--------------------------------------------------------------------
load(SpecFile,ApplicationMaps)->
    case get_application_map(SpecFile,ApplicationMaps) of
	undefined->
	    {error,["Node not started"]};
	{ok,Map}->
	    case is_loaded(Map) of
		true->
		    {error,["Allready loaded",SpecFile]};
		false->
		     %%Clone
		    ApplicationDir=maps:get(application_dir,Map),
		    GitUrl=maps:get(giturl,Map),    
		    os:cmd("git clone "++GitUrl++" "++ApplicationDir),
		    %%Compile
		    {ok,Root}=file:get_cwd(),
		    ok=file:set_cwd(ApplicationDir),
		    Rebar3CompileResult=os:cmd("rebar3 compile"),  
		    ?LOG_NOTICE("rebar3 compile result",[Rebar3CompileResult,SpecFile]),
		    ok=file:set_cwd(Root),
		    case is_loaded(Map) of
			false->
			    {error,["Failed to compile",SpecFile]};
			true ->
			    Map1=maps:update(status,loaded,Map),
			    {ok,[Map1|lists:delete(Map,ApplicationMaps)]}
		    end
	    end
    end.
%%--------------------------------------------------------------------
%% @doc
%% 
%%   
%%   
%% @end
%%--------------------------------------------------------------------
unload(SpecFile,ApplicationMaps)->
    case get_application_map(SpecFile,ApplicationMaps) of
	undefined->
	    {error,["Node not started"]};
	{ok,Map}->
	    case is_loaded(Map) of
		false->
		    {error,["Not loaded",SpecFile]};
		true->
		    ApplicationDir=maps:get(application_dir,Map),
		    {ok,Files}=file:list_dir(ApplicationDir),
		    [file:del_dir_r(filename:join(ApplicationDir,File))||File<-Files],
		    case is_loaded(Map) of
			true->
			    {error,["Failed to unload",SpecFile]};
			false ->
			    Map1=maps:update(status,unloaded,Map),
			    {ok,[Map1|lists:delete(Map,ApplicationMaps)]}
		    end
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% 
%%   
%%   
%% @end
%%--------------------------------------------------------------------
is_loaded(Map)->
    Buildir=maps:get(build_dir,Map),
    ApplicationDir=maps:get(application_dir,Map),
    BuilDirPath=filename:join(ApplicationDir,Buildir),
    filelib:is_dir(BuilDirPath).
	    
%%--------------------------------------------------------------------
%% @doc
%% 
%%   
%%   
%% @end
%%--------------------------------------------------------------------
start_node(SpecFile,ApplicationMaps)->
    Result=case is_node_started(SpecFile,ApplicationMaps) of
	       true->
		   {error,["Allready started"]};
	       false->
		   {ok,[Map]}=file:consult(SpecFile),
		   {ok,Host}=net:gethostname(),
		   %% Create dir
		   UniqueNum=integer_to_list(os:system_time(millisecond),36),
		   ApplName=maps:get(application_name,Map),
		   Vsn=maps:get(vsn,Map), 
		   [Major,Minor,Patch]=string:lexemes(Vsn,"."),
		   ApplicationDir=UniqueNum++"_"++ApplName++"_"++Major++Minor++Patch++"_"++?DirExt,
		   ok=file:make_dir(ApplicationDir),
		   NodeName=UniqueNum++"_"++ApplName++"_"++Major++Minor++Patch,
		   Node=list_to_atom(NodeName++"@"++Host),
		   CookieStr=atom_to_list(erlang:get_cookie()),
		   {ok,Node}=slave:start(Host,NodeName," -setcookie "++CookieStr),
		   pong=net_adm:ping(Node),
		   Map0=maps:update(application_dir,ApplicationDir,Map),
		   Map1=maps:update(nodename,NodeName,Map0),
		   Map2=maps:update(node,Node,Map1),
		   Map3=maps:update(status,node_started,Map2),
		   Map4=maps:update(created,{date(),time()},Map3),
		   {ok,Node,[Map4|ApplicationMaps]}
	   end,
    Result.
    
    %%--------------------------------------------------------------------
%% @doc
%% 
%%   
%%   
%% @end
%%--------------------------------------------------------------------
stop_node(SpecFile,ApplicationMaps)->
    Result=case is_node_started(SpecFile,ApplicationMaps) of
	       false->
		   {error,["Not started"]};
	       true->
		   case get_application_map(SpecFile,ApplicationMaps) of
		       undefined->
			   {error,["Undefined"]};
		       {ok,Map}->
			   Node=maps:get(node,Map),
			   slave:stop(Node),
			   ApplicationDir=maps:get(application_dir,Map), 
			   file:del_dir_r(ApplicationDir),
			   {ok,lists:delete(Map,ApplicationMaps)}
		   end
	   end,
    Result.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_node_started(SpecFile,ApplicationMaps)->
    case get_application_map(SpecFile,ApplicationMaps) of
	undefined->
	    false;
	{ok,_Map}->
	    true
    end.
    
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_application_map(SpecFile,ApplicationMaps)->
    Filename=filename:basename(SpecFile),
    case [Map||Map<-ApplicationMaps,
	       Filename=:=maps:get(filename,Map)] of
	[]->
	    undefined;
	[Map] ->
	    {ok,Map}
    end.
	   




%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
timer_to_call_update(Interval)->
    timer:sleep(Interval),
    rpc:cast(node(),application_server,check_update_repo,[]).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%-------------------------------------------------------------------
update(RepoDir,GitPath)->
    Result=case git_handler:is_repo_updated(RepoDir) of
	       {error,["RepoDir doesnt exists, need to clone"]}->
		   ok=git_handler:clone(RepoDir,GitPath),
		   {ok,"Cloned the repo"};
	       false ->
		   ok=git_handler:update_repo(RepoDir),
		   {ok,"Pulled a new update of the repo"};
	       true ->
		   {ok,"Repo is up to date"}
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
init(RepoDir,GitPath)->
    case git_handler:is_repo_updated(RepoDir) of
	{error,["RepoDir doesnt exists, need to clone"]}->
	    ok=git_handler:clone(RepoDir,GitPath);
	false ->
	    ok=git_handler:update_repo(RepoDir);
	true ->
	    ok
    end,
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
