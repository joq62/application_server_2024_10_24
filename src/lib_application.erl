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

-define(NumTries,500).
-define(SleepInterval,20).
 
%% API

-export([
	 start_node/2,
	 stop_node/2,
	 load/2,
	 unload/2,
	 start/2
%	 stop/2
	]).

-export([
	 init/2,
	 update/2,
	 timer_to_call_update/1
	]).

-export([

	 load_rel/2,
	 start_rel/2,
	 stop_rel/2,
	 unload_rel/2,
	 is_rel_loaded/2,
	 is_rel_started/2

	]).



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
add_path([],Vm)->
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
		    os:cmd("rebar3 compile"),  
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
%% Data:
%%   GitDir= git repo dir name where the files are cloned into.
%%   Code ok:2024-09-17
%% @end
%%--------------------------------------------------------------------
load_rel(CatalogDir,FileName)->
    Result=case is_rel_loaded(CatalogDir,FileName) of
	       true->
		   {error,["Allready loaded ",FileName]};
	       false ->
		   {ok,Cwd}=file:get_cwd(),
		   case git_handler:read_file(CatalogDir,FileName) of
		       {error,Reason}->
			   {error,Reason};
		       {ok,[Info]}->
			   AppGitDir=maps:get(application_git_dir,Info),
			   ApplicationDir=filename:join(Cwd,AppGitDir),
			   file:del_dir_r(ApplicationDir),
			   GitUrl=maps:get(giturl,Info),
			   CloneResult=compiler_server:git_clone(GitUrl,ApplicationDir),
			   ?LOG_NOTICE("CloneResult ",[CloneResult]),
			   CompileResult=compiler_server:compile(ApplicationDir),
			   ?LOG_NOTICE("CompileResult ",[CompileResult]),
			   ReleaseResult=compiler_server:release(ApplicationDir),
			   ?LOG_NOTICE("ReleaseResult ",[ReleaseResult]),
			   case is_rel_loaded(CatalogDir,FileName) of
			       true->
				   ok;
			       false->
				   {error,["Failed to load rel "]}
			   end
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
start_rel(CatalogDir,FileName)->
    
    Result=case is_rel_started(CatalogDir,FileName) of
	       true->
		   {error,["Already started ",FileName]};
	       false->
		   case is_rel_loaded(CatalogDir,FileName) of
		       false->
			   {error,["Application is not loaded ",FileName]};
		       true ->
			   {ok,[Info]}=git_handler:read_file(CatalogDir,FileName), 
			   {ok,Cwd}=rpc:call(node(),file,get_cwd,[],5000),
			   PathToExecFile=maps:get(path_to_exec_file,Info),
			   ExecFile=maps:get(exec_file,Info),
			   FullPathToExecFile=filename:join([Cwd,PathToExecFile,ExecFile]),
			   case filelib:is_file(FullPathToExecFile) of
			       false->
				   {error,["No execfile in path ",ExecFile,FullPathToExecFile,FileName,CatalogDir]};
			       true->
				   case os:cmd(FullPathToExecFile++" "++"daemon") of
				       []->
					   Nodename=maps:get(nodename,Info),
					   {ok,Hostname}=net:gethostname(),
					   AppVm=list_to_atom(Nodename++"@"++Hostname),
					   case check_started(AppVm) of
					       false->
						   {error,["Failed to start application Vm ",AppVm,ExecFile,FullPathToExecFile,FileName,CatalogDir]};
				       true->
						   App=maps:get(app,Info),
						   case rpc:call(AppVm,App,ping,[],5000) of
						       pong->
							   ok;
						       Err->
							   {error,["Failed to start application ",AppVm,ExecFile,FullPathToExecFile,FileName,CatalogDir,Err]}
						   end
					   end;
				       ErrorMsg->
					   {error,["Failed to start application ",ExecFile,FullPathToExecFile,FileName,CatalogDir,ErrorMsg]}
				   end
			   end
		   end
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% stop the vm
%% @end
%%--------------------------------------------------------------------
stop_rel(CatalogDir,FileName)->
    Result=case is_rel_started(CatalogDir,FileName) of
	       false->
		   {error,["Not started ",FileName]};
	       true ->
		   {ok,[Info]}=git_handler:read_file(CatalogDir,FileName), 
		   Nodename=maps:get(nodename,Info),
		   {ok,Hostname}=net:gethostname(),
		   AppVm=list_to_atom(Nodename++"@"++Hostname),
		   App=maps:get(app,Info),
		   rpc:call(AppVm,App,stop,[],3*5000),
		   timer:sleep(5000),
		   rpc:call(AppVm,init,stop,[],5000),
		   case check_stopped(AppVm) of
		       true->
			   ok;
		       false->
			   {error,["Failed to stop application ",App,AppVm,CatalogDir,FileName]}
		   end
	   end,
  Result.		   
		   
%%--------------------------------------------------------------------
%% @doc
%% Remove the git dir 
%% @end
%%--------------------------------------------------------------------
unload_rel(CatalogDir,FileName)->
     Result=case is_rel_loaded(CatalogDir,FileName) of
	       false->
		   {error,["Not loaded ",FileName]};
	       true ->
		    case is_rel_started(CatalogDir,FileName) of
			true->
			    {error,[" Application started , needs to be stopped ",CatalogDir,FileName]};
			false->
			    {ok,Cwd}=file:get_cwd(),
			    {ok,[Info]}=git_handler:read_file(CatalogDir,FileName), 
			    AppGitDir=maps:get(application_git_dir,Info),
			    ApplicationDir=filename:join(Cwd,AppGitDir),
			    file:del_dir_r(ApplicationDir),
			    case filelib:is_dir(ApplicationDir) of
				false->
				    ok;
				true ->
				    {error,["Failed to unload application ",CatalogDir,FileName]}
			    end
		    end
	    end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

check_started(Node)->
    check_started(Node,?NumTries,?SleepInterval,false).    

 check_started(_Node,_NumTries,_SleepInterval,true)->
    true;
 check_started(_Node,0,_SleepInterval,Result)->
    Result;
 check_started(Node,NumTries,SleepInterval,false)->
    case net_adm:ping(Node) of
	pang->
	    timer:sleep(SleepInterval),
	    NewN=NumTries-1,
	    Result=false;
	pong->
	    NewN=NumTries-1,
	    Result=true
    end,
    check_started(Node,NewN,SleepInterval,Result). 

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
check_stopped(Node)->
    check_stopped(Node,?NumTries,?SleepInterval,false).    

 check_stopped(_Node,_NumTries,_SleepInterval,true)->
    true;
 check_stopped(_Node,0,_SleepInterval,Result)->
    Result;
 check_stopped(Node,NumTries,SleepInterval,false)->
    case net_adm:ping(Node) of
	pong->
	    timer:sleep(SleepInterval),
	    NewN=NumTries-1,
	    Result=false;
	pang->
	    NewN=NumTries-1,
	    Result=true
    end,
    check_stopped(Node,NewN,SleepInterval,Result).    
	    

%%--------------------------------------------------------------------
%% @doc
%% Dats: exec_file = Path to and filename to the executable file
%% @end
%%--------------------------------------------------------------------
is_rel_loaded(CatalogDir,FileName)->
    case filelib:is_dir(CatalogDir) of
	false->
	    false;
	true->
	    FullFileName=filename:join(CatalogDir,FileName),
	    case file:consult(FullFileName) of
		{error,_Reason}->
		    false;
		{ok,[Map]}-> 
		    Path=maps:get(path_to_exec_file,Map),
		    ExecFile=maps:get(exec_file,Map),
		    filelib:is_file(filename:join(Path,ExecFile))
	    end
    end.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_rel_started(CatalogDir,FileName)->
    %% check if there is a Repordir 
    IsStarted=case filelib:is_dir(CatalogDir) of
		  false->
		      false;
		  true->
		      case git_handler:read_file(CatalogDir,FileName) of
			  {error,_Reason}->
			      false;
			  {ok,[Info]}->
			      NodeName=maps:get(nodename,Info),
			      {ok,Hostname}=net:gethostname(),
			      AppVm=list_to_atom(NodeName++"@"++Hostname),
			      case net_adm:ping(AppVm) of
				  pang->
				      false;
				  pong->
				      App=maps:get(app,Info),
				      case rpc:call(AppVm,App,ping,[],5000) of
					  pong->
					      true;
					  _->
					      false
				      end
			      end
		      end
	      end,
    IsStarted.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_app_loaded(RepoDir,FileName)->
    {ok,[Info]}=git_handler:read_file(RepoDir,FileName), 
    Path=maps:get(path_to_exec_file,Info),
    ExecFile=maps:get(exec_file,Info),
    StartFile=filename:join(Path,ExecFile),
    filelib:is_file(StartFile).
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_app_started(RepoDir,FileName)->
    {ok,[Info]}=git_handler:read_file(RepoDir,FileName), 
    Nodename=maps:get(nodename,Info),
    {ok,Hostname}=net:gethostname(),
    AppVm=list_to_atom(Nodename++"@"++Hostname),
    IsStarted=case net_adm:ping(AppVm) of
		  pang->
		      false;
		  pong->
		      App=maps:get(app,Info),
		      case rpc:call(AppVm,App,ping,[],5000) of
			  pong->
			      true;
			  _->
			      false
		      end
	      end,
    IsStarted.

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
