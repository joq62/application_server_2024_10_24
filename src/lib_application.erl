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
	 init/2,
	 update/2,
	 timer_to_call_update/1
	]).

-export([
	 load_app/2,
	 start_app/2,
	 stop_app/2,
	 unload_app/2,
	 is_app_loaded/2,
	 is_app_started/2

	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-define(TargetDir,"ctrl_dir").
-define(Vm,ctrl@c50).
-define(TarFile,"ctrl.tar.gz").
-define(App,"ctrl").
-define(TarSrc,"release"++"/"++?TarFile).
-define(StartCmd,"./"++?TargetDir++"/"++"bin"++"/"++?App).

% #{id=>"adder3",
%  application_name=>"adder3",
%  vsn=>"0.1.0",
%  app=>adder3, 
%  erl_args=>" ",
%  git=>"https://github.com/joq62/adder3.git",
%  target_dir=>"adder3_dir",
%  sname=>"adder3",
%  tar_file=>"adder3.tar.gz",
%  tar_src=>"adder3/release/adder3.tar.gz",
%  start_cmd=>"./adder3_dir/bin/adder3"
% }.
load_app(RepoDir,FileName)->
    {ok,[Info]}=git_handler:read_file(RepoDir,FileName), 
    StartFile=maps:get(start_cmd,Info),
    Result=case is_app_loaded(RepoDir,FileName) of
	       true->
		   {error,["Already loaded ",FileName]};
	       false ->
		   ApplicationGitDir=maps:get(application_name,Info),
		   file:del_dir_r(ApplicationGitDir),
		   AppTargetDir=maps:get(target_dir,Info),
		   file:del_dir_r(AppTargetDir),		   
		   Sname=maps:get(sname,Info),
		   {ok,Hostname}=net:gethostname(),
		   AppVm=list_to_atom(Sname++"@"++Hostname),
		   rpc:call(AppVm,init,stop,[],3000),
		   timer:sleep(2000),

		   AppGitPath=maps:get(git,Info),
		   os:cmd("git clone "++AppGitPath),
		   ok=file:make_dir(AppTargetDir),
		   TarSrc=maps:get(tar_src,Info),
		   []=os:cmd("tar -zxf "++TarSrc++" -C "++AppTargetDir),
		   file:del_dir_r(ApplicationGitDir),

		   true=filelib:is_file(StartFile),
		   ok
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
start_app(RepoDir,FileName)->
    Result=case is_app_loaded(RepoDir,FileName) of
	       false->
		   {error,["Not loaded ",FileName]};
	       true ->
		   {ok,[Info]}=git_handler:read_file(RepoDir,FileName), 
		   PathStartFile=maps:get(path_start_file,Info),
		   {ok,Cwd}=file:get_cwd(),
		   StartCmd=Cwd++"/"++PathStartFile,
		   StartResult=os:cmd(StartCmd++" "++"daemon"),
		   Sname=maps:get(sname,Info),
		   {ok,Hostname}=net:gethostname(),
		   AppVm=list_to_atom(Sname++"@"++Hostname),
		   case check_started(AppVm) of
		       true->
			   case StartResult of
			       []->
				   ok;
			       _->
				   {error,["Started application with error code  ",FileName,StartResult]}
			   end;
		       false->
			   {error,["Failed to start application ",FileName,StartResult]}
		   end     
	   end,
    Result.

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
stop_app(RepoDir,FileName)->
    Result=case is_app_started(RepoDir,FileName) of
	       false->
		   {error,["Not started ",FileName]};
	       true ->
		   {ok,[Info]}=git_handler:read_file(RepoDir,FileName), 
		   Sname=maps:get(sname,Info),
		   {ok,Hostname}=net:gethostname(),
		   AppVm=list_to_atom(Sname++"@"++Hostname),
		   rpc:call(AppVm,init,stop,[],5000),
		   true=check_stopped(AppVm),
		   ok
	   end,

  Result.

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
%% 
%% @end
%%--------------------------------------------------------------------
unload_app(RepoDir,FileName)->
     Result=case is_app_loaded(RepoDir,FileName) of
	       false->
		   {error,["Not loaded ",FileName]};
	       true ->
		    case is_app_started(RepoDir,FileName) of
			true->
			    {error,[" Application started , needs to be stopped ",FileName]};
			false->
			    {ok,[Info]}=git_handler:read_file(RepoDir,FileName), 
			    ApplicationGitDir=maps:get(application_name,Info),
			    file:del_dir_r(ApplicationGitDir),
			    AppTargetDir=maps:get(target_dir,Info),
			    file:del_dir_r(AppTargetDir),
			    Sname=maps:get(sname,Info),
			    {ok,Hostname}=net:gethostname(),
			    AppVm=list_to_atom(Sname++"@"++Hostname),
			    rpc:call(AppVm,init,stop,[],5000),
			    timer:sleep(2000),
			    pang=net_adm:ping(AppVm),
			    ok
		    end
	    end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_app_loaded(RepoDir,FileName)->
    {ok,[Info]}=git_handler:read_file(RepoDir,FileName), 
    StartFile=maps:get(start_cmd,Info),
    filelib:is_file(StartFile).
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_app_started(RepoDir,FileName)->
    {ok,[Info]}=git_handler:read_file(RepoDir,FileName), 
    Sname=maps:get(sname,Info),
    {ok,Hostname}=net:gethostname(),
    AppVm=list_to_atom(Sname++"@"++Hostname),
    IsStarted=case net_adm:ping(AppVm) of
		  pang->
		      false;
		  pong->
		      true
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
