%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 24 Sep 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(test_application_server).

%% API
-export([start/0]).

%%%
-define(AddTestVm,add_test@c50).
-define(AddTestApp,add_test).
-define(AddTestFileName,"add_test.application").
-define(AddTestRepoDir,"add_test" ).
-define(CatalogDir,"catalog_specs").
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.api").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
start()->
     io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    %% Clean up
    application_server:stop_app(?AddTestFileName),
    application_server:unload_app(?AddTestFileName),

    %% load_rel test
    {error,["Application is not loaded ","add_test.application"]}=application_server:start_app(?AddTestFileName),
    {error,["Not started ","add_test.application"]}=application_server:stop_app(?AddTestFileName),
    {error,["Not loaded ","add_test.application"]}=application_server:unload_app(?AddTestFileName),
    ok=application_server:load_app(?AddTestFileName),
    %% start_app test
    {error,["Already loaded ","add_test.application"]}=application_server:load_app(?AddTestFileName),
    {error,["Not started ","add_test.application"]}=application_server:stop_app(?AddTestFileName),
     ok=application_server:start_app(?AddTestFileName),
    ApplicationSpecFile=filename:join(?CatalogDir,?AddTestFileName),
    {ok,[Info]}=file:consult(ApplicationSpecFile), 
    Nodename=maps:get(nodename,Info),
    {ok,Hostname}=net:gethostname(),
    AppVm=list_to_atom(Nodename++"@"++Hostname),
    App=maps:get(app,Info),
    42=rpc:call(AppVm,App,add,[20,22],3*5000),
    %% stop_app test 
    {error,["Already loaded ","add_test.application"]}=application_server:load_app(?AddTestFileName),
    {error,[" Application started , needs to be stopped ","catalog_specs","add_test.application"]}=application_server:unload_app(?AddTestFileName),    
    {error,["Already started ","add_test.application"]}=application_server:start_app(?AddTestFileName),
    ok=application_server:stop_app(?AddTestFileName),
    {badrpc,nodedown}=rpc:call(AppVm,App,add,[20,22],3*5000),
    %% unload_app 
    {error,["Already loaded ","add_test.application"]}=application_server:load_app(?AddTestFileName),
    {error,["Not started ","add_test.application"]}=application_server:stop_app(?AddTestFileName),
    ok=application_server:unload_app(?AddTestFileName),  
    
    
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test1()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    %% Clean up
    lib_application:stop_rel(?CatalogDir,?AddTestFileName),
    lib_application:unload_rel(?CatalogDir,?AddTestFileName),

    %% load_rel test
    {error,["Application is not loaded ","add_test.application"]}=lib_application:start_rel(?CatalogDir,?AddTestFileName),
    {error,["Not started ","add_test.application"]}=lib_application:stop_rel(?CatalogDir,?AddTestFileName),
    {error,["Not loaded ","add_test.application"]}=lib_application:unload_rel(?CatalogDir,?AddTestFileName),
    ok=lib_application:load_rel(?CatalogDir,?AddTestFileName),
    %% start_rel test
    {error,["Already loaded ","add_test.application"]}=lib_application:load_rel(?CatalogDir,?AddTestFileName),
    {error,["Not started ","add_test.application"]}=lib_application:stop_rel(?CatalogDir,?AddTestFileName),
     ok=lib_application:start_rel(?CatalogDir,?AddTestFileName),
    ApplicationSpecFile=filename:join(?CatalogDir,?AddTestFileName),
    {ok,[Info]}=file:consult(ApplicationSpecFile), 
    Nodename=maps:get(nodename,Info),
    {ok,Hostname}=net:gethostname(),
    AppVm=list_to_atom(Nodename++"@"++Hostname),
    App=maps:get(app,Info),
    42=rpc:call(AppVm,App,add,[20,22],3*5000),
    %% stop_rel test 
    {error,["Already loaded ","add_test.application"]}=lib_application:load_rel(?CatalogDir,?AddTestFileName),
    {error,[" Application started , needs to be stopped ","catalog_specs","add_test.application"]}=lib_application:unload_rel(?CatalogDir,?AddTestFileName),    
    {error,["Already started ","add_test.application"]}=lib_application:start_rel(?CatalogDir,?AddTestFileName),
    ok=lib_application:stop_rel(?CatalogDir,?AddTestFileName),
    {badrpc,nodedown}=rpc:call(AppVm,App,add,[20,22],3*5000),
    %% unload_rel 
    {error,["Already loaded ","add_test.application"]}=lib_application:load_rel(?CatalogDir,?AddTestFileName),
    {error,["Not started ","add_test.application"]}=lib_application:stop_rel(?CatalogDir,?AddTestFileName),
    ok=lib_application:unload_rel(?CatalogDir,?AddTestFileName),  
    
    ok.



%%%===================================================================
%%% Internal functions
%%%===================================================================
