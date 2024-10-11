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
-define(CatalogDir,"application_specs").
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
    {ok,Node}=test_start_node(),
    ok=test_load(),
    ok=test_start(Node),
    ok=test_stop(Node),
    ok=test_unload(),
    ok=test_stop_node(Node),

    ok=test_intent(),

    ok.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
% intent_loop()
% 1. Check if repo is updated => update and 
% 2. check files to be started and start them
% 3. check files to be stopped and stop them
% 4 Wait a minute and loop
-define(AppToStop,"kvs_glurk_test.application").

test_intent()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {ok,WantedApplicationFiles}=application_server:get_wanted_applications(),
    {ok,[]}=application_server:get_active_applications(),
    FilesToStart1=application_server:applications_to_start(),
    [File1|_]=FilesToStart1,
    StartResult1=application_server:load_start(File1),
    io:format("StartResult1 ~p~n",[StartResult1]),    
    {ok,["add_test.application"]}=application_server:get_active_applications(),
    FilesToStart2=application_server:applications_to_start(),
    StartResult2=application_server:load_start(FilesToStart2),
    io:format("StartResul2 ~p~n",[StartResult2]),    
    {ok,["kvs_test.application","add_test.application"]}=application_server:get_active_applications(),
    []=application_server:applications_to_start(),
    []=application_server:applications_to_stop(),

    %% load and start application that should not be started
    []=os:cmd("cp test/"++?AppToStop++" application_specs"),
    StartResult3=application_server:load_start(?AppToStop),
    io:format("StartResul3 ~p~n",[StartResult3]),     
    []=os:cmd("rm application_specs/"++?AppToStop),    
    
    FilesToStop=application_server:applications_to_stop(),    
    ["kvs_glurk_test.application"]=FilesToStop,
    

    StopResult1=[application_server:stop_unload(FileToStop)||FileToStop<-FilesToStop],
    io:format("StopResult1 ~p~n",[StopResult1]),    
    []=application_server:applications_to_start(),
    []=application_server:applications_to_stop(),

    StopResult2=[application_server:stop_unload(WantedApplicationFile)||WantedApplicationFile<-WantedApplicationFiles],
    io:format("StopResult2 ~p~n",[StopResult2]),      
    {ok,[]}=application_server:get_active_applications(), 
    
    ["add_test.application","kvs_test.application"]=application_server:applications_to_start(),
    []=application_server:applications_to_stop(),

    ok.


    
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
test_start_node()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    {error,["Not started"]}=application_server:stop_node(?AddTestFileName),
    {ok,Node}=application_server:start_node(?AddTestFileName),
    pong=net_adm:ping(Node),
    
    {error,["Allready started"]}=application_server:start_node(?AddTestFileName),
    
 
    {ok,Node}.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
test_load()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
   
    ok=application_server:load(?AddTestFileName),
    {error,["Allready loaded","application_specs/add_test.application"]}=application_server:load(?AddTestFileName),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
test_start(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
   
    ok=application_server:start(?AddTestFileName),
    pong=rpc:call(Node,log,ping,[],5000),
    pong=rpc:call(Node,rd,ping,[],5000),
    pong=rpc:call(Node,add_test,ping,[],5000),
    42=rpc:call(Node,add_test,add,[20,22],5000),
    {error,["Failed to start ","application_specs/add_test.application",
	    [{log,{error,{already_started,log}}},
	     {rd,{error,{already_started,rd}}},
	     {add_test,{error,{already_started,add_test}}}
	    ]
	   ]
    }=application_server:start(?AddTestFileName),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
test_stop(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
   
    ok=application_server:stop(?AddTestFileName),
    {badrpc,_}=rpc:call(Node,log,ping,[],5000),
    {badrpc,_}=rpc:call(Node,rd,ping,[],5000),
    {badrpc,_}=rpc:call(Node,add_test,ping,[],5000),
    {error,["Failed to stop ","application_specs/add_test.application",
	    [{log,{error,{not_started,log}}},
	     {rd,{error,{not_started,rd}}},
	     {add_test,{error,{not_started,add_test}}}]]}=application_server:stop(?AddTestFileName),
    ok.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
test_unload()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
   
    ok=application_server:unload(?AddTestFileName),
    {error,["Not loaded","application_specs/add_test.application"]}=application_server:unload(?AddTestFileName),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
test_stop_node(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
     ok=application_server:stop_node(?AddTestFileName),
    {error,["Not started"]}=application_server:stop_node(?AddTestFileName),
    pang=net_adm:ping(Node),
    
    
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
