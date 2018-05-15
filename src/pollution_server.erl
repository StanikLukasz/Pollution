%%%-------------------------------------------------------------------
%%% @author Lukasz Stanik
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. maj 2018 17:15
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Lukasz Stanik").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start/0, sAddStation/2, sAddValue/4, sRemoveValue/3, sGetOneValue/3, sGetStationMean/2, sGetDailyMean/2,
  sGetDailyOverLimit/3, stop/0, init/0, sGetMonitor/0]).

-import(pollution,[createMonitor/0,addStation/3,addValue/5,removeValue/4,getOneValue/4,getStationMean/3,getDailyMean/3,
getDailyOverLimit/4]).

start() ->
  register(pollServer, spawn(pollution_server, init, [])).

init() ->
  EmptyMonitor = createMonitor(),
  loop(EmptyMonitor).

loop(Monitor) ->
  receive
    {request, Pid, stop} ->
      Pid ! {reply, ok};
    {request, Pid, {addStation, Name, Coor}} ->
      NewMonitor = addStation(Name, Coor, Monitor),
      Pid ! {reply, ok},
      loop(NewMonitor);
    {request, Pid, {addValue, Station, Date, Type, Value}} ->
      NewMonitor = addValue(Station, Date, Type, Value, Monitor),
      Pid ! {reply, ok},
      loop(NewMonitor);
    {request, Pid, {removeValue, Station, Date, Type}} ->
      NewMonitor = removeValue(Station, Date, Type, Monitor),
      Pid ! {reply, ok},
      loop(NewMonitor);
    {request, Pid, {getOneValue, Station, Date, Type}} ->
      Result = getOneValue(Station, Date, Type, Monitor),
      Pid ! {reply, Result},
      loop(Monitor);
    {request, Pid, {getStationMean, Station, Type}} ->
      Result = getStationMean(Station, Type, Monitor),
      Pid ! {reply, Result},
      loop(Monitor);
    {request, Pid, {getDailyMean, Day, Type}} ->
      Result = getDailyMean(Day, Type, Monitor),
      Pid ! {reply, Result},
      loop(Monitor);
    {request, Pid, {getDailyOverLimit, Day, Type, Limit}} ->
      Result = getDailyOverLimit(Day, Type, Limit, Monitor),
      Pid ! {reply, Result},
      loop(Monitor);
    {request, Pid, getMonitor} ->
      Pid ! {reply, Monitor},
      loop(Monitor)
  end.

call(Message) ->
  pollServer ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

sAddStation(Name, Coor) ->
 call({addStation, Name, Coor}).

sAddValue(Station, Date, Type, Value) ->
  call({addValue, Station, Date, Type, Value}).

sRemoveValue(Station, Date, Type) ->
  call({removeValue, Station, Date, Type}).

sGetOneValue(Station, Date, Type) ->
  call({getOneValue, Station, Date, Type}).

sGetStationMean(Station, Type) ->
  call({getStationMean, Station, Type}).

sGetDailyMean(Day, Type) ->
  call({getDailyMean, Day, Type}).

sGetDailyOverLimit(Day, Type, Limit) ->
  call({getDailyOverLimit, Day, Type, Limit}).

sGetMonitor() ->
  call(getMonitor).

stop() ->
  call(stop).


%tests

addStation1_test_() ->
  start(),
  sAddStation("Slowackiego",{5.67,6.78}),
  ?_assertMatch({ [ {"Slowackiego",{5.67,6.78}} ] ,[]},
     sGetMonitor() ).





















  %stop().

%addStation2_test_() ->
 % start(),
 % sAddStation({5.67,6.78} , "Slowackiego"),
 % ?_assertMatch({ [ {"Slowackiego",{5.67,6.78}} ] ,[]},
 %   sGetMonitor() ),
 % stop().

%%addValue_test_() ->
  %?_assertMatch({_, [{{1.23,2.34},{{_,_,_},{_,_,_}},"PM10",100}]},
   % addValue({1.23,2.34},calendar:local_time(),"PM10",100, { [{"Slowackiego",{1.23,2.34}}] , [] } )),
  %?_assertMatch({_, [{{1.23,2.34},{{_,_,_},{_,_,_}},"PM10",100}]},
   % addValue("Slowackiego",calendar:local_time(),"PM10",100, { [{"Slowackiego",{1.23,2.34}}] , [] } )).




