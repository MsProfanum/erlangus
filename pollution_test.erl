%%%-------------------------------------------------------------------
%%% @author kasia
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2019 12:55
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("kasia").
-include_lib("eunit/include/eunit.hrl").

-record(monitor, {
  names,
  stationCoords
}).
-type monitor() :: #monitor{}.
-record(station, {
  name :: string(),
  localization :: localization(),
  data = [] :: data()
}).
-type station() :: #station{}.
-type localization() :: {float(), float()}.
-record(data, {
  datetime :: calendar:datetime(),
  type = "":: string(),
  value :: float()
}).
-type data() :: #data{}.

-record(val, {value :: float(), weight :: integer()}).
-type val() :: #val{}.

-define(NAME1,"station1").
-define(NAME2,"station2").
-define(LOC1, {1.0,1.0}).
-define(LOC2,{10.0,10.0}).
-define(DATE1,{{2019,04,10},{23,17,30}}).
-define(DATE2,{{2019,04,10},{21,03,20}}).
-define(V1,1.0).
-define(V2,10.0).
createMonitor_test() ->
  ?assertEqual(#monitor{names = maps:new(), stationCoords = maps:new()}, pollution:createMonitor()).

addStation_test() ->
  Mon = pollution:createMonitor(),
  ?assertMatch(#monitor{}, pollution:addStation(?NAME1,?LOC1,Mon)).

addSameStation_test() ->
  Mon = pollution:createMonitor(),
  Mon1 = pollution:addStation(?NAME1,?LOC1,Mon),
  ?assertThrow("Station is already in the map",pollution:addStation(?NAME1,?LOC1,Mon1)).

operationsOnValue_test() -> %failed
  Mon = pollution:createMonitor(),
  Mon1 = pollution:addStation(?NAME1,?LOC1,Mon),
  ?assertEqual(
    pollution:addValue(?NAME1,?DATE1,"PM10",?V1,Mon1),
    pollution:addValue(?LOC1,?DATE1,"PM10",?V1,Mon1)
  ),

  Mon2 = pollution:addValue(?NAME1,?DATE1,"PM10",?V1,Mon1),
  Station = pollution:getStationKeyName(?NAME1,Mon2),
  %getting value
  ?assertMatch([?V1], pollution:getOneValue(?NAME1,"PM10",?DATE1,Station)),
  %removing value
  ?assertEqual(Mon1,pollution:removeValue(?NAME1,?DATE1,"PM10",?V1,Mon2)).

getStationMean_test() ->
  Mean = (?V1 + ?V2)/2,
  Mon = pollution:createMonitor(),
  Mon1 = pollution:addStation(?NAME1,?LOC1,Mon),
  Mon2 = pollution:addValue(?NAME1,?DATE1,"PM10",?V1,Mon1),
  Mon3 = pollution:addValue(?NAME1,?DATE2,"PM10",?V2,Mon2),
  Station = pollution:getStationKeyName(?NAME1,Mon3),
  ?assertEqual(Mean,pollution:getStationMean("PM10",Station)).

getDailyMean_test() -> %failed
  Mean = (?V1 + ?V2)/2,
  Mon = pollution:createMonitor(),
  Mon1 = pollution:addStation(?NAME1,?LOC1,Mon),
  Mon2 = pollution:addValue(?NAME1,?DATE1,"PM10",?V1,Mon1),
  Mon3 = pollution:addValue(?NAME1,?DATE2,"PM10",?V2,Mon2),
  ?assertEqual(Mean,pollution:getDailyMean("PM10",Mon3)).