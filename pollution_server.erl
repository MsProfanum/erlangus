%%%-------------------------------------------------------------------
%%% @author kasia
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2019 13:38
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("kasia").
-behaviour(gen_server).
%% API
-export([getStationMean/1, getOneValue/3, getDailyMean/1, addValue/4, stop/0, start/0, init/1, handle_call/3, terminate/2, addStation/2]).
-import('/home/kasia/IdeaProjects/lab3/src/pollution',[createMonitor/0, getStationKeyName/2, getNameKeyCoords/2, addStation/3, addValue/5, getOneValue/4, getDailyMean/2, removeValue/5,
getName/1, getDate/1, getType/1, getData/1, getValue/1, getStationMean/2, getMovingMean/3, filteredData/2, dataToVal/1, valResult/1]).
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

-define(SERVER, ?MODULE).

start() -> gen_server:start_link({local, ?SERVER},?MODULE,[],[]).
stop() -> gen_server:stop(?SERVER).

-spec addStation(string(), localization()) -> ok | erlang:throw().
addStation(Name, Localization) ->
  gen_server:call(?MODULE, {addStation, Name, Localization}).

-spec addValue(localization() | string(), calendar:datetime(), string(), float()) -> ok | erlang:throw().
addValue(Loc, Date, Type, Value) ->
  gen_server:call(?MODULE,{addValue,Loc,Date,Type,Value});
addValue(Name, Date, Type, Value) ->
  gen_server:call(?MODULE,{addValue,Name,Date,Type,Value}).

-spec getDailyMean(float()) -> float().
getDailyMean(Type) ->
  gen_server:call(?MODULE, {getDailyMean,Type}).

-spec getOneValue(string() | localization(), string(), calendar:datetime()) -> list() | erlang:throw().
getOneValue(Loc, Type, Date) ->
  gen_server:call(?MODULE, {getDailyMean,Loc,Type,Date});
getOneValue(Name, Type, Date) ->
  gen_server:call(?MODULE, {getDailyMean,Name,Type,Date}).

-spec getStationMean(string()) -> float() | erlang:throw().
getStationMean(Type) ->
  gen_server:call(?MODULE, {getStationMean,Type}).

init([]) ->
  Monitor = pollution:createMonitor(),
  {ok,Monitor}.

terminate(Reason, Monitor) -> ok.

handle_call({addStation,Name,Localization},_From,Monitor) ->
  case pollution:addStation(Name,Localization,Monitor) of
    #monitor{} = Mon -> {reply,ok,Mon};
    _ -> {reply,false,Monitor}
  end;

handle_call({addValue,Loc,Date,Type,Value},_From,Monitor) ->
  case pollution:addValue(Loc,Date,Type,Value,Monitor) of
    #monitor{} = Mon ->
      {reply,ok,Mon};
    _ -> {reply,false,Monitor}
  end;

handle_call({addValue,Name,Date,Type,Value},_From,Monitor) ->
  case pollution:addValue(Name,Date,Type,Value,Monitor) of
    #monitor{} = Mon ->
      {reply,ok,Mon};
    _ -> {reply,false,Monitor}
  end;

handle_call({getDailyMean,Type},_From,Monitor) ->
  case pollution:getDailyMean(Type,Monitor) of
    Val when is_float(Val) ->
      {reply,Val,Monitor};
    false -> {reply,false,Monitor}
  end;

handle_call({getOneValue,Loc,Type,Date},_From,Monitor) ->
  case pollution:getOneValue(Loc,Type,Date,Monitor) of
    Val when is_float(Val) -> {reply,Val,Monitor};
    false -> {reply,false,Monitor}
  end;

handle_call({getOneValue,Name,Type,Date},_From,Monitor) ->
  case pollution:getOneValue(Name,Type,Date,Monitor) of
    Val when is_float(Val) -> {reply,Val,Monitor};
    false -> {reply,false,Monitor}
  end;

handle_call({getStationMean,Type},_From,Monitor) ->
  case pollution:getStationMean(Type,Monitor) of
    Val when is_float(Val) -> {reply, Val, Monitor};
    false -> {reply,false,Monitor}
  end.