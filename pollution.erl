%%%-------------------------------------------------------------------
%%% @author kasia
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Mar 2019 11:52
%%%-------------------------------------------------------------------
-module(pollution).
-author("kasia").

%% API
-export([createMonitor/0, getStationKeyName/2, getNameKeyCoords/2, addStation/3, addValue/5, removeValue/4, getOneValue/3, getStationMean/2, getDailyMean/2]).

-record(monitor, {
  names, %mapa przechowująca nazwę stacji jako klucze, wartością oddawaną jest cała stacja
  stationCoords %mapa przechowująca współrzędne stacji jako klucz, zwracająca nazwę stacji
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

createMonitor() ->
  #monitor{names = maps:new(), stationCoords = maps:new()}.

getStationKeyName(Name, Mon) ->
    case maps:find(Name, Mon#monitor.names) of
      {ok, Station} -> Station;
      _ -> erlang:throw("Station with this name does not exist")
    end.

getNameKeyCoords(Localization, Mon) ->
  case maps:find(Localization,Mon#monitor.stationCoords) of
    {ok, Name} -> Name;
    _ -> erlang:throw("Station with this coordinated does not exist")
  end.

addStation(Name, Localization, Mon) ->
  case (maps:is_key(Name, Mon#monitor.names) or maps:is_key(Localization, Mon#monitor.stationCoords)) of
    false ->#monitor{
      names = maps:put(Name, #station{name = Name, localization = Localization, data = []},Mon#monitor.names),
      stationCoords = maps:put(Localization, Name, Mon#monitor.stationCoords)
    };
    true -> erlang:throw("Station is already in the map")
  end.

addValue(Name, Date, Type, Value, Mon) ->
  case maps:is_key(Name, Mon#monitor.names) of
    false -> erlang:throw("Station with this name does not exist");
    true -> addValueStation(getStationKeyName(Name, Mon),#data{datetime = Date,type = Type, value = Value}, Mon)
  end;

addValue(Location, Date, Type, Value, Mon) ->
  case maps:is_key(Location, Mon#monitor.stationCoords) of
    false -> erlang:throw("Station with this coordinates does not exist");
    true -> addValueStation(getStationKeyName(getNameKeyCoords(Location,Mon),Mon), #data{datetime = Date, type = Type, value = Value}, Mon)
  end.

addValueStation(Station, Ms, Mon) ->
  Station2 = Station#station{data = [Ms | Station#station.data]},
  Mon#monitor{names = maps:put(Station#station.name, Station2, Mon#monitor.names)}.

removeValue(Name, Date, Val, Mon) -> removeValueStation(getStationKeyName(Name, Mon), #data{datetime = Date, value = Val}, Mon);
removeValue(Loc, Date, Val, Mon) -> removeValueStation(getStationKeyName(getNameKeyCoords(Loc, Mon),Mon),#data{datetime = Date, value = Val}, Mon).

removeValueStation(Station, Ms, Mon) ->
  Station2 = Station#station{data = newList(Station#station.data,Ms)},
  Mon#monitor{names = maps:put(Station#station.name, Station2, Mon#monitor.names)}.

newList([],_) -> erlang:throw("There was nothing to remove");
newList(List, MsRemove) ->
  case [Ms || Ms <- List, Ms#data.datetime =:= MsRemove#data.datetime, Ms#data.value =:= MsRemove#data.value] of
    [] -> erlang:throw("There is no specific data");
    _ -> lists:delete(MsRemove,List)
  end.


getOneValue(name, date, type) ->
  erlang:error(not_implemented).

getStationMean(name, type) ->
  erlang:error(not_implemented).

getDailyMean(date, type) ->
  erlang:error(not_implemented).