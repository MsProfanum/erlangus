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
-export([createMonitor/0, getStationKeyName/2, getNameKeyCoords/2, addStation/3, addValue/5, getOneValue/4, getDailyMean/2, removeValue/5, getName/1, getDate/1, getType/1, getData/1, getValue/1, getStationMean/2]).

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

addValue({X,Y}, Date, Type, Value, Mon) ->
  case maps:is_key({X,Y}, Mon#monitor.stationCoords) of
    false -> erlang:throw("Station with this coordinates does not exist");
    true -> addValueStation(getStationKeyName(getNameKeyCoords({X,Y},Mon),Mon), #data{datetime = Date, type = Type, value = Value}, Mon)
  end;

addValue(Name, Date, Type, Value, Mon) ->
  case maps:is_key(Name, Mon#monitor.names) of
    false -> erlang:throw("Station with this name does not exist");
    true -> addValueStation(getStationKeyName(Name, Mon),#data{datetime = Date,type = Type, value = Value}, Mon)
  end.

addValueStation(Station, Ms, Mon) ->
  Station2 = Station#station{data = [Ms | Station#station.data]},
  Mon#monitor{names = maps:update(Station#station.name, Station2, Mon#monitor.names)}.

removeValue(Name, Date, Type, Val, Mon) -> removeValueStation(getStationKeyName(Name, Mon), #data{datetime = Date, type = Type, value = Val}, Mon);
removeValue({X,Y}, Date, Type, Val, Mon) -> removeValueStation(getStationKeyName(getNameKeyCoords({X,Y}, Mon),Mon),#data{datetime = Date,type = Type, value = Val}, Mon).

removeValueStation(Station, Ms, Mon) ->
  Station2 = #station{name = Station#station.name, localization = Station#station.localization, data = newList(Station#station.data, Ms)},
  Mon#monitor{names = maps:update(Station#station.name, Station2, Mon#monitor.names)}.

newList([],_) -> erlang:throw("There was nothing to remove");
newList(List, MsRemove) ->
  case [Ms || Ms <- List, Ms#data.datetime =:= MsRemove#data.datetime, Ms#data.value =:= MsRemove#data.value] of
    [] -> erlang:throw("There is no specific data");
    _ -> lists:delete(MsRemove,List)
  end.

getOneValue(Name, Type, Date, Station) -> %zwraca pustą listę :(
  Data = Station#station.data,
  DataList = [Ms || Ms <- Data, getDate(Ms) =:= Date, getType(Ms) =:= Type],
  case (Station#station.name =:= Name) and DataList =/= [] of
    false -> erlang:throw("There is no station like this");
    true -> [getValue(X) || X <- DataList]
  end;
getOneValue({X,Y}, Type, Date, Station) ->
  Data = Station#station.data,
  DataList = [Ms || Ms <- Data, Data#data.datetime =:= Date, Data#data.type =:= Type],
  case (Station#station.localization =:= {X,Y}) and (DataList =/= []) of
    false -> erlang:throw("There is no station like this");
    true -> [getValue(X) || X <- DataList]
  end.
getName(Station) -> Station#station.name.
getDate(Data) -> Data#data.datetime.
getData(Station) -> Station#station.data.
getType(Data) -> Data#data.type.
getValue(Data) -> Data#data.value.



getStationMean(Type, Station) ->
  Data = Station#station.data,
  DataList = [Ms || Ms <- Data, getType(Ms) =:= Type],
  case DataList of
    [] -> erlang:throw("Station does not have any values of this type");
    _ -> L = [getValue(X) || X <- DataList],
      lists:sum(L)/length(L)
  end.

getDailyMean(Type, Mon) ->
  Names = maps:keys(Mon#monitor.names),
  Stations = lists:map(fun(X) -> getStationKeyName(X,Mon) end, Names),
  Data = lists:map(fun(X) -> getData(X) end, Stations),
  Values = [getValue(M) || M <- Data, M#data.type =:= Type],
  lists:sum(Values).