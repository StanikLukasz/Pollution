%%%-------------------------------------------------------------------
%%% @author Lukasz Stanik
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. kwi 2018 14:25
%%%-------------------------------------------------------------------
-module(pollution).
-author("Lukasz Stanik").

%% API
-export([createMonitor/0,addStation/3,addValue/5,removeValue/4,getOneValue/4,getStationMean/3,getDailyMean/3,
  getDailyOverLimit/4]).

%patterns
%Monitor = { [list of stations], [list of values] }
%Station = { Name, {Xcoordinate,Ycoordinate}, }
%Value = { {StationXcoordinate,StationYcoordinate}, Date, Type, Value }

checkIfStationExists({X,Y}, {Stations,_}) ->
  case length( [ null || {_,{SX,SY}} <- Stations,  {X,Y} == {SX,SY} ] ) of
    0 -> false;
    _ -> true
  end;
checkIfStationExists(Name, {Stations,_}) ->
  case length( [ null || {SName,_} <- Stations, Name == SName ] ) of
    0 -> false;
    _ -> true
  end.

checkIfValueExists({X,Y}, Date, Type, {_,Values}) ->
  case length( [null || {{VX,VY},VDate,VType, _} <- Values, {{X,Y},Date,Type} == {{VX,VY},VDate,VType}]) of
    0 -> false;
    _ -> true
  end.

getStationCoor(Name,{Stations,_}) ->
  case [ {SX,SY} || {SName,{SX,SY}} <- Stations, Name == SName] of
    [] -> false;
    [H|_] -> H
  end.

createMonitor() ->{[],[]}.

addStation(Name, {X,Y}, {Stations,Values})
  when is_float(X) and is_float(Y) and (X>=0) and (Y>=0) -> %and length(Name)>0 ->
  case checkIfStationExists({X,Y}, {Stations,Values}) of
    true -> io:format("There is already station at these coordinates.");
    false -> case checkIfStationExists(Name, {Stations,Values}) of
               true -> io:format("There is already station with this name.");
               false -> {[ {Name,{X,Y}} | Stations ],Values}
             end
  end;
addStation({X,Y}, Name, {Stations,Values}) ->
  addStation(Name, {X,Y}, {Stations,Values});
addStation(_,_,M) ->
  io:format("Incorrect data."),
  M.

addValue({X,Y}, Date, Type, Value, {Stations,Values}) ->
  case checkIfStationExists({X,Y},{Stations,Values}) of
      false -> io:format("There is no station with these coordinates.");
      true -> case checkIfValueExists({X,Y}, Date, Type, {Stations,Values}) of
                true -> io:format("This value already exists!");
                false -> {Stations,[{{X,Y},Date,Type,Value}|Values]}
              end
  end;
addValue(Name, Date, Type, Value, {Stations,Values}) ->
  case getStationCoor(Name,{Stations,Values}) of
    false -> io:format("There is no station with this name.");
    C -> addValue(C,Date,Type,Value,{Stations,Values})
  end.

removeValue({X,Y}, Date, Type, {Stations,Values}) ->
  case getWholeOneValue({X,Y},Date,Type,{Stations,Values}) of
    null ->
      {Stations,Values};
    OneValue ->
      NewValues = Values -- [OneValue],
      {Stations,NewValues}
  end;
removeValue(Name, Date, Type, Monitor) ->
  case getStationCoor(Name,Monitor) of
    false -> io:format("There is no station with this name.");
    C -> removeValue(C,Date,Type,Monitor)
  end.

getWholeOneValue({X,Y},Date,Type,{Stations,Values}) ->
  case checkIfValueExists({X,Y},Date,Type,{Stations,Values}) of
    false ->
      io:format("That value doesn't exist."),
      null;
    true ->
      [OneValue] = [{{VX,VY},VDate,VType,VValue} || {{VX,VY},VDate,VType,VValue} <- Values,
        {{VX,VY},VDate,VType} == {{X,Y},Date,Type}],
      OneValue
  end;
getWholeOneValue(Name, Date, Type, Monitor) ->
  case getStationCoor(Name,Monitor) of
    false -> io:format("There is no station with this name.");
    C -> getOneValue(C,Date,Type,Monitor)
  end.

getOneValue({X,Y},Date,Type,{Stations,Values}) ->
  case checkIfValueExists({X,Y},Date,Type,{Stations,Values}) of
    false ->
      io:format("That value doesn't exist."),
      null;
    true ->
      [{_,_,_,ThisValue}] = [{{VX,VY},VDate,VType,VValue} || {{VX,VY},VDate,VType,VValue} <- Values,
        {{VX,VY},VDate,VType} == {{X,Y},Date,Type}],
      ThisValue
  end;
getOneValue(Name, Date, Type, Monitor) ->
  case getStationCoor(Name,Monitor) of
    false -> io:format("There is no station with this name.");
    C -> getOneValue(C,Date,Type,Monitor)
  end.

sum(A,B) ->
  A + B.

average(List) ->
  Count = length(List),
  Sum = lists:foldl(fun sum/2, 0, List),
  Sum/Count.

calculateStationMean({X,Y}, Type, {_,Values}) ->
  TheseValues = [ VValue || {{VX,VY}, _, VType, VValue} <- Values, {{VX,VY},VType} == {{X,Y},Type}],
  average(TheseValues).

getStationMean({X,Y}, Type, {Stations,Values}) ->
  case checkIfStationExists({X,Y},{Stations,Values}) of
    false ->
      io:format("There is no station with these coordinates."),
      null;
    true -> calculateStationMean({X,Y},Type,{Stations,Values})
  end;
getStationMean(Name, Type, Monitor) ->
  case getStationCoor(Name,Monitor) of
    false -> io:format("There is no station with this name.");
    C -> getStationMean(C,Type,Monitor)
  end.

getDailyMean(Day, Type, {_,Values}) ->
  TheseValues = [ VValue || {_, {VDay,_}, VType, VValue} <- Values, {VDay,VType} == {Day,Type}],
  average(TheseValues).

getDailyOverLimit(Day,Type,Limit,{_,Values}) ->
  sets:size(sets:from_list(
    [ VC || {VC, {VDay,_}, VType, VValue} <- Values, {VDay,VType} == {Day,Type}, VValue > Limit])).