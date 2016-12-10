-module(calendar_sb).

-author("samir").

-import(string,[substr/3, to_integer/1]).

-export([get_month_info/2, get_utc_now/0, day_name/1, is_weekend/1, get_business_calendar/1,
  get_public_holidays/0, is_public_holiday/2, get_full_calendar/1, get_full_month/2]).

-define(MONTHS, [{1,"Jan","January",31},
                 {2,"Feb","Feburary",0},{3,"Mar","March",31},{4,"Apr","April",30},
                 {5,"May","May",31},{6,"Jun","June",30},{7,"Jul","July",31},
                 {8,"Aug","August",31},{9,"Sept","September",30},{10,"Oct","October",31},
                 {11,"Nov","November",30},{12,"Dec","December",31}]).
%%FUNCTIONS

day_name(D) when D > 0, D < 8 ->
  case D of
    1-> "Monday";
    2-> "Tuesday";
    3-> "Wednesday";
    4-> "Thursday";
    5-> "Friday";
    6-> "Saturday";
    7-> "Sunday"
  end;
day_name(_)->
  invalid_day.

get_month_info(M,Yr) ->
  R = [{Mn,X,Y,Z} || {Mn,X,Y,Z}<-?MONTHS,Mn=:=M],
  case R of
    [] -> invalid_month;
    _ ->
      L = lists:last(R),
      {Mnth,X,Y,_} = L,
      if
        Mnth =:= 2 ->
          case calendar:is_leap_year(Yr) of
            true -> {Mnth,X,Y,29};
            false -> {Mnth,X,Y,28}
          end;
        true -> L
      end
  end.

get_month_days(Mnth,Yr, IncludeWeekends, IncludePublicHolidays, Hols)->
  Fdow = fun(X)->calendar:day_of_the_week(X) end,
  Ftoutc = fun(X)->calendar:local_time_to_universal_time(X) end,
  {_,_,_,DaysInMonth} = get_month_info(Mnth,Yr),
  L = [{{Yr,Mnth,X},{0,0,0}} || X <- lists:seq(1,DaysInMonth)],
  Utc = [Ftoutc(X)|| X<-L],
  case IncludeWeekends of
    true ->
      case IncludePublicHolidays of
        true ->
          [{D,T,{dayofweek,Fdow(D),day_name(Fdow(D))}}|| {D,T}<-Utc];
        false ->
          [{D,T,{dayofweek,Fdow(D),day_name(Fdow(D))}}|| {D,T}<-Utc, is_public_holiday(D,Hols)=:=false]
      end;
    false ->
      case IncludePublicHolidays of
        true ->
          [{D,T,{dayofweek,Fdow(D),day_name(Fdow(D))}}|| {D,T}<-Utc, is_weekend({D,T})=:=false];
        false ->
          L1 = [{D,T,{dayofweek,Fdow(D),day_name(Fdow(D))}}|| {D,T}<-Utc, is_weekend({D,T})=:=false],
          [{R,R2,R3} || {R,R2,R3} <- L1, is_public_holiday({R,{0,0,0}},Hols)=:=false]
      end
  end.

get_utc_now() ->
  calendar:universal_time().

is_public_holiday(D,H) ->
  {DT,_}=D,
  case calendar:valid_date(DT) of
    true  ->
      lists:member(D,H);
    _ ->
      {bad_date}
  end.

is_weekend(D)->
  case D of
    {D1,_} ->
      DoW = calendar:day_of_the_week(D1),
      case DoW of
        6 -> true;
        7 -> true;
        _ -> false
      end;
    _ ->
      {unknown}
  end.

get_business_calendar(Yr)->
  Hols=get_public_holidays(),
  U = [get_month_days(X,Yr,false,false,Hols) || X <- lists:seq(1,12)],
  U.

get_full_calendar(Yr)->
  [get_month_days(X,Yr,true,true,[]) || X <- lists:seq(1,12)].


get_full_month(M,Yr)->
  get_month_days(M,Yr,true,true,[]).


get_public_holidays()->

  inets:start(),
  ssl:start(),
  {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {"https://www.gov.uk/bank-holidays/england-and-wales.ics", []}, [], []),
  inets:stop(),
  ssl:stop(),
  Lines = string:tokens(Body, "\n\r"),
  Starts = [string:substr(L,20,8) || L <- Lines, string:substr(L,1,7)=:="DTSTART"],
  Ends = [string:substr(L,18,8) || L <- Lines, string:substr(L,1,5)=:="DTEND"],
  All = lists:merge(Starts,Ends),
  Res = lists:map(fun(X) -> {{cln(substr(X,1,4)), cln(substr(X,5,2)), cln(substr(X,7,8))},{0,0,0}} end, All),
  Res.

cln(C)->
  {V,_} = to_integer(C),
  V.