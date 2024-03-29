%%%-------------------------------------------------------------------
%%% @author Boaz Ardel <boaz@ubuntu>
%%% @copyright (C) 2015, Boaz Ardel
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2015 by Boaz Ardel <boaz@ubuntu>
%%%-------------------------------------------------------------------
-module(gen_RM).

-behaviour(gen_server).
%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% defines
-define(Display_Node, 'S@10.0.0.3').
-define(Display_Module, gen_Display).
-define(Car_Speed, 20).
-define(Road_Length, 550).
-define(Road_width, 300).
-define(Car_length, 50).
-define(Lane_Num, 3).
-define(Color_Num, 4).
-define(Map_size, 1500).
-define(Lambda, 1/3000). %Ideal: 1/3000


%%%===================================================================
%%% API
%%%===================================================================
start_link(Road_Direction) ->
    gen_server:start(?MODULE, [Road_Direction], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Road_Direction]) ->
	%init DataBase
	ETS = ets:new(rm_ets, [set]),
	Self = self(), spawn(fun() -> timer(exp,?Lambda,Self) end), 	%Setting timeout-timer for generating cars
	register(Road_Direction,Self), %name will be: north/south/east/west
 	random:seed(erlang:phash2([node()]),erlang:monotonic_time(),erlang:unique_integer()),	%individual seed
    {ok, {ETS,Road_Direction,{[],[],[]}}}.	%simple queues

handle_info({timeout,exp}, {ETS,Road_Direction,{Q1,Q2,Q3}}) ->
	Lane = random:uniform(?Lane_Num) -1,  %randomizing Lane % Color
	Color = random:uniform(?Color_Num) -1,
	case Lane of 0-> L = length(Q1); 1-> L = length(Q2); 2-> L = length(Q3) end,  	%checking if there is space in lane.
	if
		(L < (?Road_Length div ?Car_length)-1) ->        							%setting up start conditions depending which RM you are
			case Road_Direction of
				west ->
					Deg = 0,
					X_Start_Axis = 0,
					Y_Start_Axis = (?Map_size div 2) + Lane*(?Road_width div 6) + (?Road_width div 12),
					X_Final_Axis = ?Road_Length,
					Y_Final_Axis = Y_Start_Axis;
				east ->
					Deg = 180,
					X_Start_Axis = ?Map_size,
					Y_Start_Axis = (?Map_size div 2) - Lane*(?Road_width div 6) - (?Road_width div 12),
					X_Final_Axis = ?Map_size - ?Road_Length,
					Y_Final_Axis = Y_Start_Axis;
				north ->
					Deg = 270,
					X_Start_Axis = (?Map_size div 2) - Lane*(?Road_width div 6) - (?Road_width div 12),
					Y_Start_Axis = 0,
					X_Final_Axis = X_Start_Axis,
					Y_Final_Axis = ?Road_Length;
				south ->
					Deg = 90,
					X_Start_Axis = (?Map_size div 2) + Lane*(?Road_width div 6) + (?Road_width div 12),
					Y_Start_Axis = ?Map_size,
					X_Final_Axis = X_Start_Axis,
					Y_Final_Axis = ?Map_size - ?Road_Length
			end,
			Self = self(), {ok,PID} = gen_Car:start({X_Start_Axis,Y_Start_Axis}, {X_Final_Axis,Y_Final_Axis}, ?Car_Speed, before,Road_Direction,Self,Color), %Generating car
			ets:insert(ETS, {PID,{X_Start_Axis,Y_Start_Axis,Deg,Color,Lane}}), %Storing in DATABASE
			%Inserting to the relevant Lane queue
			case Lane of 0-> New_Q1 = Q1 ++ [PID],New_Q2=Q2,New_Q3=Q3; 1-> New_Q2 = Q2 ++ [PID],New_Q1=Q1,New_Q3=Q3; 2-> New_Q3 = Q3 ++ [PID],New_Q1=Q1,New_Q2=Q2 end,
			spawn(fun() -> timer(exp,?Lambda,Self) end), 	%Setting timeout-timer for generating car
			{noreply, {ETS,Road_Direction,{New_Q1,New_Q2,New_Q3}}};
		true ->
			Self = self(),spawn(fun() -> timer(exp,?Lambda,Self) end), 	%Setting timeout-timer for generating car
			{noreply, {ETS,Road_Direction,{Q1,Q2,Q3}}}
	end;

handle_info({rm,update,location,{X_New_Axis,Y_New_Axis},PID}, {ETS,Road_Direction,{Q1,Q2,Q3}}) ->
	[{_,{_,_,Deg,Color,Lane}}] = ets:lookup(ETS, PID),  %getting other stored info
	ets:insert(ETS, {PID,{X_New_Axis,Y_New_Axis,Deg,Color,Lane}}), %updating new location
	{display,?Display_Node}!{display,update,location,{PID,{X_New_Axis,Y_New_Axis,Deg,Color}}},  %Send update to Display
	{noreply, {ETS,Road_Direction,{Q1,Q2,Q3}}};

handle_info({rm,create,{Color,X,Y}}, {ETS,Road_Direction,{Q1,Q2,Q3}}) ->		%request from IM to generate car
	X_Start_Axis = X, Y_Start_Axis = Y,	
	case Road_Direction of														%setting up reversed start conditions
		west -> Deg = 180, X_Final_Axis = 0,Y_Final_Axis = Y;
		east -> Deg = 0, X_Final_Axis = ?Map_size, Y_Final_Axis = Y;
		north -> Deg = 90, X_Final_Axis = X, Y_Final_Axis = 0;
		south -> Deg = 270, X_Final_Axis = X, Y_Final_Axis = ?Map_size
	end,
	Self = self(), {ok,PID} = gen_Car:start({X_Start_Axis,Y_Start_Axis}, {X_Final_Axis,Y_Final_Axis}, ?Car_Speed, afterr, Road_Direction, Self,Color), %Generating car at specific Lane
	ets:insert(ETS, {PID,{X_Start_Axis,Y_Start_Axis,Deg,Color,0}}), %Storing in Database 0* it's unused
	{noreply, {ETS,Road_Direction,{Q1,Q2,Q3}}};

handle_info({rm,update,departure,PID}, {ETS,Road_Direction,{Q1,Q2,Q3}}) ->			%car is leaving road
	[{_,{_,_,_,_,Lane}}] = ets:lookup(ETS, PID),
	ets:delete(ETS, PID), %delete from DATABASE
	%delete from queue
	case Lane of 0-> New_Q1=lists:delete(PID, Q1),New_Q2=Q2,New_Q3=Q3; 1-> New_Q2=lists:delete(PID, Q2),New_Q1=Q1,New_Q3=Q3; 2-> New_Q3=lists:delete(PID, Q3),New_Q1=Q1,New_Q2=Q2 end,
	{noreply, {ETS,Road_Direction,{New_Q1,New_Q2,New_Q3}}};

handle_info({rm,update,term,PID}, {ETS,Road_Direction,{Q1,Q2,Q3}}) ->				%car finished road
	[{_,{X,Y,Ang,Color,Lane}}] = ets:lookup(ETS, PID),
	{display,?Display_Node}!{display,delete,location,{PID,{X,Y,Ang,Color}}},  %Send delete update to Display
	ets:delete(ETS, PID), %delete from DATABASE
	%delete from queue
	case Lane of 0-> New_Q1=lists:delete(PID, Q1),New_Q2=Q2,New_Q3=Q3; 1-> New_Q2=lists:delete(PID, Q2),New_Q1=Q1,New_Q3=Q3; 2-> New_Q3=lists:delete(PID, Q3),New_Q1=Q1,New_Q2=Q2 end, %delete from queue
	{noreply, {ETS,Road_Direction,{New_Q1,New_Q2,New_Q3}}};

handle_info({rm,request,rear_car_pid,PID}, {ETS,Road_Direction,{Q1,Q2,Q3}}) ->		%searching rear car
	case ets:lookup(ETS, PID) of
	[{_,{_,_,_,_,Lane}}] ->
		case Lane of 0-> Answer = search_Rear(Q1,PID);1-> Answer = search_Rear(Q2,PID);2-> Answer = search_Rear(Q3,PID) end,	%search in specific lane queue
		PID!{car,rear_car_pid,Answer}, 					%Reply with answer
		{noreply, {ETS,Road_Direction,{Q1,Q2,Q3}}};
	_->
		{noreply, {ETS,Road_Direction,{Q1,Q2,Q3}}}
	end;

handle_info({rm,terminate},{ETS,Road_Direction,{Q1,Q2,Q3}}) ->				%termination
	_ = [PID!{car,terminate}||{PID,{_,_,_,_,_}} <- ets:tab2list(ETS)],
	_ = [{display,?Display_Node}!{display,delete,location,{PID,{X,Y,Ang,Color}}}||{PID,{X,Y,Ang,Color}} <- ets:tab2list(ETS)],
	ets:delete(ETS),
	{stop, shutdown, {ETS,Road_Direction,{Q1,Q2,Q3}}}.

handle_call(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

terminate(_Reason, _State) ->
    ok.

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
timer(Mode,Par,PID) ->		%2 modes for timer: exponential and constant
	case Mode of
		const -> 
			receive
				{cancel} -> ok
			after Par -> PID!{timeout,Mode}
			end;
		exp -> 
			T = trunc(-math:log(random:uniform())/Par),
			receive
				{cancel} -> ok
			after T -> 	PID!{timeout,Mode}
			end
	end.

search_Rear([],_) -> noCar;
search_Rear([First|Rest],Car_PID) -> %serach in queue function
	if
		(Rest == []) -> 
			noCar;
		(First == Car_PID) -> 
			[Rear|_] = Rest, Rear;
		true ->
			search_Rear(Rest,Car_PID)
	end.

%%%===================================================================
%%% Debug
%%%===================================================================
%c(gen_Car),c(gen_RM).
%erl -name S@127.0.0.1 -setcookie cook