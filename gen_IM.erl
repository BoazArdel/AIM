%%%-------------------------------------------------------------------
%%% @author Boaz Ardel <boaz@ubuntu>
%%% @copyright (C) 2015, Boaz Ardel
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2015 by Boaz Ardel <boaz@ubuntu>
%%%-------------------------------------------------------------------
-module(gen_IM).

-behaviour(gen_server).
%% API
-export([start_link/0,rpc_Call/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% defines
-define(Display_Node, 'S@127.0.0.1').
-define(Display_Module, noets).
-define(RM_North_Node, 'F@127.0.0.1').
-define(RM_North_Module, gen_RM).
-define(RM_South_Node, 'F@127.0.0.1').
-define(RM_South_Module, gen_RM).
-define(RM_East_Node, 'F@127.0.0.1').
-define(RM_East_Module, gen_RM).
-define(RM_West_Node, 'F@127.0.0.1').
-define(RM_West_Module, gen_RM).
-define(Car_Mudule, gen_Car).
-define(Time_Slot, 20).
-define(Speed, 3).
-define(Intersection_Edge,400).
-define(Corner_X, 450).
-define(Corner_Y, 450).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
	ETS = ets:new(im_ets, [set]), 		%Data => {Timeslot,{X,Y}} for taken
	Cars_ETS = ets:new(car_ets, [set]),	%DataBase => {PID,[toMove||RestPath]}
	Queue = queue:new(),				%Data => {PID,{X,Y,Deg},Path,HeavyPath}
	Self = self(), spawn(fun() -> timer(const,?Time_Slot,Self) end), 	%Setting timeout-timer for moving cars
	register(im_server,Self),
    {ok, {ETS,Cars_ETS,Queue}}.

handle_info({timeout,const},{ETS,Cars_ETS,Queue}) ->
	%moving cars
	_ = [sendUpdateMove(X,Cars_ETS)||X <- ets:tab2list(Cars_ETS)], %updating moves to Display + refreshing ETS
	%calculating and allocating + Confirm
	NewQ = checkAndConfirm(ETS,Cars_ETS,Queue),
	Self = self(), spawn(fun() -> timer(const,?Time_Slot,Self) end), 	%Setting timeout-timer for moving cars
	{noreply, {ETS,Cars_ETS,NewQ}};

handle_info({im,request,{X,Y,Deg,Color,RM_Dir},PID},{ETS,Cars_ETS,Queue}) ->
	Direction = random:uniform(3)-1,
	if 
		(Direction==0) -> 
			Path = turnPath(X,Y,Deg,true),
			case RM_Dir of north -> Dest = west; south -> Dest = east; west -> Dest = south; east-> Dest = north end;
		(Direction==1) ->
			Path = turnPath(X,Y,Deg,false),
			case RM_Dir of north -> Dest = east; south -> Dest = west; west -> Dest = north; east-> Dest = south end;
		true ->
			Path = strightPath(X,Y,Deg),
			case RM_Dir of north -> Dest = south; south -> Dest = north; west -> Dest = east; east-> Dest = west end
	end,
	%NewQ = queue:in_r({PID,{X,Y,Deg},Path,heavyPath(Path)}),
	NewQ = queue:in_r({PID,{X,Y,Deg,Color,Dest},Path},Queue),
	{noreply, {ETS,Cars_ETS,NewQ}};

handle_info(_,{ETS,Cars_ETS,Queue}) ->
	io:format("sad"),
	{noreply, {ETS,Cars_ETS,Queue}}.

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
sendUpdateMove({PID,Path,Road_Direction,Color,LastX,LastY,LastDeg},Cars_ETS) ->
	if
		(Path==[]) -> 
			ets:delete(Cars_ETS,PID),
			rpc:call(?Display_Node, ?Display_Module, rpc_Call, [{display,delete,location,{PID,{LastX,LastY,LastDeg,Color}}}]),  %Send delete update to Display
			case Road_Direction of %!!
				north ->	rpc:call(?RM_North_Node, ?RM_North_Module, rpc_Call, [{rm,create,{Color,LastX,LastY,Road_Direction}}]);  %create car
				south ->	rpc:call(?RM_South_Node, ?RM_South_Module, rpc_Call, [{rm,create,{Color,LastX,LastY,Road_Direction}}]);
				west ->		rpc:call(?RM_West_Node, ?RM_West_Module, rpc_Call, [{rm,create,{Color,LastX,LastY,Road_Direction}}]);
				east ->		rpc:call(?RM_East_Node, ?RM_East_Module, rpc_Call, [{rm,create,{Color,LastX,LastY,Road_Direction}}])
			end;
		true -> 
			[{_,{X,Y,Deg}}|Rest] = Path,
			rpc:call(?Display_Node, ?Display_Module, rpc_Call, [{display,update,location,{PID,{X,Y,Deg,Color}}}]),  %Send update to Display
			ets:insert(Cars_ETS,{PID,Rest,Road_Direction,Color,X,Y,Deg})
	end.

checkAndConfirm(ETS,Cars_ETS,Queue) ->
	case queue:is_empty(Queue) of
		true -> 
			Queue;
		false ->
			{{_,{PID,{X,Y,Deg,Color,Road_Direction},Path}},NewQ} = queue:out(Queue),
			
			%Algorithm
			
			%approve all
			
			ets:insert(Cars_ETS,{PID,Path,Road_Direction,Color,X,Y,Deg}),
			case Road_Direction of
				north ->	rpc:call(?RM_North_Node, ?Car_Mudule, rpc_Call, [{car,approved,PID}]);  %Confirm 
				south ->	rpc:call(?RM_South_Node, ?Car_Mudule, rpc_Call, [{car,approved,PID}]);
				west ->		rpc:call(?RM_West_Node, ?Car_Mudule, rpc_Call, [{car,approved,PID}]);
				east ->		rpc:call(?RM_East_Node, ?Car_Mudule, rpc_Call, [{car,approved,PID}])
			end,
			NewQ2 = checkAndConfirm(ETS,Cars_ETS,NewQ),NewQ2
	end.
	
turnPath(X_axis,Y_axis,Car_Start_Deg,IsClockwise) ->
	case Car_Start_Deg of
		0 ->
			if
				(IsClockwise==true) ->
					R = round(math:sqrt(math:pow(X_axis-?Corner_X, 2) + math:pow(Y_axis - (?Corner_Y + ?Intersection_Edge), 2))),
					W = ?Speed / R,
					Path = turnPath(R,W,0,0,IsClockwise,[]),
					[{TimeSlot,{X_axis + X,Y_axis + (R-Y), Deg}} || {TimeSlot,{X,Y,Deg}} <- Path];
				true ->
					R = round(math:sqrt(math:pow(X_axis-?Corner_X, 2) + math:pow(Y_axis - (?Corner_Y), 2))),
					W = ?Speed / R,
					Path = turnPath(R,W,0,0,IsClockwise,[]),
					[{TimeSlot,{X_axis + X,Y_axis - (R+Y),Deg}}|| {TimeSlot,{X,Y,Deg}} <- Path]
			end;
		90 ->
			if
				(IsClockwise==true) ->
					R = round(math:sqrt(math:pow(X_axis-(?Corner_X+?Intersection_Edge), 2) + math:pow(Y_axis - (?Corner_Y + ?Intersection_Edge), 2))),
					W = ?Speed / R,
					Path = turnPath(R,W,0,90,IsClockwise,[]),
					[{TimeSlot,{X_axis + (R+X),Y_axis -Y,Deg}} || {TimeSlot,{X,Y,Deg}} <- Path];
				true ->
					R = round(math:sqrt(math:pow(X_axis-?Corner_X, 2) + math:pow(Y_axis - (?Corner_Y + ?Intersection_Edge), 2))),
					W = ?Speed / R,
					Path = turnPath(R,W,0,90,IsClockwise,[]),
					[{TimeSlot,{X_axis - (R-X),Y_axis - Y,Deg}}|| {TimeSlot,{X,Y,Deg}} <- Path]
			end;
		180 ->
			if
				(IsClockwise==true) ->
					R = round(math:sqrt(math:pow(X_axis-(?Corner_X+?Intersection_Edge), 2) + math:pow(Y_axis - (?Corner_Y), 2))),
					W = ?Speed / R,
					Path = turnPath(R,W,0,180,IsClockwise,[]),
					[{TimeSlot,{X_axis + X,Y_axis -(R+Y),Deg}} || {TimeSlot,{X,Y,Deg}} <- Path];
				true ->
					R = round(math:sqrt(math:pow(X_axis-(?Corner_X+?Intersection_Edge), 2) + math:pow(Y_axis - (?Corner_Y + ?Intersection_Edge), 2))),
					W = ?Speed / R,
					Path = turnPath(R,W,0,180,IsClockwise,[]),
					[{TimeSlot,{X_axis + X,Y_axis + (R-Y),Deg}}|| {TimeSlot,{X,Y,Deg}} <- Path]
			end;
		270 ->
			if
				(IsClockwise==true) ->
					R = round(math:sqrt(math:pow(X_axis-(?Corner_X), 2) + math:pow(Y_axis - (?Corner_Y), 2))),
					W = ?Speed / R,
					Path = turnPath(R,W,0,270,IsClockwise,[]),
					[{TimeSlot,{X_axis - (R-X),Y_axis - Y,Deg}} || {TimeSlot,{X,Y,Deg}} <- Path];
				true ->
					R = round(math:sqrt(math:pow(X_axis-(?Corner_X+?Intersection_Edge), 2) + math:pow(Y_axis - (?Corner_Y), 2))),
					W = ?Speed / R,
					Path = turnPath(R,W,0,270,IsClockwise,[]),
					[{TimeSlot,{X_axis + (R+X),Y_axis -Y,Deg}}|| {TimeSlot,{X,Y,Deg}} <- Path]
			end
	end.
turnPath(R,W,TimeSlot,Car_Start_Deg,IsClockwise,Path) ->
	%io:format("T:~p W:~p TN:~p~n",[TimeSlot,W,TimeSlot*W]),
	if
		(TimeSlot*W >= 3.1415926/2) -> %TimeSlot*W  in Rads
			Path;
		true ->
			case IsClockwise of
				true ->
					X = round(R*math:cos(-W*TimeSlot + degToRad(90 + Car_Start_Deg))),
					Y = round(R*math:sin(-W*TimeSlot + degToRad(90 + Car_Start_Deg))),
					turnPath(R,W,TimeSlot+1,Car_Start_Deg,IsClockwise,Path++[{TimeSlot,{X,Y,mod(round(-radToDeg(W*TimeSlot) + Car_Start_Deg) , 360)}}]);
				false ->
					X = round(R*math:cos(W*TimeSlot + degToRad(-90 + Car_Start_Deg))),
					Y = round(R*math:sin(W*TimeSlot + degToRad(-90 + Car_Start_Deg))),
					turnPath(R,W,TimeSlot+1,Car_Start_Deg,IsClockwise,Path++[{TimeSlot,{X,Y,mod(round(radToDeg(W*TimeSlot) + Car_Start_Deg) , 360)}}])
			end
	end.

strightPath(X_axis,Y_axis,Angle) -> 
	case Angle of
		0 ->
			[{TimeSlot,{X_axis + ?Speed*TimeSlot,Y_axis,0}}||TimeSlot<-lists:seq(0,?Intersection_Edge div ?Speed)];
		90 ->
			[{TimeSlot,{X_axis ,Y_axis - ?Speed*TimeSlot ,90}}||TimeSlot<-lists:seq(0,?Intersection_Edge div ?Speed)];
		180 ->
			[{TimeSlot,{X_axis - ?Speed*TimeSlot,Y_axis,180}}||TimeSlot<-lists:seq(0,?Intersection_Edge div ?Speed)];
		270 ->
			[{TimeSlot,{X_axis ,Y_axis + ?Speed*TimeSlot ,270}}||TimeSlot<-lists:seq(0,?Intersection_Edge div ?Speed)]
	end.

timer(Mode,Par,PID) ->
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

degToRad(Deg)->
	(math:pi()*(mod(Deg , 360)))/180.
radToDeg(Rad)->
	mod(round((Rad*180)/math:pi()),360).

rpc_Call(Msg) ->
	im_server!Msg.

mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_) -> 0.

%%%===================================================================
%%% Debug
%%%===================================================================
