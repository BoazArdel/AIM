
-module(gen_IM).

-behaviour(gen_server).
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% defines
-define(Display_Node, 'S@10.0.0.3').
-define(Display_Module, gen_Display).
-define(RM_North_Node, 'F@10.0.0.9').
-define(RM_North_Module, gen_RM).
-define(RM_South_Node,'F@10.0.0.2').
-define(RM_South_Module, gen_RM).
-define(RM_East_Node,'F@10.0.0.8').
-define(RM_East_Module, gen_RM).
-define(RM_West_Node, 'F@10.0.0.7').
-define(RM_West_Module, gen_RM).
-define(Car_Module, gen_Car).
-define(Time_Slot, 20).
-define(Speed, 3).
-define(Slot_Counter_Mod, 600).
-define(Intersection_Edge,400).
-define(Corner_X, 550).
-define(Corner_Y, 550).
-define(Car_length, 13).
-define(Car_width, 8).
-define(HeavyPixel,5).
-define(Calc_Time,6).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
	%Database init
	ETS = ets:new(im_ets, [bag,public,named_table]), 		%Data => {Timeslot,{X,Y}} for taken
	Cars_ETS = ets:new(car_ets, [set,public,named_table]),	%DataBase => {PID,Path,Road_Destination,Color,X,Y,Deg})
	ets:insert(Cars_ETS,{slot_Counter,0}),   %initial time slot is 0
	Queue = queue:new(),				%Data => {PID,{X,Y,Deg,Color,Dest},Path,HeavyPath}, Path => {timeslot,{x,y,deg}} , HeavyPath => {timeslot,{x,y}}
	
	Self = self(),CarMover = spawn(fun() -> moveCars(ETS,Cars_ETS,Self) end),	%Setting timeout-timer for moving cars
	ets:insert(ETS,{car_move,CarMover}),   %car_move PID for termination purpose
	register(im_server,Self),
	
	%starting all servers
	?Display_Module:init(), %starting display
	rpc:call(?RM_North_Node, ?RM_North_Module, start_link, [north]),
	rpc:call(?RM_South_Node, ?RM_South_Module, start_link, [south]),
	rpc:call(?RM_West_Node, ?RM_West_Module, start_link, [west]),
	rpc:call(?RM_East_Node, ?RM_East_Module, start_link, [east]),
	
	random:seed(erlang:phash2([node()]),erlang:monotonic_time(),erlang:unique_integer()),	%individual seed
	
    {ok, {ETS,Cars_ETS,Queue}}.

handle_info({timeout,const},{ETS,Cars_ETS,Queue}) ->
	[{_,Time}] = ets:lookup(Cars_ETS, slot_Counter),  %getting time slot
	%calculating and allocating + Confirm
	NewQ = checkAndConfirm(ETS,Cars_ETS,Queue,((Time+1) rem ?Slot_Counter_Mod)),
	{noreply, {ETS,Cars_ETS,NewQ}};

handle_info({im,request,{X,Y,Deg,Color,RM_Dir},PID},{ETS,Cars_ETS,Queue}) ->
	Direction = random:uniform(3)-1,		%random turn
	if 
		(Direction==0) -> 					%determine destination
			Path = turnPath(X,Y,Deg,true),
			case RM_Dir of north -> Dest = west; south -> Dest = east; west -> Dest = south; east-> Dest = north end;
		(Direction==1) ->
			Path = turnPath(X,Y,Deg,false),
			case RM_Dir of north -> Dest = east; south -> Dest = west; west -> Dest = north; east-> Dest = south end;
		true ->
			Path = strightPath(X,Y,Deg),
			case RM_Dir of north -> Dest = south; south -> Dest = north; west -> Dest = east; east-> Dest = west end
	end,
	
	NewQ = queue:in({PID,{X,Y,Deg,Color,Dest},Path,heavyPath(Path),erlang:timestamp()},Queue),			%enter requrst to queue
	{noreply, {ETS,Cars_ETS,NewQ}};

handle_info({im,terminate},{ETS,Cars_ETS,Queue}) ->			%termination
	[{car_move,CarMover}] = ets:lookup(ETS, car_move),  %getting car mover pid
	Stat1_Result =  average([X||{_,X}<-ets:lookup(ETS,stat1)]),
	Stat2_Result =  average([X||{_,X}<-ets:lookup(ETS,stat2)]),
	io:format("Request Average Handle Time:~p[sec]~nCar Average Sojourn Time:~p[sec]~n",[Stat1_Result/1000000,Stat2_Result/1000000]),
	CarMover!{terminate},
	{north,?RM_North_Node}!{rm,terminate},			%terminating servers
	{south,?RM_South_Node}!{rm,terminate},
	{west,?RM_West_Node}!{rm,terminate},
	{east,?RM_East_Node}!{rm,terminate},
	display!{im_is_dead},
	{stop, shutdown, {ETS,Cars_ETS,Queue}};
	
handle_info(_,{ETS,Cars_ETS,Queue}) ->
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
moveCars(ETS,Cars_ETS,MasterPid) ->
	receive
		{terminate} -> 
			try  
				ets:delete(im_ets),   %deleting Database
				ets:delete(car_ets)
			catch
				_:_ -> ok
			end
	after ?Time_Slot -> 
		%Update Time slot
		[{_,Time}] = ets:lookup(Cars_ETS, slot_Counter),  %getting time slot
		ets:insert(Cars_ETS,{slot_Counter,((Time+1) rem ?Slot_Counter_Mod)}),   %initial time slot is 0
		%delete the past
		ets:delete(ETS, Time), 
		%send sync to Master
		MasterPid!{timeout,const},	
		%moving cars
		CarList = lists:delete({slot_Counter,((Time+1) rem ?Slot_Counter_Mod)}, ets:tab2list(Cars_ETS)),
		_ = [sendUpdateMove(X,Cars_ETS)||X <- CarList], %updating moves to Display + refreshing ETS
		moveCars(ETS,Cars_ETS,MasterPid)
	end.
	  
sendUpdateMove({PID,Path,Road_Direction,Color,LastX,LastY,LastDeg,Req_Time},Cars_ETS) ->
	if
		(Path==[]) -> 
			ets:insert(im_ets,{stat2,timer:now_diff(erlang:timestamp(),Req_Time)}),   %stat2 -> sojourn time
			ets:delete(Cars_ETS,PID),
			display!{display,delete,location,{PID,{LastX,LastY,LastDeg,Color}}},  %Send delete update to Display
			case Road_Direction of
						north ->	{Road_Direction,?RM_North_Node}!{rm,create,{Color,LastX,LastY}};  %create car
						south ->	{Road_Direction,?RM_South_Node}!{rm,create,{Color,LastX,LastY}};
						west ->		{Road_Direction,?RM_West_Node}!{rm,create,{Color,LastX,LastY}};
						east ->		{Road_Direction,?RM_East_Node}!{rm,create,{Color,LastX,LastY}}
			end;
		true -> 
			[{_,{X,Y,Deg}}|Rest] = Path,
			display!{display,update,location,{PID,{X,Y,Deg,Color}}},  %Send update to Display
			ets:insert(Cars_ETS,{PID,Rest,Road_Direction,Color,X,Y,Deg,Req_Time})
	end.

checkAndConfirm(ETS,Cars_ETS,Queue,Slot_Counter) ->
	case queue:is_empty(Queue) of
		true -> 
			Queue;
		false ->
			{{_,{PID,{X,Y,Deg,Color,Road_Direction},Path,HeavyPath_ETS,Req_Time}},NewQ} = queue:out(Queue),
			
			%checking collisions
			Bool = etsCrossCompareAllocation(ETS,HeavyPath_ETS,Slot_Counter,0),
			if
				(Bool == true) -> 	%approve
					ets:insert(ETS,{stat1,timer:now_diff(erlang:timestamp(),Req_Time)}),   %stat1 -> request handle time
					ets:insert(Cars_ETS,{PID,Path,Road_Direction,Color,X,Y,Deg,Req_Time}),	
					PID!{car,approved}, %approve car
					ets:insert(ETS,keyOfsset(ets:tab2list(HeavyPath_ETS),((Slot_Counter+?Calc_Time) rem ?Slot_Counter_Mod),[])), %allocating
					ets:delete(HeavyPath_ETS),																%erase heavy path
					[{_,NewTimeSlot}] = ets:lookup(Cars_ETS, slot_Counter), 								%getting time slot
					checkAndConfirm(ETS,Cars_ETS,NewQ,((NewTimeSlot+1) rem ?Slot_Counter_Mod));				%next request
				true ->       		%denied
					[{_,NewTimeSlot}] = ets:lookup(Cars_ETS, slot_Counter),									%getting time slot
					NewQ1 = checkAndConfirm(ETS,Cars_ETS,NewQ,((NewTimeSlot+1) rem ?Slot_Counter_Mod)),
					queue:in({PID,{X,Y,Deg,Color,Road_Direction},Path,HeavyPath_ETS,Req_Time},NewQ1)		%insert denied car
			end
	end.

etsCrossCompareAllocation(Main_ETS,Path_ETS,Slot_Time,Index) ->
	MainList = [{X,Y}||{_,{X,Y}}<-ets:lookup(Main_ETS, ((Slot_Time+Index) rem ?Slot_Counter_Mod))],
	PathList = [{X,Y}||{_,{X,Y}}<-ets:lookup(Path_ETS, Index)],
	if 
		(PathList == []) -> %Stop Condition
			true;
		(MainList == []) ->
			etsCrossCompareAllocation(Main_ETS,Path_ETS,Slot_Time,Index+1);
		true ->
			MutualList = lists:filter((fun(X) -> lists:member(X,MainList) end),PathList),
			%io:format("req: Mutual:~p, Main:~p, Path:~p~n",[MutualList,MainList,PathList]),
			if
				(MutualList == []) ->	etsCrossCompareAllocation(Main_ETS,Path_ETS,Slot_Time,Index+1);
				true -> false
			end
	end.

keyOfsset([],_,Ans) -> Ans; 					%adding all keys ofsset and modding the outcome
keyOfsset([{Key,{A,B}}|Rest],Offset,Ans) -> 
	keyOfsset(Rest,Offset,Ans ++ [{((Key+Offset) rem ?Slot_Counter_Mod),{A,B}}]). 
	
turnPath(X_axis,Y_axis,Car_Start_Deg,IsClockwise) ->
	case Car_Start_Deg of
		0 ->
			if
				(IsClockwise==true) ->
					R = round(math:sqrt(math:pow(X_axis-?Corner_X, 2) + math:pow(Y_axis - (?Corner_Y + ?Intersection_Edge), 2))),		%determine radius
					W = ?Speed / R, %finding angular speed
					Path = turnPath(R,W,0,0,IsClockwise,[]), %calculating path
					[{TimeSlot,{X_axis + X,Y_axis + (R-Y), Deg}} || {TimeSlot,{X,Y,Deg}} <- Path]; %shifting coordinates
				true ->
					R = round(math:sqrt(math:pow(X_axis-?Corner_X, 2) + math:pow(Y_axis - (?Corner_Y), 2))),		%determine radius
					W = ?Speed / R, %finding angular speed
					Path = turnPath(R,W,0,0,IsClockwise,[]),
					[{TimeSlot,{X_axis + X,Y_axis - (R+Y),Deg}}|| {TimeSlot,{X,Y,Deg}} <- Path]  %shifting coordinates
			end;
		90 ->
			if
				(IsClockwise==true) ->
					R = round(math:sqrt(math:pow(X_axis-(?Corner_X+?Intersection_Edge), 2) + math:pow(Y_axis - (?Corner_Y + ?Intersection_Edge), 2))),		%determine radius
					W = ?Speed / R, %finding angular speed
					Path = turnPath(R,W,0,90,IsClockwise,[]),
					[{TimeSlot,{X_axis + (R+X),Y_axis -Y,Deg}} || {TimeSlot,{X,Y,Deg}} <- Path];
				true ->
					R = round(math:sqrt(math:pow(X_axis-?Corner_X, 2) + math:pow(Y_axis - (?Corner_Y + ?Intersection_Edge), 2))),		%determine radius
					W = ?Speed / R, %finding angular speed
					Path = turnPath(R,W,0,90,IsClockwise,[]),
					[{TimeSlot,{X_axis - (R-X),Y_axis - Y,Deg}}|| {TimeSlot,{X,Y,Deg}} <- Path]
			end;
		180 ->
			if
				(IsClockwise==true) ->
					R = round(math:sqrt(math:pow(X_axis-(?Corner_X+?Intersection_Edge), 2) + math:pow(Y_axis - (?Corner_Y), 2))),		%determine radius
					W = ?Speed / R, %finding angular speed
					Path = turnPath(R,W,0,180,IsClockwise,[]),
					[{TimeSlot,{X_axis + X,Y_axis -(R+Y),Deg}} || {TimeSlot,{X,Y,Deg}} <- Path];
				true ->
					R = round(math:sqrt(math:pow(X_axis-(?Corner_X+?Intersection_Edge), 2) + math:pow(Y_axis - (?Corner_Y + ?Intersection_Edge), 2))),		%determine radius
					W = ?Speed / R, %finding angular speed
					Path = turnPath(R,W,0,180,IsClockwise,[]),
					[{TimeSlot,{X_axis + X,Y_axis + (R-Y),Deg}}|| {TimeSlot,{X,Y,Deg}} <- Path]
			end;
		270 ->
			if
				(IsClockwise==true) ->
					R = round(math:sqrt(math:pow(X_axis-(?Corner_X), 2) + math:pow(Y_axis - (?Corner_Y), 2))),		%determine radius
					W = ?Speed / R, %finding angular speed
					Path = turnPath(R,W,0,270,IsClockwise,[]),
					[{TimeSlot,{X_axis - (R-X),Y_axis - Y,Deg}} || {TimeSlot,{X,Y,Deg}} <- Path];
				true ->
					R = round(math:sqrt(math:pow(X_axis-(?Corner_X+?Intersection_Edge), 2) + math:pow(Y_axis - (?Corner_Y), 2))),		%determine radius
					W = ?Speed / R, %finding angular speed
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
					X = round(R*math:cos(-W*TimeSlot + degToRad(90 + Car_Start_Deg))),	%polar coordinates path calculation
					Y = round(R*math:sin(-W*TimeSlot + degToRad(90 + Car_Start_Deg))),
					turnPath(R,W,TimeSlot+1,Car_Start_Deg,IsClockwise,Path++[{TimeSlot,{X,Y,mod(round(-radToDeg(W*TimeSlot) + Car_Start_Deg) , 360)}}]);
				false ->
					X = round(R*math:cos(W*TimeSlot + degToRad(-90 + Car_Start_Deg))),
					Y = round(R*math:sin(W*TimeSlot + degToRad(-90 + Car_Start_Deg))),
					turnPath(R,W,TimeSlot+1,Car_Start_Deg,IsClockwise,Path++[{TimeSlot,{X,Y,mod(round(radToDeg(W*TimeSlot) + Car_Start_Deg) , 360)}}])
			end
	end.

strightPath(X_axis,Y_axis,Angle) -> %calculating stright path
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

%% Find lane recursivly 
heavyPath(List)-> ETS = ets:new(heavy,[bag]), heavyPath(List,ETS).
heavyPath([],ETS)-> ETS;   %stop condition
heavyPath([{TimeSlot,{X,Y,Angle}}|Rest],ETS)-> fillCoor((X div ?HeavyPixel),(Y div ?HeavyPixel),Angle,TimeSlot,ETS),heavyPath(Rest,ETS).

%%Find specific coordinate occuupid area 
fillCoor(X_front,Y_front,Angle,TimeSlot,ETS)->
	RadAngle=degToRad(Angle),
	X_rear=X_front-?Car_length*math:cos(RadAngle),%rear middle
	Y_rear=Y_front-?Car_length*math:sin(RadAngle),%rear middle
	Front_R={round(X_front-(?Car_width/2)*math:sin(RadAngle)),round(Y_front+(?Car_width/2)*math:cos(RadAngle))},
	Front_L={round(X_front+(?Car_width/2)*math:sin(RadAngle)),round(Y_front-(?Car_width/2)*math:cos(RadAngle))},
	Rear_R={round(X_rear-(?Car_width/2)*math:sin(RadAngle)),round(Y_rear+(?Car_width/2)*math:cos(RadAngle))},
	Rear_L={round(X_rear+(?Car_width/2)*math:sin(RadAngle)),round(Y_rear-(?Car_width/2)*math:cos(RadAngle))},
	connectDots(Rear_R,Front_R,TimeSlot,ETS),connectDots(Front_R,Front_L,TimeSlot,ETS),connectDots(Front_L,Rear_L,TimeSlot,ETS),connectDots(Rear_L,Rear_R,TimeSlot,ETS).
	%turn:heavyPath(turn:turnPath(850, 725, 180, true)).

%%Find edges
connectDots({X1,Y1},{X2,Y2},TimeSlot,ETS)->
	case (X1-X2) of  
		0-> 
			M = (Y2-Y1)/(X2-(X1-1)),
			N = (Y1*X2-Y2*X1)/(X2-(X1-1));
		_->
			M = (Y2-Y1)/(X2-X1),
			N = (Y1*X2-Y2*X1)/(X2-X1)
	end,
	[ets:insert(ETS, {TimeSlot,{X,round(M*X+N)}})||X<-lists:seq(lists:min([X1,X2]), lists:max([X1,X2]))].%all line dots

degToRad(Deg)->
	(math:pi()*(mod(Deg , 360)))/180.
radToDeg(Rad)->
	mod(round((Rad*180)/math:pi()),360).

mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_) -> 0.

average(X) ->
        average(X, 0, 0).
average([], Length, Sum) ->
        Sum / Length;
average([H|T], Length, Sum) ->
        average(T, Length + 1, Sum + H).

%%%===================================================================
%%% Debug
%%%===================================================================
%c(gen_Display),c(gen_IM),gen_IM:start_link().
%erl -name S@127.0.0.1 -setcookie cook