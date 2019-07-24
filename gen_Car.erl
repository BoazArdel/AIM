%%%-------------------------------------------------------------------
%%% @author Boaz Ardel <boaz@ubuntu>
%%% @copyright (C) 2015, Boaz Ardel
%%% @doc
%%%
%%% @end
%%% Created : 22 Jul 2015 by Boaz Ardel <boaz@ubuntu>
%%%-------------------------------------------------------------------
-module(gen_Car).
-behaviour(gen_fsm).

%% API
-export([start/7]).

%% gen_fsm callbacks
-export([init/1, move/2, handle_info/3, terminate/3, wait/2, stop/2,
		 code_change/4, handle_sync_event/4, handle_event/3]).

%% defines
-define(IM_SERVER, 'S@127.0.0.1').
-define(IM_MODULE, gen_IM).
-define(ReRequest, 150).
-define(Car_Space,60).
-define(Car_Speed,3).

%%%===================================================================
%%% API
%%%===================================================================
%%%@doc Mode - after_t/before, 1/Speed - ms/Pixel 
start({X_Start_Axis,Y_Start_Axis},{X_Dest_Axis,Y_Dest_Axis},Speed,Mode,RM_Direction,RM_PID,Color) ->
	gen_fsm:start(?MODULE, [{X_Start_Axis,Y_Start_Axis},{X_Dest_Axis,Y_Dest_Axis},(Speed div ?Car_Speed),Mode,RM_Direction,RM_PID,Color], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([{X_Start_Axis,Y_Start_Axis},{X_Dest_Axis,Y_Dest_Axis},Speed,Mode,RM_Direction,RM_PID,Color]) ->
	X_Axis = X_Start_Axis,Y_Axis = Y_Start_Axis,
	X_Stop_Axis = false,Y_Stop_Axis = false,			%Stop_Flag = false
	RM_PID!{rm,update,location,{X_Axis,Y_Axis},self()},	%Update Location to RM
	%io:format("->~p~n",[{rm,update,location,{X_Axis,Y_Axis},self()}]), 
	Self = self(), spawn(fun() -> timer(const,Speed,Self) end), 	%Setting timeout-timer for movemnt
	{ok, move, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}}.

handle_event(stop, _StateName, StateData) ->
    {stop, shutdown, StateData};

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}. 

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(_Reason, _StateName, _State) ->
	ok.

%%%===================================================================
%%% gen_fsm States
%%%===================================================================
move({timeout,const},{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}) ->
	if
		(X_Axis < X_Dest_Axis) -> X_New_Axis = X_Axis + 1;			%move toward Destination on X_Axis
		(X_Axis > X_Dest_Axis) -> X_New_Axis = X_Axis - 1;
		true -> X_New_Axis = X_Axis
	end,
	if
		(Y_Axis < Y_Dest_Axis) -> Y_New_Axis = Y_Axis + 1;			%move toward Destination on Y_Axis
		(Y_Axis > Y_Dest_Axis) -> Y_New_Axis = Y_Axis - 1;
		true -> Y_New_Axis = Y_Axis
	end,
	
	case (abs((X_Dest_Axis - X_New_Axis)+(Y_Dest_Axis-Y_New_Axis)) rem ?Car_Speed) of
		0 -> 
			RM_PID!{rm,update,location,{X_Axis,Y_Axis},self()};	%Update Location to RM
			%io:format("->~p~n",[{rm,update,location,{X_Axis,Y_Axis},self()}]), 
		_ -> 
			ok
	end,

	case {X_Stop_Axis,Y_Stop_Axis} of
		{false,false} -> 
			if
				((X_New_Axis == X_Dest_Axis) and (Y_New_Axis == Y_Dest_Axis)) ->
					if
						(Mode == before) ->
							RM_PID!{rm,request,rear_car_pid,self()},  %request RM for rear car pid
							%io:format("->~p~n",[{rm,request,rear_car_pid,self()}]),
							case RM_Direction of west -> Deg = 0;east -> Deg = 180;north -> Deg = 270;south -> Deg = 90 end,
							{im_server,?IM_SERVER}!{im,request,{X_New_Axis,Y_New_Axis,Deg,Color,RM_Direction},self()},  %Send request to IM
							%io:format("->~p~n",[{im,request,{X_New_Axis,Y_New_Axis},self()}]),
							{next_state, wait, {X_New_Axis,Y_New_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,noCar,Color}};
						true -> 
							RM_PID!{rm,update,term,self()},  %Send terminate update RM
							%io:format("->~p~n",[{rm,update,term,self()}]),
							gen_fsm:send_all_state_event(self(), stop), %Terminating
							{next_state, stop, {X_New_Axis,Y_New_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,noCar,Color}}
					end;
				true -> 
					Self = self(), spawn(fun() -> timer(const,Speed,Self) end), 	%Setting timeout-timer for movemnt
					{next_state, move, {X_New_Axis,Y_New_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}}
			end;
				
		_Else ->
			if
				((X_New_Axis == X_Stop_Axis) and (Y_New_Axis == Y_Stop_Axis)) ->
							RM_PID!{rm,request,rear_car_pid,self()},  %request RM for rear car pid
							%io:format("->~p~n",[{rm,request,rear_car_pid,self()}]),
							{next_state, stop, {X_New_Axis,Y_New_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,noCar,Color}};
				true ->
					Self = self(), spawn(fun() -> timer(const,Speed,Self) end), 	%Setting timeout-timer for movemnt
					{next_state, move, {X_New_Axis,Y_New_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}}
			end
	end;
move({car,update,stop,{X_New_Stop_Axis,Y_New_Stop_Axis}},{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,_,_,Speed,Mode,RM_Direction,RM_PID,Color}) ->
	{next_state, move, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_New_Stop_Axis,Y_New_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}}; %Updating Stop Location
move({car,update,continue},{X_New_Axis,Y_New_Axis,X_Dest_Axis,Y_Dest_Axis,_,_,Speed,Mode,RM_Direction,RM_PID,Color}) ->
	{next_state, move,{X_New_Axis,Y_New_Axis,X_Dest_Axis,Y_Dest_Axis,false,false,Speed,Mode,RM_Direction,RM_PID,Color}};  %Disabling Stop Location
move({car,rear_car_pid, Rear_Car_PID},{X_New_Axis,Y_New_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}) -> %Updating rear car
		if 
		(Rear_Car_PID == noCar) -> 
			{next_state, move,{X_New_Axis,Y_New_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}};
		true -> 
			Rear_Car_PID!{car,update,continue},		%letting rear car to continue
			%io:format("->~p~n",[{car,update,continue}]),
			{next_state, move,{X_New_Axis,Y_New_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}}
	end;
move({timeout,exp},{X_New_Axis,Y_New_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}) ->
	{next_state, move,{X_New_Axis,Y_New_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}}; %Ignoring
move({car,terminate},_) ->
	gen_fsm:send_all_state_event(self(), stop). %Terminating

%%%//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
wait({car,rear_car_pid, Rear_Car_PID},{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,_,Color}) ->
	case Rear_Car_PID of
		noCar -> 
			Self = self(), spawn(fun() -> timer(exp,?ReRequest,Self) end), 	%for checking if new car has generated
			{next_state, wait, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,noCar,Color}};
		_Else -> 
			case RM_Direction of							%determine space from car
				west -> Space_X = -?Car_Space,Space_Y = 0;
				east -> Space_X = ?Car_Space,Space_Y = 0;
				north -> Space_X = 0,Space_Y = -?Car_Space;
				south -> Space_X = 0,Space_Y = ?Car_Space
			end,
			Rear_Car_PID!{car,update,stop, {X_Axis + Space_X,Y_Axis + Space_Y}},				%Sending stop location to rear car
			{next_state, wait, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}}
	end;
wait({timeout,exp},{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}) ->
	RM_Direction,RM_PID!{rm,request,rear_car_pid,self()},  %request RM for rear car pid
	%io:format("->~p~n",[{rm,request,rear_car_pid,self()}]),
	{next_state, wait, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}};
wait({car,approved},{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}) ->
	
	if 
		(Rear_Car_PID =/= noCar) -> 
			Rear_Car_PID!{car,update,continue},		%letting rear car to continue
			%io:format("->~p~n",[{car,update,continue}]),
			RM_PID!{rm,update,departure,self()},		%Notifying departure to RM
			%io:format("->~p~n",[{rm,update,departure,self()}]),
			gen_fsm:send_all_state_event(self(), stop); %Terminating
		true -> 
			RM_PID!{rm,update,departure,self()},		%Notifying departure to RM
			%io:format("->~p~n",[{rm,update,departure,self()}]),
			gen_fsm:send_all_state_event(self(), stop) %Terminating
	end,
	{next_state, wait, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}};
wait({car,terminate},_) ->
	gen_fsm:send_all_state_event(self(), stop). %Terminating

%%%//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
stop({car,rear_car_pid, Rear_Car_PID},{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,_,Color}) ->
	case Rear_Car_PID of
		noCar -> 
			Self = self(), spawn(fun() -> timer(exp,?ReRequest,Self) end), 	%for checking if new car has generated
			{next_state, stop, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,noCar,Color}};
		_Else -> 
			case RM_Direction of							%determine space from car
				west -> Space_X = -?Car_Space,Space_Y = 0;
				east -> Space_X = ?Car_Space,Space_Y = 0;
				north -> Space_X = 0,Space_Y = -?Car_Space;
				south -> Space_X = 0,Space_Y = ?Car_Space
			end,
			Rear_Car_PID!{car,update,stop,{X_Axis + Space_X,Y_Axis + Space_Y}},				%Sending stop location to rear car
			{next_state, stop, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}}
	end;
stop({timeout,exp},{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}) ->
	RM_PID!{rm,request,rear_car_pid,self()},  %request RM for rear car pid
	%io:format("->~p~n",[{rm,request,rear_car_pid,self()}]),
	{next_state, stop, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}};
stop({car,update,continue},{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,_,_,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}) ->
	timer:sleep(200),  %Human Delay
	if 
		(Rear_Car_PID =/= noCar) -> 
			Rear_Car_PID!{car,update,continue},		%letting rear car to continue
			%io:format("->~p~n",[{car,update,continue}]),
			Self = self(), spawn(fun() -> timer(const,Speed,Self) end), 	%Setting timeout-timer for movemnt
			RM_PID!{rm,request,rear_car_pid,self()},  %request RM for rear car pid
			{next_state, move, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,false,false,Speed,Mode,RM_Direction,RM_PID,Color}};
		true ->
			Self = self(), spawn(fun() -> timer(const,Speed,Self) end), 	%Setting timeout-timer for movemnt
			RM_PID!{rm,request,rear_car_pid,self()},  %request RM for rear car pid
			{next_state, move, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,false,false,Speed,Mode,RM_Direction,RM_PID,Color}}	
	end;
stop({car,update,stop,{X_New_Stop_Axis,Y_New_Stop_Axis}},{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}) ->
	if
		((X_New_Stop_Axis =/= X_Stop_Axis) or (Y_New_Stop_Axis =/= Y_Stop_Axis)) ->
				if 
					(Rear_Car_PID =/= noCar) -> 
						Rear_Car_PID!{car,update,continue},		%letting rear car to continue
						%io:format("->~p~n",[{car,update,continue}]),
						Self = self(), spawn(fun() -> timer(const,Speed,Self) end), 	%Setting timeout-timer for movemnt
						RM_PID!{rm,request,rear_car_pid,self()},  %request RM for rear car pid
						{next_state, move, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_New_Stop_Axis,Y_New_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}};
					true ->
						Self = self(), spawn(fun() -> timer(const,Speed,Self) end), 	%Setting timeout-timer for movemnt
						RM_PID!{rm,request,rear_car_pid,self()},  %request RM for rear car pid
						{next_state, move, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_New_Stop_Axis,Y_New_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}}	
				end;		
		true ->
			{next_state, stop, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_New_Stop_Axis,Y_New_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}}
	end;
stop({car,terminate},_) ->
	gen_fsm:send_all_state_event(self(), stop). %Terminating

%%%===================================================================
%%% gen_fsm External Events
%%%===================================================================
handle_info({timeout,const}, move,{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}) ->
	gen_fsm:send_event(self(), {timeout,const}),
    {next_state, move, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}};
handle_info({car,update,stop,{X_New_Stop_Axis,Y_New_Stop_Axis}}, move,{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}) ->
	gen_fsm:send_event(self(), {car,update,stop,{X_New_Stop_Axis,Y_New_Stop_Axis}}),
    {next_state, move, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}};
handle_info({timeout,exp}, move,{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}) ->
	gen_fsm:send_event(self(), {timeout,exp}),
    {next_state, move, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}};
handle_info({car,terminate}, move,{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}) ->
	gen_fsm:send_event(self(), {car,terminate}),
    {next_state, move, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}};
handle_info({car,update,continue}, move,{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}) ->
    gen_fsm:send_event(self(), {car,update,continue}),
	{next_state, move, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}};
handle_info({car,rear_car_pid, Rear_Car_PID}, move,{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}) ->
	gen_fsm:send_event(self(),{car,rear_car_pid, Rear_Car_PID}),
    {next_state, move, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Color}};

handle_info({car,rear_car_pid, Rear_Car_PID}, wait,{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,_,Color}) ->
	gen_fsm:send_event(self(), {car,rear_car_pid, Rear_Car_PID}),
    {next_state, wait, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}};
handle_info({timeout,exp}, wait,{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}) ->
	gen_fsm:send_event(self(), {timeout,exp}),
    {next_state, wait, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}};
handle_info({car,approved}, wait,{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}) ->
	gen_fsm:send_event(self(), {car,approved}),
    {next_state, wait, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}};
handle_info({car,terminate}, wait,{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}) ->
	gen_fsm:send_event(self(), {car,terminate}),
    {next_state, wait, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}};
handle_info({car,update,stop,{_,_}}, wait,{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}) ->
    {next_state, wait, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}};	%Ignor
handle_info({car,update,continue}, wait,{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}) ->
    {next_state, wait, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}};	%Ignor

handle_info({car,rear_car_pid, Rear_Car_PID}, stop,{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,_,Color}) ->
	gen_fsm:send_event(self(), {car,rear_car_pid, Rear_Car_PID}),
    {next_state, stop, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}};
handle_info({timeout,exp}, stop,{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}) ->
	gen_fsm:send_event(self(), {timeout,exp}),
    {next_state, stop, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}};
handle_info({car,update,continue}, stop,{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}) ->
	gen_fsm:send_event(self(), {car,update,continue}),
    {next_state, stop, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}};
handle_info({car,update,stop,{X_New_Stop_Axis,Y_New_Stop_Axis}}, stop,{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}) ->
	gen_fsm:send_event(self(), {car,update,stop,{X_New_Stop_Axis,Y_New_Stop_Axis}}),
    {next_state, stop, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}};
handle_info({car,terminate}, stop,{X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}) ->
	gen_fsm:send_event(self(), {car,terminate}),
    {next_state, stop, {X_Axis,Y_Axis,X_Dest_Axis,Y_Dest_Axis,X_Stop_Axis,Y_Stop_Axis,Speed,Mode,RM_Direction,RM_PID,Rear_Car_PID,Color}}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
timer(Mode,Par,PID) ->
	receive
		{cancel} -> ok
	after Par -> PID!{timeout,Mode}
	end.

%%%===================================================================
%%% Debug
%%%===================================================================
%{ok,PID} = gen_Car:start({0,0}, {200,200}, 100, before, self()).
%PID!{car,rear_car_pid,noCar}.
%PID!{car,approved}.
%PID!{car,update,stop,{150,150}}.
%PID!{car,update,continue}.
%PID!{car,terminate}.
%process_info(PID).