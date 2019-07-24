%% @author snir
%% @doc @todo Add description to turn.


-module(turn).

%% defines
-define(Time_Slot, 20).
-define(Speed, (3)).
-define(Intersection_Edge,400).
-define(Corner_X, 450).
-define(Corner_Y, 450).

-define(Car_length, 51).
-define(Car_width, 25).

%% ====================================================================
%% API functions
%% ====================================================================
-export([turnPath/4,degToRad/1,heavyPath/1,fillCoor/4,strightPath/3]).

%% ====================================================================
%% Internal functions
%% ====================================================================
degToRad(Deg)->
	(math:pi()*(mod(Deg , 360)))/180.
radToDeg(Rad)->
	mod(round((Rad*180)/math:pi()),360).

%%return curved path given start point,start angle and diraction
turnPath(X_axis,Y_axis,Car_Start_Angle,IsClockwise) ->
	case Car_Start_Angle of
		0 ->
			if
				(IsClockwise==true) ->%clockwise
					R = round(math:sqrt(math:pow(X_axis-?Corner_X, 2) + math:pow(Y_axis - (?Corner_Y + ?Intersection_Edge), 2))),%radius of curve
					W = ?Speed / R,% angle speed
					Path = turnPath(R,W,0,0,IsClockwise,[]),%calc all path
					[{TimeSlot,{X_axis + X,Y_axis + (R-Y), Angle}} || {TimeSlot,{X,Y,Angle}} <- Path];%fix offset for all path ]dots
				true -> %Counterclockwise 
					R = round(math:sqrt(math:pow(X_axis-?Corner_X, 2) + math:pow(Y_axis - (?Corner_Y), 2))),% angle speed
					W = ?Speed / R,% angle speed
					Path = turnPath(R,W,0,0,IsClockwise,[]),%calc all path
					[{TimeSlot,{X_axis + X,Y_axis - (R+Y),Angle}}|| {TimeSlot,{X,Y,Angle}} <- Path]%fix offset for all path dots
			end;
		90 ->
			if
				(IsClockwise==true) ->
					R = round(math:sqrt(math:pow(X_axis-(?Corner_X+?Intersection_Edge), 2) + math:pow(Y_axis - (?Corner_Y + ?Intersection_Edge), 2))),
					W = ?Speed / R,
					Path = turnPath(R,W,0,90,IsClockwise,[]),
					[{TimeSlot,{X_axis + (R+X),Y_axis -Y,Angle}} || {TimeSlot,{X,Y,Angle}} <- Path];
				true ->
					R = round(math:sqrt(math:pow(X_axis-?Corner_X, 2) + math:pow(Y_axis - (?Corner_Y + ?Intersection_Edge), 2))),
					W = ?Speed / R,
					Path = turnPath(R,W,0,90,IsClockwise,[]),
					[{TimeSlot,{X_axis - (R-X),Y_axis - Y,Angle}}|| {TimeSlot,{X,Y,Angle}} <- Path]
			end;
		180 ->
			if
				(IsClockwise==true) ->
					R = round(math:sqrt(math:pow(X_axis-(?Corner_X+?Intersection_Edge), 2) + math:pow(Y_axis - (?Corner_Y), 2))),
					W = ?Speed / R,
					Path = turnPath(R,W,0,180,IsClockwise,[]),
					[{TimeSlot,{X_axis + X,Y_axis -(R+Y),Angle}} || {TimeSlot,{X,Y,Angle}} <- Path];
				true ->
					R = round(math:sqrt(math:pow(X_axis-(?Corner_X+?Intersection_Edge), 2) + math:pow(Y_axis - (?Corner_Y + ?Intersection_Edge), 2))),
					W = ?Speed / R,
					Path = turnPath(R,W,0,180,IsClockwise,[]),
					[{TimeSlot,{X_axis + X,Y_axis + (R-Y),Angle}}|| {TimeSlot,{X,Y,Angle}} <- Path]
			end;
		270 ->
			if
				(IsClockwise==true) ->
					R = round(math:sqrt(math:pow(X_axis-(?Corner_X), 2) + math:pow(Y_axis - (?Corner_Y), 2))),
					W = ?Speed / R,
					Path = turnPath(R,W,0,270,IsClockwise,[]),
					[{TimeSlot,{X_axis - (R-X),Y_axis - Y,Angle}} || {TimeSlot,{X,Y,Angle}} <- Path];
				true ->
					R = round(math:sqrt(math:pow(X_axis-(?Corner_X+?Intersection_Edge), 2) + math:pow(Y_axis - (?Corner_Y), 2))),
					W = ?Speed / R,
					Path = turnPath(R,W,0,270,IsClockwise,[]),
					[{TimeSlot,{X_axis + (R+X),Y_axis -Y,Angle}}|| {TimeSlot,{X,Y,Angle}} <- Path]
			end
	end.

%%recursivly find curve path
turnPath(R,W,TimeSlot,Car_Start_Angle,IsClockwise,Path) ->
	if
		(TimeSlot*W >= 3.1415926/2) -> %TimeSlot*W  in Rads (stop condation)
			Path;
		true ->%turn wasn't complete
			case IsClockwise of
				true ->
					X = round(R*math:cos(-W*TimeSlot + degToRad(90 + Car_Start_Angle))),
					Y = round(R*math:sin(-W*TimeSlot + degToRad(90 + Car_Start_Angle))),
					turnPath(R,W,TimeSlot+1,Car_Start_Angle,IsClockwise,Path++[{TimeSlot,{X,Y,mod(round(-radToDeg(W*TimeSlot) + Car_Start_Angle) , 360)}}]);
				false ->
					X = round(R*math:cos(W*TimeSlot + degToRad(-90 + Car_Start_Angle))),
					Y = round(R*math:sin(W*TimeSlot + degToRad(-90 + Car_Start_Angle))),
					turnPath(R,W,TimeSlot+1,Car_Start_Angle,IsClockwise,Path++[{TimeSlot,{X,Y,mod(round(radToDeg(W*TimeSlot) + Car_Start_Angle) , 360)}}])
			end
	end.

%% Modulo
mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_) -> 0.

%% Find lane thick path recursivly 
heavyPath(List)->heavyPath(List,[]).
heavyPath([],Acc)->Acc;
heavyPath([{TimeSlot,{X,Y,Angle}}|T],Acc)->heavyPath(T,fillCoor(X,Y,Angle,TimeSlot)++Acc).

%%Find specific coordinate occuupid area 
fillCoor(X_front,Y_front,Angle,TimeSlot)->
	RadAngle=degToRad(Angle),
	X_rear=X_front-?Car_length*math:cos(RadAngle),%rear middle
	Y_rear=Y_front-?Car_length*math:sin(RadAngle),%rear middle
	Front_R={round(X_front-(?Car_width/2)*math:sin(RadAngle)),round(Y_front+(?Car_width/2)*math:cos(RadAngle))},
	Front_L={round(X_front+(?Car_width/2)*math:sin(RadAngle)),round(Y_front-(?Car_width/2)*math:cos(RadAngle))},
	Rear_R={round(X_rear-(?Car_width/2)*math:sin(RadAngle)),round(Y_rear+(?Car_width/2)*math:cos(RadAngle))},
	Rear_L={round(X_rear+(?Car_width/2)*math:sin(RadAngle)),round(Y_rear-(?Car_width/2)*math:cos(RadAngle))},
	connectDots(Rear_R,Front_R,TimeSlot)++connectDots(Front_R,Front_L,TimeSlot)++connectDots(Front_L,Rear_L,TimeSlot)++connectDots(Rear_L,Rear_R,TimeSlot).
	%turn:heavyPath(turn:turnPath(850, 725, 180, true)).

%%Find edges
connectDots({X1,Y1},{X2,Y2},TimeSlot)->
	case (X1-X2) of  
		0-> 
			M = (Y2-Y1)/(X2-(X1-1)),
			N = (Y1*X2-Y2*X1)/(X2-(X1-1));
		_->
			M = (Y2-Y1)/(X2-X1),
			N = (Y1*X2-Y2*X1)/(X2-X1)
	end,
	[{TimeSlot,{X,round(M*X+N)}}||X<-lists:seq(lists:min([X1,X2]), lists:max([X1,X2]))].%all line dots

%%Find stright path
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