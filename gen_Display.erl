%% @author snir
%% @doc @todo Add description to noets.


-module(gen_Display).

-define(refreshTime,(1000 div 30)).
-define(length,(51)).%car length


-include_lib("wx/include/wx.hrl").
  -define(max_x,(1300)).
  -define(max_y,(700)).
  -define(bgOffset,(300)).

%% ====================================================================
%% API functions
%% ====================================================================
-export([opsLoop/1,init/0,drawLoop/4,initDrawLoop/1,rpc_Call/1]).



%%%===================================================================
%%% Internal functions
%%%===================================================================
init() -> 
	CarsETS=ets:new(data, [set,public]),
	register(ops,spawn(fun() -> opsLoop(CarsETS) end)), 	
	spawn(fun() -> initDrawLoop(CarsETS) end).%
	
	initDrawLoop(CarsETS)->%!!!!!!can be in init and in same process, its only for testing
		Wx = wx:new(),
		Frame = wxFrame:new(Wx, -1, "AIM", [{size, {?max_x, ?max_y}}]),
		Panel = wxPanel:new(Frame),
		A = wxImage:new("/home/snir/workspace1/tt/src/A.png"),
		B = wxImage:new("/home/snir/workspace1/tt/src/B.png"),
		C = wxImage:new("/home/snir/workspace1/tt/src/C.png"),
		D = wxImage:new("/home/snir/workspace1/tt/src/D.png"),
		BG = wxBitmap:new(wxImage:new("/home/snir/workspace1/tt/src/bg2.png")),
		wxFrame:show(Frame),
		drawLoop(Panel,{A,B,C,D},BG,CarsETS).


	drawLoop(Panel,CarImages,BG,CarsETS)->
		receive 
			after ?refreshTime->
				drawSession(Panel,CarImages,BG,CarsETS),
				drawLoop(Panel,CarImages,BG,CarsETS)	
		end.


	opsLoop(CarsETS)->
		receive
			{display,delete,location,{PID,{_,_,_,_}}} ->
					ets:delete(CarsETS,PID),
					opsLoop(CarsETS);
			{display,update,location,{PID,{X,Y,Angle,Color}}} -> 
				if 
					(?bgOffset<Y)andalso(Y<(?max_x-?bgOffset))->
						ets:insert(CarsETS,{PID,{X,Y-?bgOffset,Angle,Color}}),
						opsLoop(CarsETS);
					true->
						opsLoop(CarsETS)
				end;		
			Else -> 
				io:format("else:~p", [Else]),opsLoop(CarsETS)
		end.
	

addSingle(DC,Color,NewPos,Angle)->
	Temp = wxImage:rotate(Color, degToRad(Angle), {0,0}),
	Bitmap = wxBitmap:new(Temp),
	wxDC:drawBitmap(DC, Bitmap, NewPos),
	wxImage:destroy(Temp),
	wxBitmap:destroy(Bitmap).

drawSession(Panel,CarImages,BG,CarsETS) ->
	ClientDC = wxClientDC:new(Panel),
	DC = wxBufferedDC:new(ClientDC),
	wxDC:drawBitmap(DC, BG, {0,0}),

	List = ets:tab2list(CarsETS),
	[addSingle(DC,findMyColor(CarImages,Color),offset({X,Y},Angle),Angle)||{_,{X,Y,Angle,Color}}<-List],
	
	wxBufferedDC:destroy(DC),
	wxClientDC:destroy(ClientDC).


degToRad(Deg)->
	(math:pi()*Deg)/180.

rpc_Call(Msg)->
	ops!Msg.

offset({X,Y},AngleDeg)->
	Angle = degToRad(AngleDeg),
	case ( AngleDeg rem 360 ) of 	
		0->
			{X-?length,Y-round(?length div 2)};	
		180->
			{X,Y-round(?length div 2)};
		270->
			{X-round(?length div 2),Y-?length};
		90->
			{X-round(?length div 2),Y};
		_ when (AngleDeg <90)->
			{round(X-(?length*(math:cos(Angle))+(?length div 2)*math:sin(Angle))),round(Y-(?length div 2)*math:cos(Angle))};
		_ when (AngleDeg <180)->
			{round(X-((?length div 2)*(math:cos(Angle-1.57)))),round(Y-((?length div 2)*math:sin(Angle-1.57)))};
		_ when (AngleDeg <270)->
			{round(X-((?length div 2)*(math:sin(Angle-3.14)))),round(Y-(?length*(math:sin(Angle-3.14))+((?length div 2)*math:cos(Angle-3.14))))};
		_ when (AngleDeg <360)->
			{round(X-(?length*(math:sin(Angle-4.71))+((?length div 2)*math:cos(Angle-4.71)))),round(Y-(?length*(math:cos(Angle-4.71))+(?length div 2)*math:sin(Angle-4.71)))}
	end.



findMyColor({A,B,C,D},Color)->
	case Color of
		0->A;
		1->B;
		2->C;
		3->D;
		_->A
	end.