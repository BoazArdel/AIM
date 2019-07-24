


-module(gen_Display).

-define(refreshTime,(1000 div 30)).
-define(length,(51)).%car length


-include_lib("wx/include/wx.hrl").
-define(max_x,(1299)).
-define(max_y,(700)).
-define(bgOffset,(300)).
-define(negOffset,(100)).

%% ====================================================================
%% API functions
%% ====================================================================
-export([opsLoop/1,init/0]).



%%%===================================================================
%%% Internal functions
%%%===================================================================
init() -> 
	CarsETS=ets:new(data, [set,public]),%all current cars instances
	register(display,spawn(fun() -> opsLoop(CarsETS) end)),%operations drawing orders
	register(drawloopREG,spawn(fun() -> initDrawLoop(CarsETS) end)).%refreshTime FPS drawing
	
	initDrawLoop(CarsETS)->
		Wx = wx:new(),
		Frame = wxFrame:new(Wx, -1, "AIM", [{size, {?max_x, ?max_y}}]),
		Panel = wxPanel:new(Frame),
		A = wxImage:new("../include/A.png"),%cars images
		B = wxImage:new("../include/B.png"),
		C = wxImage:new("../include/C.png"),
		D = wxImage:new("../include/D.png"),
		BG = wxBitmap:new( wxImage:new("../include/bg.png")),%background
		wxFrame:show(Frame),
		drawLoop(Panel,{A,B,C,D},BG,CarsETS,Wx).

%%drawLoop- draws new frame from CarsETS every refreshTime
	drawLoop(Panel,CarImages,BG,CarsETS,Wx)->
		receive 
			{pause} -> 
				self()!{pause},drawLoop(Panel,CarImages,BG,CarsETS,Wx);%waiting for termination
			{terminate}->
				wxBitmap:destroy(BG),%destroys  WX wigetes 
				{A,B,C,D}=CarImages,
				wxImage:destroy(A),
				wxImage:destroy(B),
				wxImage:destroy(C),
				wxImage:destroy(D),
				wxPanel:destroy(Panel),
				io:format("Program Terminated~n")
		after ?refreshTime->
				drawSession(Panel,CarImages,BG,CarsETS),%make a draw session
				drawLoop(Panel,CarImages,BG,CarsETS,Wx)	
		end.
	
%%opsLoop recive draw location and enter it to ETS
	opsLoop(CarsETS)->
		receive
			{display,delete,location,{PID,{_,_,_,_}}} ->
					ets:delete(CarsETS,PID),
					opsLoop(CarsETS);
			{display,update,location,{PID,{X,Y,Angle,Color}}} -> 
				if %ignore out of current screen locations
					(?bgOffset-?negOffset<Y)andalso(Y<(?max_x-?bgOffset+2*?negOffset))->
						ets:insert(CarsETS,{PID,{X-?negOffset,Y-?negOffset-?bgOffset,Angle,Color}}),
						opsLoop(CarsETS);
					true->
						opsLoop(CarsETS)
				end;
			{display,terminate}->
				imMustDie();%wait im terminate
			Else -> 
				io:format("else:~p", [Else]),opsLoop(CarsETS)
		end.
	
%%add single draw to buffer
addSingle(DC,Color,NewPos,Angle)->
	Temp = wxImage:rotate(Color, degToRad(Angle), {0,0}),
	Bitmap = wxBitmap:new(Temp),
	wxDC:drawBitmap(DC, Bitmap, NewPos),%draw bitmap to DC
	wxImage:destroy(Temp),
	wxBitmap:destroy(Bitmap).

drawSession(Panel,CarImages,BG,CarsETS) ->
	try
	ClientDC = wxClientDC:new(Panel),
	DC = wxBufferedDC:new(ClientDC),
	wxDC:drawBitmap(DC, BG, {0,0}),
	
	List = ets:tab2list(CarsETS),% all current cars
	[addSingle(DC,findMyColor(CarImages,Color),offset({X,Y},Angle),Angle)||{_,{X,Y,Angle,Color}}<-List],
	
	wxBufferedDC:destroy(DC),%prints buffer
	wxClientDC:destroy(ClientDC)
	catch%catch any wx error and terminate all
 	 _:_ -> 
 	 	drawloopREG!{pause},
		 ets:delete(CarsETS),
		display!{display,terminate},
	 	im_server!{im,terminate}	
	end.


degToRad(Deg)->
	(math:pi()*Deg)/180.

%% fix turned car offset (this wx option does not work)
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

%%wait for im termination
imMustDie()->
	receive
		{im_is_dead}->
				drawloopREG!{terminate};
		_->imMustDie()
	end.


findMyColor({A,B,C,D},Color)->
	case Color of
		0->A;
		1->B;
		2->C;
		3->D;
		_->A
	end.