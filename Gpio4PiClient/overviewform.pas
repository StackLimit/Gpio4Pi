unit OverviewForm;

// -------------------------------------------------------------------
//
// Shows a overview of Clocks and PWM's in a form
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// -------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls;

type
  IntArray = Array of Integer;

  // TFormOverview
  TFormOverview = class(TForm)
    Panel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure PanelPaint(Sender: TObject);
  private
    procedure DrawOnePwmChannel(First: Boolean; Chan, X, Y: Integer);
    procedure DrawOneGpio(First: Boolean; Gpio, X, Y: Integer);
    procedure DrawOneClock(ClkNo, X, Y: Integer);
    function FindGpiosForPwm(PwmNo: Integer): IntArray;
    function FindGpiosForGpioClock(ClkNo: Integer): IntArray;
  public
    procedure UpdateView;
  end;

var
  FormOverview: TFormOverview;

implementation

{$R *.lfm}

uses
  Common, GpioDefs, GpioPiCli, GPIOcheckbox;



// -------------------------------------------------------------
// Find all the GPIOs that are assigned to a GpioClock
// -------------------------------------------------------------
function TFormOverview.FindGpiosForGpioClock(ClkNo: Integer): IntArray;
var
  Gpio: Integer;
  Ok: Boolean;

begin
  Result:= [];

  for Gpio:= Low(AllPins) to High(AllPins) do
  begin
    Ok:= False;

    case ClkNo of
      0: Ok:= ((Gpio = 4)  and (AllPins[Gpio].Mode = FSEL_ALT0)) or
              ((Gpio = 20) and (AllPins[Gpio].Mode = FSEL_ALT5)) or
              ((Gpio = 32) and (AllPins[Gpio].Mode = FSEL_ALT0)) or
              ((Gpio = 34) and (AllPins[Gpio].Mode = FSEL_ALT0));

      1: Ok:= ((Gpio = 5)  and (AllPins[Gpio].Mode = FSEL_ALT0)) or
              ((Gpio = 21) and (AllPins[Gpio].Mode = FSEL_ALT5)) or
              ((Gpio = 42) and (AllPins[Gpio].Mode = FSEL_ALT0)) or
              ((Gpio = 44) and (AllPins[Gpio].Mode = FSEL_ALT0));

      2: Ok:= ((Gpio = 6)  and (AllPins[Gpio].Mode = FSEL_ALT0)) or
              ((Gpio = 43) and (AllPins[Gpio].Mode = FSEL_ALT0));
    end;

    If Ok then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result)-1]:= Gpio;
    end;
  end;
end;



// -------------------------------------------------------------
// Find all the GPIOs that are assigned to a PWM channel
// PwmNo: 0 = Channel 0_0, 1 = Channel 0_1
//        2 = Channel 1_0, 3 = Channel 1_1
// -------------------------------------------------------------
function TFormOverview.FindGpiosForPwm(PwmNo: Integer): IntArray;
var
  Gpio: Integer;
  Ok: Boolean;

begin
  Result:= [];

  for Gpio:= Low(AllPins) to High(AllPins) do
  begin
    Ok:= False;

    case PwmNo of
      0: Ok:= ((Gpio = 12) and (AllPins[Gpio].Mode = FSEL_ALT0)) or
              ((Gpio = 18) and (AllPins[Gpio].Mode = FSEL_ALT5));

      1: Ok:= ((Gpio = 13) and (AllPins[Gpio].Mode = FSEL_ALT0)) or
              ((Gpio = 19) and (AllPins[Gpio].Mode = FSEL_ALT5)) or
              ((Gpio = 45) and (AllPins[Gpio].Mode = FSEL_ALT0));

      2: Ok:= ((Gpio = 40) and (AllPins[Gpio].Mode = FSEL_ALT0));

      3: Ok:= ((Gpio = 41) and (AllPins[Gpio].Mode = FSEL_ALT0));
    end;

    If Ok then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result)-1]:= Gpio;
    end;
  end;
end;


// -------------------------------------------------------------
// Draw one PWM channel
// First: The channel are the very first
// Chan:  Channel number PWM_CHANNEL_0_0 to PWM_CHANNEL_1_1
// X,Y:   Position to draw the channel
// -------------------------------------------------------------
procedure TFormOverview.DrawOnePwmChannel(First: Boolean; Chan, X, Y: Integer);
var
  S: String;
  Style: TTextStyle;
  Mask: LongWord;
  Freq: LongWord;
  Proc: Integer;

begin
  // Draw green / grayed rect
  if Chan in [0,2]
    then Mask:= PWM0_ENABLE
    else Mask:= PWM1_ENABLE;

  if (AllPWMblocks[Chan shr 1].Control and Mask) <> 0
    then Panel.Canvas.Brush.Color:= clLightGreen
    else Panel.Canvas.Brush.Color:= clDefault;

  if not First then Panel.Canvas.Line(X, Y-50, X, Y);
  Panel.Canvas.Line(X, Y, X+20, Y);
  Panel.Canvas.Rectangle(X+20, Y-9, X+130, Y+39);
  Panel.Canvas.Line(X+130, Y, X+150, Y);

  S:= 'PWM ';
  case Chan of
    PWM_CHANNEL_0_0: S:= S+ '0_0';
    PWM_CHANNEL_0_1: S:= S+ '0_1';
    PWM_CHANNEL_1_0: S:= S+ '1_0';
    PWM_CHANNEL_1_1: S:= S+ '1_1';
  end;

  Freq:= 0;
  Proc:= 0;

  if AllPWMblocks[Chan shr 1].Channel[Chan and 1].Range > 0 then
  begin
    Freq:= FreqFromDiv(4) div AllPWMblocks[Chan shr 1].Channel[Chan and 1].Range;
    Proc:= (100 * AllPWMblocks[Chan shr 1].Channel[Chan and 1].Data) div
           AllPWMblocks[Chan shr 1].Channel[Chan and 1].Range;
  end;

  S:= S + #13#10 + 'Rng/Dat: ' + IntToStr(AllPWMblocks[Chan shr 1].Channel[Chan and 1].Range) +
                   '/' + IntToStr(AllPWMblocks[Chan shr 1].Channel[Chan and 1].Data);
  S:= S + #13#10 + IntToStr(Freq) + ' Hz, ' + IntToStr(Proc) + '%';

  // Draw multiline text
  Style:= Panel.Canvas.TextStyle;
  Style.SingleLine:= False;
  Panel.Canvas.TextStyle:= Style;
  Panel.Canvas.TextRect(Self.ClientRect, X+25, Y-7, S);
end;


// -------------------------------------------------------------
// Draw one GPIO
// First: The GPIO are the first on a device
// GPIO:  GPIO number 0-63
// X,Y:   Position to draw the GPIO
// -------------------------------------------------------------
procedure TFormOverview.DrawOneGpio(First: Boolean; Gpio, X, Y: Integer);
var
  S: String;

begin
  Panel.Canvas.Brush.Color:= clLightGreen;

  if not First then Panel.Canvas.Line(X, Y-20, X, Y);
  Panel.Canvas.Line(X, Y, X+20, Y);
  Panel.Canvas.Rectangle(X+20, Y-9, X+110, Y+9);

  S:= 'GPIO ' + IntToStr(Gpio);

  case AllPins[Gpio].Mode of
    FSEL_ALT0: S:= S + ' (Alt 0)';
    FSEL_ALT1: S:= S + ' (Alt 1)';
    FSEL_ALT2: S:= S + ' (Alt 2)';
    FSEL_ALT3: S:= S + ' (Alt 3)';
    FSEL_ALT4: S:= S + ' (Alt 4)';
    FSEL_ALT5: S:= S + ' (Alt 5)';
  end;

//  Panel.Canvas.TextOut(X+25, Y-8, S);
  Panel.Canvas.TextRect(Self.ClientRect, X+25, Y-8, S);
end;



// -------------------------------------------------------------
// Draw one Clock device
// ClkNo: 0-2 = GpioClock 0-2, 3 = PCM Clock, 4 = PWM Clock
// X,Y:   Position to draw the Clock
// -------------------------------------------------------------
procedure TFormOverview.DrawOneClock(ClkNo, X, Y: Integer);
var
  S: String;
  Style: TTextStyle;

begin
  // Draw green / grayed rect
  if ((AllClocks[ClkNo].Control shr 4) and 1) <> 0
    then Panel.Canvas.Brush.Color:= clLightGreen
    else Panel.Canvas.Brush.Color:= clDefault;

  Panel.Canvas.Rectangle(X, Y, X+100, Y+80);
  Panel.Canvas.Line(X+100, Y+10, X+120, Y+10);

  // Build text
  case ClkNo of
    CLK_GPIO0..CLK_GPIO2: S:= 'GPIO Clock ' + IntToStr(ClkNo);
    CLK_PCM:              S:= 'PCM Clock';
    CLK_PWM:              S:= 'PWM Clock';
  end;

  // Enable bit (B4)
  S:= S + #13#10 + 'Enable: ' +
    LongToTrueFalse((AllClocks[ClkNo].Control shr 4) and 1);

  // Source (B0-B3)
  S:= S + #13#10 + 'Source: ';
  case AllClocks[ClkNo].Control and $0F of
    1:   S:= S + 'OSC';
    4:   S:= S + 'PLLA';
    5:   S:= S + 'PLLC';
    6:   S:= S + 'PLLD';
    else S:= S + 'GND';
  end;

  // Calculate Frequency.
  S:= S + #13#10 + 'Freq: ' + IntToStr(FreqFromDiv(ClkNo));

  // Draw multiline text
  Style:= Panel.Canvas.TextStyle;
  Style.SingleLine:= False;
  Panel.Canvas.TextStyle:= Style;
  Panel.Canvas.TextRect(Self.ClientRect, X+5, Y+5, S);
end;



// -------------------------------------------------------------
// Panel OnPaint received
// We draw all the stuff ourself in a TPanel
// -------------------------------------------------------------
procedure TFormOverview.PanelPaint(Sender: TObject);
var
  Cl,Gp,Pw: Integer;
  Gpio: IntArray;
  FirstGp: Boolean;

begin
  // Erase background
  Panel.Canvas.Brush.Color:= clDefault;
  Panel.Canvas.FillRect(0, 0, Panel.Width, Panel.Height);

  // Draw GPIO clocks 0-2 and all the GPIOs that are connected to the clocks
  for Cl:= 0 to 2 do
  begin
    DrawOneClock(Cl, 10, 10+(Cl*90));

    Gpio:= FindGpiosForGpioClock(Cl);
    if Length(Gpio) > 0 then
    begin
      FirstGp:= True;
      for Gp:= 0 to Length(Gpio)-1 do
      begin
        DrawOneGpio(FirstGp, Gpio[Gp], 130, 20+(Cl*90)+(Gp*20));
        FirstGp:= False;
      end;
    end;
  end;


  // Draw PWM clock and 4 PWM channels and all the GPIOs that
  // are connected to the PWM channels
  DrawOneClock(CLK_PWM, 10, 280);

  for Pw:= 0 to 3 do
  begin
    DrawOnePwmChannel(Pw = 0, Pw, 130, 290+(Pw*50));

    Gpio:= FindGpiosForPwm(Pw);
    if Length(Gpio) > 0 then
    begin
      FirstGp:= True;
      for Gp:= 0 to Length(Gpio)-1 do
      begin
        DrawOneGpio(FirstGp, Gpio[Gp], 280, 290+(Pw*50)+(Gp*20));
        FirstGp:= False;
      end;
    end;
  end;
end;



// -------------------------------------------------------------
// Update View
// Called when data is received
// -------------------------------------------------------------
procedure TFormOverview.UpdateView;
begin
  Repaint;
end;



procedure TFormOverview.FormCreate(Sender: TObject);
begin
  //
end;



end.

