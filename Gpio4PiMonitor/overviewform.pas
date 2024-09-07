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
  // TFormOverview
  TFormOverview = class(TForm)
    Panel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure PanelPaint(Sender: TObject);
  private
    procedure DrawOnePwmChannel(First: Boolean; Chan, X, Y: Integer);
    procedure DrawOneGpio(First: Boolean; Gpio, X, Y: Integer);
    procedure DrawOneClock(ClkNo, X, Y: Integer);
  public
    procedure UpdateView;
  end;

var
  FormOverview: TFormOverview;

implementation

{$R *.lfm}

uses
  Common, GpioDefs, Gpio4Pi, RasPiMem, GPIOcheckbox;




// -------------------------------------------------------------
// Draw one PWM channel
// First: The channel are the very first
// Chan:  Channel number 0-3
// X,Y:   Position to draw the channel
// -------------------------------------------------------------
procedure TFormOverview.DrawOnePwmChannel(First: Boolean; Chan, X, Y: Integer);
var
  S: String;
  Style: TTextStyle;
  Mask: LongWord;
  Freq,Rng,Dat: LongWord;
  Proc: Integer;
  PwmData: TPwmData;

begin
  FillChar(PwmData{%H-}, SizeOf(PwmData), 0);
  if not PiGpio.GetRawPwmData(Chan div 2, PwmData) then exit;

  if Chan in [0,2] then
  begin
    Mask:= PWM0_ENABLE;
    Rng:=  PwmData.Channels[0].Range;
    Dat:=  PwmData.Channels[0].Data;
  end
  else
  begin
    Mask:= PWM1_ENABLE;
    Rng:=  PwmData.Channels[1].Range;
    Dat:=  PwmData.Channels[1].Data;
  end;

  // Draw green / grayed rect
  if (PwmData.Control and Mask) <> 0
    then Panel.Canvas.Brush.Color:= clLightGreen
    else Panel.Canvas.Brush.Color:= clDefault;

  if not First then Panel.Canvas.Line(X, Y-50, X, Y);
  Panel.Canvas.Line(X, Y, X+20, Y);
  Panel.Canvas.Rectangle(X+20, Y-9, X+130, Y+39);
  Panel.Canvas.Line(X+130, Y, X+150, Y);

  S:= 'PWM ';
  case Chan of
    0: S:= S+ '0_0';
    1: S:= S+ '0_1';
    2: S:= S+ '1_0';
    3: S:= S+ '1_1';
  end;

  Freq:= 0;
  Proc:= 0;

  if Rng > 0 then
  begin
    Freq:= PiGpio.GetClockFrequency(4) div Rng;
    Proc:= (100 * Dat) div Rng;
  end;

  S:= S + #13#10 + 'Rng/Dat: ' + IntToStr(Rng) + '/' + IntToStr(Dat);
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
  Data: TGpioPin;

begin
  Panel.Canvas.Brush.Color:= clLightGreen;

  if not First then Panel.Canvas.Line(X, Y-20, X, Y);
  Panel.Canvas.Line(X, Y, X+20, Y);
  Panel.Canvas.Rectangle(X+20, Y-9, X+110, Y+9);

  S:= 'GPIO ' + IntToStr(Gpio);

  if PiGpio.GetGpioPinData(Gpio, Data{%H-}) then
  begin
    case Data.Mode of
      FSEL_ALT0: S:= S + ' (Alt 0)';
      FSEL_ALT1: S:= S + ' (Alt 1)';
      FSEL_ALT2: S:= S + ' (Alt 2)';
      FSEL_ALT3: S:= S + ' (Alt 3)';
      FSEL_ALT4: S:= S + ' (Alt 4)';
      FSEL_ALT5: S:= S + ' (Alt 5)';
    end;
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
  Data: TClock;

begin
  if not PiGpio.GetRawClockData(ClkNo, Data{%H-}) then exit;

  // Draw green / grayed rect
  if ((Data.Control shr 4) and 1) <> 0
    then Panel.Canvas.Brush.Color:= clLightGreen
    else Panel.Canvas.Brush.Color:= clDefault;

  Panel.Canvas.Rectangle(X, Y, X+100, Y+80);
  Panel.Canvas.Line(X+100, Y+10, X+120, Y+10);

  // Build text
  case ClkNo of
    0..2: S:= 'GPIO Clock ' + IntToStr(ClkNo);
    3:    S:= 'PCM Clock';
    4:    S:= 'PWM Clock';
  end;

  // Enable bit (B4)
  S:= S + #13#10 + 'Enable: ' +
    LongToTrueFalse((Data.Control shr 4) and 1);

  // Source (B0-B3)
  S:= S + #13#10 + 'Source: ';
  case Data.Control and $0F of
    1:   S:= S + 'OSC';
    4:   S:= S + 'PLLA';
    5:   S:= S + 'PLLC';
    6:   S:= S + 'PLLD';
    else S:= S + 'GND';
  end;

  // Calculate Frequency.
  S:= S + #13#10 + 'Freq: ' + IntToStr(PiGpio.GetClockFrequency(ClkNo));

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

    Gpio:= PiGpio.GetGpiosForGpioClock(Cl);
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
  DrawOneClock(4, 10, 280);

  for Pw:= 0 to 3 do
  begin
    DrawOnePwmChannel(Pw = 0, Pw, 130, 290+(Pw*50));

    Gpio:= PiGpio.GetGpiosForPwm(Pw);
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

