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
  Classes, Types, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls;


type
  // TFormOverview
  TFormOverview = class(TForm)
    Panel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure PanelPaint(Sender: TObject);
  private
    ClkBoxDim: TSize;
    PwmBoxDim: TSize;
    GpioBoxDim: TSize;
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

const
  Space = 10;           // Distance between boxes
  Space2 = Space * 2;
  Space4 = Space * 4;



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

  if not First then Panel.Canvas.Line(X - Space2, Y - PwmBoxDim.Height,
                                      X - Space2, Y + Space);

  Panel.Canvas.Line(X - Space2, Y + Space, X, Y + Space);

  Panel.Canvas.Rectangle(X, Y, X + PwmBoxDim.Width, Y + PwmBoxDim.Height);

  Panel.Canvas.Line(X + PwmBoxDim.Width, Y + Space,
                    X + PwmBoxDim.Width +  Space2, Y + Space);

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
  Panel.Canvas.TextRect(Self.ClientRect, X+5, Y+2, S);
end;


// -------------------------------------------------------------
// Draw one GPIO
// First: The GPIO are the first on a device
// GPIO:  GPIO number 0-63
// X,Y:   Position to draw the GPIO box
// -------------------------------------------------------------
procedure TFormOverview.DrawOneGpio(First: Boolean; Gpio, X, Y: Integer);
var
  S: String;
  Data: TGpioPin;

begin
  Panel.Canvas.Brush.Color:= clLightGreen;

  if not First then Panel.Canvas.Line(X - Space2, Y - GpioBoxDim.Height,
                                      X - Space2, Y + Space);

  Panel.Canvas.Line(X - Space2, Y + Space, X, Y + Space);

  Panel.Canvas.Rectangle(X, Y, X + GpioBoxDim.Width, Y + GpioBoxDim.Height);

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

//  Panel.Canvas.TextOut(X+5, Y+5, S);
  Panel.Canvas.TextRect(Self.ClientRect, X+5, Y+2, S);
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

  Panel.Canvas.Rectangle(X, Y, X + ClkBoxDim.Width, Y + ClkBoxDim.Height);

  Panel.Canvas.Line(X + ClkBoxDim.Width, Y + Space,
                    X + ClkBoxDim.Width + Space2, Y + Space);

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
  Gpio: TIntArray;
  FirstGp: Boolean;
  PX,PY: Integer;

begin
  // Erase background
  Panel.Canvas.Brush.Color:= clDefault;
  Panel.Canvas.FillRect(0, 0, Panel.Width, Panel.Height);

  // Draw GPIO clocks 0-2 and all the GPIOs that are connected to the clocks
  PX:= 10;
  PY:= 10;

  for Cl:= 0 to 2 do
  begin
    DrawOneClock(Cl, PX, PY);

    Gpio:= PiGpio.GetGpiosForGpioClock(Cl);
    if Length(Gpio) > 0 then
    begin
      FirstGp:= True;
      for Gp:= 0 to Length(Gpio)-1 do
      begin
        DrawOneGpio(FirstGp, Gpio[Gp],
                    PX + ClkBoxDim.Width + Space4,
                    PY + (GpioBoxDim.Height * Gp) + (Space * Gp));
        FirstGp:= False;
      end;
    end;

    PY:= PY + ClkBoxDim.Height + Space;
  end;


  // Draw PWM clock and 4 PWM channels and all the GPIOs that
  // are connected to the PWM channels
  DrawOneClock(4, PX, PY);

  PX:= PX + ClkBoxDim.Width + Space4;

  for Pw:= 0 to 3 do
  begin
    DrawOnePwmChannel(Pw = 0, Pw, PX, PY);

    Gpio:= PiGpio.GetGpiosForPwm(Pw);
    if Length(Gpio) > 0 then
    begin
      FirstGp:= True;
      for Gp:= 0 to Length(Gpio)-1 do
      begin
        DrawOneGpio(FirstGp, Gpio[Gp],
                    PX + PwmBoxDim.Width + Space4,
                    PY + (GpioBoxDim.Height * Gp) + (Space * Gp));
        FirstGp:= False;
      end;
    end;

    PY:= PY + PwmBoxDim.Height + Space;
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
  // Set size of boxes
  ClkBoxDim:= Panel.Canvas.TextExtent('Freq: 20000000');
  ClkBoxDim.Height:= (ClkBoxDim.Height+3) * 4;
  ClkBoxDim.Width:= ClkBoxDim.Width + 10;

  PwmBoxDim:= Panel.Canvas.TextExtent('20000000 Hz, 100%');
  PwmBoxDim.Height:= (PwmBoxDim.Height+3) * 3;
  PwmBoxDim.Width:= PwmBoxDim.Width + 10;

  GpioBoxDim:= Panel.Canvas.TextExtent('GPIO 22 (ALT 0)');
  GpioBoxDim.Height:= GpioBoxDim.Height + 5;
  GpioBoxDim.Width:= GpioBoxDim.Width + 10;
end;



end.

