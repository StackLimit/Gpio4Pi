unit OverviewForm;

// -------------------------------------------------------------------
//
// Shows a overview of Clocks and PWM's in a form
//
// Still under development and therefore not quite finished
// Copyright (c) 2024-2025 Jan Andersen
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
// Chan:  Channel number 0-3/7
// X,Y:   Position to draw the channel
// -------------------------------------------------------------
procedure TFormOverview.DrawOnePwmChannel(First: Boolean; Chan, X, Y: Integer);
var
  S: String;
  Style: TTextStyle;
  Freq,Rng,Dat: LongWord;
  Proc: Integer;
  PwmData: TPwmData;
  PwmEna: Boolean;

begin
  FillChar(PwmData{%H-}, SizeOf(PwmData), 0);

  if PiGpio.RPiModelInfo.Cpu = PI_CPU_BCM2712 then
  begin
    // RaspberryPi 5
    if not PiGpio.GetRawPwmData(Chan div 4, PwmData) then exit;

    PwmEna:= ((PwmData.Rp1Channels[Chan mod 4].Rp1Control and RP1_PWM_CHANCTRL_MODE_MASK) <> 0);
    Rng:=  PwmData.Rp1Channels[Chan mod 4].Rp1Range;
    Dat:=  PwmData.Rp1Channels[Chan mod 4].Rp1Duty;
  end
  else
  begin
    // RaspberryPi 1 to RaspberryPi 4
    if not PiGpio.GetRawPwmData(Chan div 2, PwmData) then exit;

    if Chan in [0,2]
      then PwmEna:= ((PwmData.Control and PWM0_ENABLE) <> 0)
      else PwmEna:= ((PwmData.Control and PWM1_ENABLE) <> 0);

    Rng:=  PwmData.Channels[Chan mod 2].Range;
    Dat:=  PwmData.Channels[Chan mod 2].Data;
  end;

  // Draw green / grayed rect
  if PwmEna
    then Panel.Canvas.Brush.Color:= clLightGreen
    else Panel.Canvas.Brush.Color:= clDefault;

  if not First then Panel.Canvas.Line(X - Space2, Y - PwmBoxDim.Height,
                                      X - Space2, Y + Space);

  Panel.Canvas.Line(X - Space2, Y + Space, X, Y + Space);

  Panel.Canvas.Rectangle(X, Y, X + PwmBoxDim.Width, Y + PwmBoxDim.Height);

  Panel.Canvas.Line(X + PwmBoxDim.Width, Y + Space,
                    X + PwmBoxDim.Width +  Space2, Y + Space);

  S:= 'PWM ';
  if PwmData.Cpu = PI_CPU_BCM2712 then
  begin
    // RaspberryPi 5
    case Chan of
      0: S:= S+ '0_0';
      1: S:= S+ '0_1';
      2: S:= S+ '0_2';
      3: S:= S+ '0_3';
      4: S:= S+ '1_0';
      5: S:= S+ '1_1';
      6: S:= S+ '1_2';
      7: S:= S+ '1_3';
    end;
  end
  else
  begin
    // RaspberryPi 1 to RaspberryPi 4
    case Chan of
      0: S:= S+ '0_0';
      1: S:= S+ '0_1';
      2: S:= S+ '1_0';
      3: S:= S+ '1_1';
    end;
  end;

  Freq:= 0;
  Proc:= 0;

  if Rng > 0 then
  begin
    Freq:= PiGpio.GetClockFrequency(CLK_PWM) div Rng;
    Proc:= Trunc((100 * Dat) / Rng);
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
      PM_ALT0: S:= S + ' (Alt 0)';
      PM_ALT1: S:= S + ' (Alt 1)';
      PM_ALT2: S:= S + ' (Alt 2)';
      PM_ALT3: S:= S + ' (Alt 3)';
      PM_ALT4: S:= S + ' (Alt 4)';
      PM_ALT5: S:= S + ' (Alt 5)';
      PM_ALT6: S:= S + ' (Alt 6)';
      PM_ALT7: S:= S + ' (Alt 7)';
      PM_ALT8: S:= S + ' (Alt 8)';
    end;
  end;

//  Panel.Canvas.TextOut(X+5, Y+5, S);
  Panel.Canvas.TextRect(Self.ClientRect, X+5, Y+2, S);
end;



// -------------------------------------------------------------
// Draw one Clock device
// ClkNo: GpioClock 0-5, PWM Clock, PCM Clock
// X,Y:   Position to draw the Clock
// -------------------------------------------------------------
procedure TFormOverview.DrawOneClock(ClkNo, X, Y: Integer);
var
  S: String;
  Style: TTextStyle;
  ClkEna: Boolean;
  Data: TGpioClk;

begin
  if not PiGpio.GetRawClockData(ClkNo, Data{%H-}) then exit;

  if PiGpio.RPiModelInfo.Cpu = PI_CPU_BCM2712 then
  begin
    // RaspberryPi 5
    ClkEna:= ((Data.Control and RP1_CLK_CTRL_ENABLE) <> 0);
  end
  else
  begin
    // RaspberryPi 1 to RaspberryPi 4
    ClkEna:= (((Data.Control shr 4) and 1) <> 0);
  end;

  // Draw green / grayed rect
  if ClkEna
    then Panel.Canvas.Brush.Color:= clLightGreen
    else Panel.Canvas.Brush.Color:= clDefault;

  Panel.Canvas.Rectangle(X, Y, X + ClkBoxDim.Width, Y + ClkBoxDim.Height);

  Panel.Canvas.Line(X + ClkBoxDim.Width, Y + Space,
                    X + ClkBoxDim.Width + Space2, Y + Space);

  // Build text
  case ClkNo of
    CLK_GPIO0..CLK_GPIO5: S:= 'GPIO Clock ' + IntToStr(ClkNo);
    CLK_PWM:              S:= 'PWM Clock';
    CLK_PCM:              S:= 'PCM Clock';
  end;

  // Enable bit (B4)
  S:= S + #13#10 + 'Enable: ' + LongToTrueFalse(LongWord(ClkEna));

  // Source
  S:= S + #13#10 + 'Source: ';

  if PiGpio.RPiModelInfo.Cpu = PI_CPU_BCM2712 then
  begin
    // RaspberryPi 5
    S:= S + '0x' + IntToHex((Data.Control and RP1_CLK_CTRL_SRCMASK) shr 5, 2);
  end
  else
  begin
    // RaspberryPi 1 to RaspberryPi 4
    case Data.Control and $0F of
      1:   S:= S + 'OSC';
      4:   S:= S + 'PLLA';
      5:   S:= S + 'PLLC';
      6:   S:= S + 'PLLD';
      else S:= S + 'GND';
    end;
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
const
  PwmChanBcm: Array[0..7] of Integer =
    (PWM_CHANNEL_0_0, PWM_CHANNEL_0_1,
     PWM_CHANNEL_1_0, PWM_CHANNEL_1_1,
     0,0,0,0);

  PwmChanRp1: Array[0..7] of Integer =
    (PWM_CHANNEL_0_0, PWM_CHANNEL_0_1, PWM_CHANNEL_0_2, PWM_CHANNEL_0_3,
     PWM_CHANNEL_1_0, PWM_CHANNEL_1_1, PWM_CHANNEL_1_2, PWM_CHANNEL_1_3);

var
  Cl,Gp,Pw: Integer;
  Gpio: TIntArray;
  PX,PY: Integer;
  ClkCnt,PwmCnt: Integer;
  PwmChan: Array[0..7] of Integer;

begin
  // Erase background
  Panel.Canvas.Brush.Color:= clDefault;
  Panel.Canvas.FillRect(0, 0, Panel.Width, Panel.Height);

  if PiGpio.RPiModelInfo.Cpu = PI_CPU_BCM2712 then
  begin
    // RaspberryPi 5
    ClkCnt:= 6;
    PwmCnt:= 8;
    PwmChan:= PwmChanRp1;
  end
  else
  begin
    // RaspberryPi 1 to RaspberryPi 4
    ClkCnt:= 3;
    PwmCnt:= 4;
    PwmChan:= PwmChanBcm;
  end;

  // Draw GPIO clocks 0-2/5 and all the GPIOs that are connected to the clocks
  PX:= 10;
  PY:= 10;

  for Cl:= 0 to ClkCnt-1 do
  begin
    DrawOneClock(Cl, PX, PY);

    Gpio:= PiGpio.GetGpiosForGpioClock(Cl);
    if Length(Gpio) > 0 then
    begin
      for Gp:= 0 to Length(Gpio)-1 do
      begin
        DrawOneGpio(Gp = 0, Gpio[Gp],
                    PX + ClkBoxDim.Width + Space4,
                    PY + (GpioBoxDim.Height * Gp) + (Space * Gp));
      end;
    end;

    PY:= PY + ClkBoxDim.Height + Space;
  end;


  // All PWM's are on the right side
  PX:= 140 + ClkBoxDim.Width + GpioBoxDim.Width;
  PY:= 10;

  // Draw PWM clock and 4/8 PWM channels and all the GPIOs that
  // are connected to the PWM channels
  DrawOneClock(CLK_PWM, PX, PY);

  PX:= PX + ClkBoxDim.Width + Space4;

  for Pw:= 0 to (PwmCnt-1) do
  begin
    DrawOnePwmChannel(Pw = 0, Pw, PX, PY);

    Gpio:= PiGpio.GetGpiosForPwm(PwmChan[Pw]);
    if Length(Gpio) > 0 then
    begin
      for Gp:= 0 to Length(Gpio)-1 do
      begin
        DrawOneGpio(Gp = 0, Gpio[Gp],
                    PX + PwmBoxDim.Width + Space4,
                    PY + (GpioBoxDim.Height * Gp) + (Space * Gp));
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

