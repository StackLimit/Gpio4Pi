unit MainForm;

// ------------------------------------------------------------------------
//
// A test form to test Gpio4Pi...
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// ------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Gpio4Pi, GpioDefs;

type
  TFormMain = class(TForm)
    ButtonWrite: TButton;
    ButtonRead: TButton;
    ButtonPiInfo: TButton;
    ButtonPwmClock: TButton;
    ButtonGpioClock: TButton;
    ButtonPwmRange: TButton;
    ButtonPwmValue: TButton;
    ButtonInit: TButton;
    ButtonToggel: TButton;
    ButtonPinMode: TButton;
    ComboBoxPull: TComboBox;
    ComboBoxPinMode: TComboBox;
    EditWrite: TEdit;
    EditGpioClock: TEdit;
    EditPwmValue: TEdit;
    EditGpioPin: TEdit;
    EditPwmRange: TEdit;
    EditPwmClock: TEdit;
    GroupBoxReadWrite: TGroupBox;
    GroupBoxPWM: TGroupBox;
    GroupBoxPull: TGroupBox;
    GroupBoxPinMode: TGroupBox;
    Memo1: TMemo;
    procedure ButtonGpioClockClick(Sender: TObject);
    procedure ButtonPiInfoClick(Sender: TObject);
    procedure ButtonPinModeClick(Sender: TObject);
    procedure ButtonInitClick(Sender: TObject);
    procedure ButtonPwmClockClick(Sender: TObject);
    procedure ButtonPwmRangeClick(Sender: TObject);
    procedure ButtonPwmValueClick(Sender: TObject);
    procedure ButtonReadClick(Sender: TObject);
    procedure ButtonToggelClick(Sender: TObject);
    procedure ButtonWriteClick(Sender: TObject);
    procedure ComboBoxPullChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    PiGpio: TPiGpio;
  public
    procedure Memo(S: String);
  end;


var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.Memo(S: String);
begin
  Memo1.Lines.Add(S);
end;


procedure TFormMain.ButtonInitClick(Sender: TObject);
var
  S: String;

begin
  if PiGpio <> nil then exit;

  Memo1.Clear;

  PiGpio:= TPiGpio.Create;
  if PiGpio <> nil then
  begin
    S:= 'GPIO Initialized OK, using ';
    if PiGpio.UsingGpioMem
      then S:= S + '/dev/gpiomem/'
      else S:= S + '/dev/mem/';
    Memo(S);
  end
  else Memo('Error Initialize GPIO')
end;



procedure TFormMain.ButtonPinModeClick(Sender: TObject);
var
  Pin, Mode: Integer;

begin
  if PiGpio = nil then exit;

  Pin:= StrToIntDef(EditGpioPin.Text, -1);
  If Pin = -1 then exit;

  case ComboBoxPinMode.ItemIndex of
    0:  Mode:= PM_INPUT;
    1:  Mode:= PM_OUTPUT;
    2:  Mode:= PM_ALT0;
    3:  Mode:= PM_ALT1;
    4:  Mode:= PM_ALT2;
    5:  Mode:= PM_ALT3;
    6:  Mode:= PM_ALT4;
    7:  Mode:= PM_ALT5;
    8:  Mode:= PM_PWMOUT_MS;
    9:  Mode:= PM_PWMOUT_BAL;
    10: Mode:= PM_GPIO_CLOCK;
    else Exit;
  end;

  if PiGpio.SetPinMode(Pin, Mode)
    then Memo('SetPinMode OK')
    else Memo('Error SetPinMode: ' + PiGpio.LastErrorStr);
end;


procedure TFormMain.ComboBoxPullChange(Sender: TObject);
var
  Pin: Integer;
  Ok: Boolean;

begin
  if PiGpio = nil then exit;

  Pin:= StrToIntDef(EditGpioPin.Text, -1);
  If Pin = -1 then exit;

  case ComboBoxPull.ItemIndex of
    0: Ok:= PiGpio.SetPullMode(Pin, PUD_OFF);
    1: Ok:= PiGpio.SetPullMode(Pin, PUD_DOWN);
    2: Ok:= PiGpio.SetPullMode(Pin, PUD_UP);
    else Ok:= False;
  end;

  if Ok
    then Memo('SetPullMode OK')
    else Memo('Error SetPullMode: ' + PiGpio.LastErrorStr);
end;


procedure TFormMain.ButtonWriteClick(Sender: TObject);
var
  Pin: Integer;
  Val: Integer;
  Ok: Boolean;

begin
  if PiGpio = nil then exit;

  Pin:= StrToIntDef(EditGpioPin.Text, -1);
  If Pin = -1 then exit;

  Val:= StrToIntDef(EditWrite.Text, -1);
  If Val = -1 then exit;

  Ok:= PiGpio.GpioWrite(Pin, Val);

  if Ok
    then Memo('GpioWrite OK')
    else Memo('Error GpioWrite: ' + PiGpio.LastErrorStr);
end;



procedure TFormMain.ButtonReadClick(Sender: TObject);
var
  Pin: Integer;
  Val: Integer;

begin
  if PiGpio = nil then exit;

  Pin:= StrToIntDef(EditGpioPin.Text, -1);
  If Pin = -1 then exit;

  Val:= PiGpio.GpioRead(Pin);
  Memo('Gpio ' + IntToStr(Pin) + ' value: ' + IntToStr(Val));
end;



procedure TFormMain.ButtonToggelClick(Sender: TObject);
var
  Pin: Integer;
  Val: Integer;
  Ok: Boolean;

begin
  if PiGpio = nil then exit;

  Pin:= StrToIntDef(EditGpioPin.Text, -1);
  If Pin = -1 then exit;

  Val:= PiGpio.GpioRead(Pin);
  Ok:= True;
  if Val <> $FF then
    Ok:= PiGpio.GpioWrite(Pin, (not Val) and $01);

  if (Val <> $FF) and Ok
    then Memo('GpioRead + GpioWrite OK')
    else Memo('Error GpioRead + GpioWrite: ' + PiGpio.LastErrorStr);
end;


procedure TFormMain.ButtonPwmValueClick(Sender: TObject);
var
  Pin: Integer;
  Val: Integer;

begin
  if PiGpio = nil then exit;

  Pin:= StrToIntDef(EditGpioPin.Text, -1);
  If Pin = -1 then exit;

  Val:= StrToIntDef(EditPwmValue.Text, -1);
  If Val = -1 then exit;

  if PiGpio.SetPwmValue(Pin, Val)
    then Memo('SetPwmValue OK')
    else Memo('Error SetPwmValue: ' + PiGpio.LastErrorStr);
end;


procedure TFormMain.ButtonPwmRangeClick(Sender: TObject);
var
  Pin: Integer;
  Val: Integer;

begin
  if PiGpio = nil then exit;

  Pin:= StrToIntDef(EditGpioPin.Text, -1);
  If Pin = -1 then exit;

  Val:= StrToIntDef(EditPwmRange.Text, -1);
  If Val = -1 then exit;

  if PiGpio.SetPwmRange(Pin, Val)
    then Memo('SetPwmRange Ok')
    else Memo('Error SetPwmRange: ' + PiGpio.LastErrorStr);
end;


procedure TFormMain.ButtonPwmClockClick(Sender: TObject);
var
  Pin, Val: Integer;

begin
  if PiGpio = nil then exit;

  Pin:= StrToIntDef(EditGpioPin.Text, -1);
  If Pin = -1 then exit;

  Val:= StrToIntDef(EditPwmClock.Text, -1);
  If Val = -1 then exit;

  if PiGpio.SetPwmMasterClock(Val)
    then Memo('SetPwmMasterClock OK')
    else Memo('Error SetPwmMasterClock: ' + PiGpio.LastErrorStr);
end;


procedure TFormMain.ButtonGpioClockClick(Sender: TObject);
var
  Pin, Val: Integer;

begin
  if PiGpio = nil then exit;

  Pin:= StrToIntDef(EditGpioPin.Text, -1);
  If Pin = -1 then exit;

  Val:= StrToIntDef(EditGpioClock.Text, -1);
  If Val = -1 then exit;

  if PiGpio.SetGpioClock(Pin, Val)
    then Memo('SetGpioClock OK')
    else Memo('Error SetGpioClock: ' + PiGpio.LastErrorStr);
end;




procedure TFormMain.ButtonPiInfoClick(Sender: TObject);
var
  PiInfo: TRPiModelInfo;

begin
  if PiGpio = nil then exit;
  PiInfo:= PiGpio.RPiModelInfo;

  Memo('---------------------------------');
  Memo('Raspberry Pi Info:');
  Memo('------------------');
  Memo(' Revision: ' + PiRevisionNames[PiInfo.Rev]);
  Memo(' Model...: ' + PiModelNames[PiInfo.Model]);
  Memo(' CPU.....: ' + PiProcessor[PiInfo.Cpu]);
  Memo(' Maker...: ' + PiMakerNames[PiInfo.Maker]);
  Memo(' Memory..: ' + IntToStr(PiMemorySize[PiInfo.Mem]));
  Memo('---------------------------------');
end;


procedure TFormMain.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
  Memo('');
  Memo('---> First, press "Initialize GPIO"');
end;



end.

