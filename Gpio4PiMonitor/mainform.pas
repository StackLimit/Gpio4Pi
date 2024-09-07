unit MainForm;

// -----------------------------------------------------------------
//
// Gpio4Pi Monitor is a test / debug App which scan the memory
// of Raspberry Pi and show the status og GPIO's, Clocks and PWM's
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// -----------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Menus, GPIOcheckbox;


type
  // TFormMain
  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    MemoText: TMemo;
    MenuItemGpioExtra: TMenuItem;
    MenuItemWiew: TMenuItem;
    MenuItemPwm: TMenuItem;
    MenuItemClocks: TMenuItem;
    MenuItemLogClear: TMenuItem;
    PanelForComboBoxe: TPanel;
    StatusBar: TStatusBar;
    MenuItemLog: TMenuItem;
    MenuItemPi: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemFile: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure MenuItemClocksClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemGpioExtraClick(Sender: TObject);
    procedure MenuItemLogClearClick(Sender: TObject);
    procedure MenuItemPwmClick(Sender: TObject);
    procedure MenuItemWiewClick(Sender: TObject);
  private
    PinCheckBoxe: Array [1..40] of TGpioCheckBox;
    procedure CheckBoxChange(Sender: TObject);
    procedure InitAtStartup;
    procedure UpdateStatBar(No: integer; S: String);
  public
    procedure WrtLog(S: String);
    procedure UpdatePinCombo(GpioPin: Integer);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  GpioDefs, Gpio4Pi, RasPiMem, GpioExtraStuff,
  GpioExtraForm, ClockForm, PwmForm, OverviewForm;


procedure TFormMain.WrtLog(S: String);
begin
  MemoText.lines.add(S);
end;

procedure TFormMain.UpdateStatBar(No: integer; S: String);
begin
  StatusBar.Panels[No].Text:= S;
end;



// -------------------------------------------------------------
// A Pin Combo must be updated
// GpioPin: Gpio pin number 0-27 or 28-57. -1 = All combos
// -------------------------------------------------------------
procedure TFormMain.UpdatePinCombo(GpioPin: Integer);
var
  I: Integer;

procedure UpdateOneCombo(GP: Integer);
var
  ChBox: TGpioCheckBox;
  Data: TGpioPin;
  Phys,P: Integer;
  Mo,Pu: String[10];
  Cap,Hnt: String;

begin
  if not PiGpio.GetGpioPinData(GP, Data{%H-}) then Exit;
  Phys:= ConvertPinFromTo(RPI_PIN_GPIO, RPI_PIN_PHYS, GP);
  if not (Phys in [Low(PinCheckBoxe)..High(PinCheckBoxe)]) then Exit;

  ChBox:= PinCheckBoxe[Phys];
  Cap:= TextForPinCombo(Phys);
  Hnt:= HintForPinCombo(Phys);

  Mo:= GpioModeToShortStr(Data.Mode);
  Pu:= GpioPullToShortStr(Data.Pull);
  Hnt:= Hnt + ', ' + GpioModeToLongStr(PiGpio.RPiModelInfo.Cpu, GP, Data.Mode);
  Hnt:= Hnt + ', ' + GpioPullToLongStr(Data.Pull);

  case Data.Level of
    0: ChBox.Checked:= False;
    1: ChBox.Checked:= True;
    else Hnt:= Hnt + ', State=' + IntToStr(Data.Level);
  end;

  // Set Mode (In,Out,etc.) in Caption
  P:= Pos('[U]', Cap);
  if P <> 0 then
  begin
    Delete(Cap, P+1, 1);     // Delete 'U'
    Insert(Mo+Pu, Cap, P+1);
  end;
  ChBox.Caption:= Cap;

  // Set Hint
  ChBox.Hint:= Hnt;

  // Additional GPIO data
  ChBox.GpioMode:=  Data.Mode;
  ChBox.GpioPull:=  Data.Pull;
  ChBox.GpioLevel:= Data.Level;
end;

// UpdatePinCombo entry. Update One or All ?
begin
  if GpioPin = -1 then
  begin
    for I:= 0 to NumberHeaderPins-1 do UpdateOneCombo(I);
    if FormGpioExtra <> nil then FormGpioExtra.UpdatePinCombo(-1);
  end
  else
  begin
    if (GpioPin >= 0) and
       (GpioPin < NumberHeaderPins) then UpdateOneCombo(GpioPin)
    else
    if (GpioPin >= NumberHeaderPins) then FormGpioExtra.UpdatePinCombo(GpioPin);
  end;
end;



// -------------------------------------------------------------
// A CheckBox has been pressed (NOT USED)
// -------------------------------------------------------------
procedure TFormMain.CheckBoxChange(Sender: TObject);
var
  PhysPin,UsePin: Integer;

begin
  // ComboBox number (Physical Pin) is in Tag
  PhysPin:= TGpioCheckBox(Sender).Tag;

  // Convert pin number to correct Gpio Pin
  UsePin:= ConvertPinFromTo(RPI_PIN_PHYS, RPI_PIN_GPIO, PhysPin);
  if UsePin = -1 then Exit;

end;



// -------------------------------------------------------------
// Menu Events
// -------------------------------------------------------------

// File -> Exit
procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;


// GPIO -> Show Gpio 28 to 57
procedure TFormMain.MenuItemGpioExtraClick(Sender: TObject);
begin
  FormGpioExtra.Show;
end;


// GPIO -> Show Clocks
procedure TFormMain.MenuItemClocksClick(Sender: TObject);
begin
  FormClocks.Show;
end;


// GPIO -> Show PWM
procedure TFormMain.MenuItemPwmClick(Sender: TObject);
begin
  FormPwm.Show;
end;


// GPIO -> Overview
procedure TFormMain.MenuItemWiewClick(Sender: TObject);
begin
  FormOverview.Show;
end;


// Log View -> Clear all logs
procedure TFormMain.MenuItemLogClearClick(Sender: TObject);
begin
  MemoText.Clear;
end;




// -------------------------------------------------------------
// Initialize
// -------------------------------------------------------------
procedure TFormMain.InitAtStartup;
var
  PiInfo: TRPiModelInfo;
  I,X,Y: Integer;

begin
  // Initialize RasPiMem
  PiGpio:= TPiGpioMem.Create;

  if PiGpio <> Nil then
  begin
    WrtLog('GPIO Initialized OK');
    if PiGpio.UsingGpioMem
      then WrtLog('Using memory: /dev/gpiomem')
      else WrtLog('Using memory: /dev/mem');

    PiInfo:= PiGpio.RPiModelInfo;
    WrtLog('---------------------------------');
    WrtLog('Raspberry Pi Info:');
    WrtLog('------------------');
    WrtLog(' Revision: ' + PiRevisionNames[PiInfo.Rev]);
    WrtLog(' Model...: ' + PiModelNames[PiInfo.Model]);
    WrtLog(' CPU.....: ' + PiProcessor[PiInfo.Cpu]);
    WrtLog(' Maker...: ' + PiMakerNames[PiInfo.Maker]);
    WrtLog(' Memory..: ' + IntToStr(PiMemorySize[PiInfo.Mem]));
    WrtLog('---------------------------------');
  end
  else WrtLog('GPIO Initialized FAIL');

  // Create all combos in Panel: PanelForComboBoxe
  For I:= Low(PinCheckBoxe) to High(PinCheckBoxe) do
  begin
    if Odd(I) then X:= 8 else X:= 170;
    Y:= 8 + (((I-1) Div 2) * 22);

    PinCheckBoxe[I]:= TGpioCheckBox.Create(self);
    PinCheckBoxe[I].Parent:= PanelForComboBoxe;
    PinCheckBoxe[I].Top:= Y;
    PinCheckBoxe[I].Left:= X;
    PinCheckBoxe[I].Name:= 'Pin' + IntToStr(I);
    PinCheckBoxe[I].Tag:= I;                     // Tag is used in the OnChange event
    PinCheckBoxe[I].OnChange:= @CheckBoxChange;
    PinCheckBoxe[I].ShowHint:= True;
    PinCheckBoxe[I].Caption:= TextForPinCombo(I);
    PinCheckBoxe[I].Hint:= HintForPinCombo(I);

    if Odd(I) then
    begin
      PinCheckBoxe[I].BiDiMode:= bdRightToLeft;    // Text BEFORE the box
    end;

    // Disable Combos that are not GPIO pin
    if Pos('[X]', PinCheckBoxe[I].Caption) <> 0 then
    begin
      PinCheckBoxe[I].Enabled:= False;
    end;
  end;

  // Update all Combo's
  UpdatePinCombo(-1);
end;



// -------------------------------------------------------------
// FORM Events
// -------------------------------------------------------------

// Form Create event
procedure TFormMain.FormCreate(Sender: TObject);
begin
  MemoText.Clear;
  PanelForComboBoxe.Caption:= '';

  InitAtStartup;
end;



end.

