unit MainForm;

// -----------------------------------------------------------------
//
// GpioPi Client is a test / debug App which connects via UDP
// to other app that uses Gpio4Pi on an platform other than
// Raspberry Pi.
//
// Used for development when developing on a platform other than
// Raspberry Pi, e.g. Windows or Raspian on Virtualbox.
//
// Written in Lazarus 2.2.4 on Windows and are compatible with
// Lazarus on "Raspbian on Virtualbox"
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// -----------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Menus, IniFiles, GPIOcheckbox;


const
  NumberPins = 32;   // Gpio-0 to Gpio-31


type
  // TFormMain
  TFormMain = class(TForm)
    Label1: TLabel;
    MainMenu1: TMainMenu;
    MemoText: TMemo;
    MenuItemWiew: TMenuItem;
    MenuItemPwm: TMenuItem;
    MenuItemClocks: TMenuItem;
    Separator1: TMenuItem;
    Separator4: TMenuItem;
    MenuItemSetup: TMenuItem;
    MenuItemLogClear: TMenuItem;
    PanelForComboBoxe: TPanel;
    StatusBar: TStatusBar;
    MenuItemLog: TMenuItem;
    MenuItemReqIoMap: TMenuItem;
    MenuItemPi: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemFile: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure MenuItemClocksClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemLogClearClick(Sender: TObject);
    procedure MenuItemPwmClick(Sender: TObject);
    procedure MenuItemReqIoMapClick(Sender: TObject);
    procedure MenuItemSetupClick(Sender: TObject);
    procedure MenuItemWiewClick(Sender: TObject);
  private
    PinCheckBoxe: Array [1..40] of TGpioCheckBox;
    procedure CheckBoxChange(Sender: TObject);
    procedure DataReceived(FraAdr, Data: String);
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
  Common, GpioPiCli, GpioDefs, UdpRxTxClient, GpioPiTxtCmd,
  SetupForm, ClockForm, PwmForm, OverviewForm;


procedure TFormMain.WrtLog(S: String);
begin
  MemoText.lines.add(S);
end;

procedure TFormMain.UpdateStatBar(No: integer; S: String);
begin
  StatusBar.Panels[No].Text:= S;
end;



// -------------------------------------------------------------
// UDP Data received from other app
// -------------------------------------------------------------
procedure TFormMain.DataReceived(FraAdr, Data: String);
begin
  DataFromGpioPi(FraAdr, Data);
end;


// -------------------------------------------------------------
// A Pin Combo must be updated
// GpioPin: Gpio pin number (0-31). -1 = All combos
// -------------------------------------------------------------
procedure TFormMain.UpdatePinCombo(GpioPin: Integer);
var
  I: Integer;

procedure UpdateOneCombo(GP: Integer);
var
  ChBox: TGpioCheckBox;
  Mode,Pud,State: Byte;
  Phys,P: Integer;
  Mo,Pu: String[10];
  Cap,Hnt: String;

begin
  if not GetGpioPiData(GP, Mode{%H-}, Pud{%H-}, State{%H-}) then Exit;
  Phys:= ConvertPinFromTo(RPI_PIN_GPIO, RPI_PIN_PHYS, GP);
  if not (Phys in [Low(PinCheckBoxe)..High(PinCheckBoxe)]) then Exit;

  ChBox:= PinCheckBoxe[Phys];
  Cap:= TextForPinCombo(Phys);
  Hnt:= HintForPinCombo(Phys);

  case Mode of
    FSEL_INPUT:  begin Mo:= 'In';    Hnt:= Hnt + ', Input'; end;
    FSEL_OUTPUT: begin Mo:= 'Out';   Hnt:= Hnt + ', Output'; end;
    FSEL_ALT0:   begin Mo:= 'Alt0';  Hnt:= Hnt + ', Alt 0'; end;
    FSEL_ALT1:   begin Mo:= 'Alt1';  Hnt:= Hnt + ', Alt 1'; end;
    FSEL_ALT2:   begin Mo:= 'Alt2';  Hnt:= Hnt + ', Alt 2'; end;
    FSEL_ALT3:   begin Mo:= 'Alt3';  Hnt:= Hnt + ', Alt 3'; end;
    FSEL_ALT4:   begin Mo:= 'Alt4';  Hnt:= Hnt + ', Alt 4'; end;
    FSEL_ALT5:   begin Mo:= 'Alt5';  Hnt:= Hnt + ', Alt 5'; end;
    else         begin Mo:= 'Undef'; Hnt:= Hnt + ', Undefined'; end;
  end;

  case Pud of
    PUD_UP:   begin Pu:= ',PulU';   Hnt:= Hnt + ', PullUp'; end;
    PUD_DOWN: begin Pu:= ',PulD';   Hnt:= Hnt + ', PullDown'; end;
    PUD_OFF:  begin Pu:= ',NoPul';  Hnt:= Hnt + ', No PullUp/Down'; end;
    else      begin Pu:= ',Pull?';  Hnt:= Hnt + ', PullUp/Down Undefined'; end;
  end;

  case State of
    0: ChBox.Checked:= False;
    1: ChBox.Checked:= True;
    else Hnt:= Hnt + ', State=' + IntToStr(State);
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
  ChBox.GpioMode:=  Mode;
  ChBox.GpioPud:=   Pud;
  ChBox.GpioState:= State;
end;

// UpdatePinCombo entry. Update One or All ?
begin
  if (GpioPin < 0) or (GpioPin >= NumberPins) then
  begin
    for I:= 0 to NumberPins-1 do UpdateOneCombo(I);
  end
  else UpdateOneCombo(GpioPin);
end;



// -------------------------------------------------------------
// A CheckBox has been pressed
// -------------------------------------------------------------
procedure TFormMain.CheckBoxChange(Sender: TObject);
var
  PhysPin,UsePin: Integer;
  TxData: String;

begin
  // ComboBox number (Physical Pin) is in Tag
  PhysPin:= TGpioCheckBox(Sender).Tag;

  // Convert pin number to correct Gpio Pin
  UsePin:= ConvertPinFromTo(RPI_PIN_PHYS, RPI_PIN_GPIO, PhysPin);
  if UsePin = -1 then Exit;

  // Xmit data: "GpioPiSetGpioPin,p,s"   where p=pin and s=state
  TxData:= TxtGpioSetGpioPin + ',' +
           IntToStr(UsePin) + ',';

  if TGpioCheckBox(Sender).Checked
    then TxData:= TxData + '1'
    else TxData:= TxData + '0';

  UdpRxTxSend(TxData);
end;



// -------------------------------------------------------------
// Menu Events
// -------------------------------------------------------------

// File -> Setup
procedure TFormMain.MenuItemSetupClick(Sender: TObject);
begin
  FormSetup.ShowModal;
end;


// File -> Exit
procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;


// GPIO -> Request Full IO map
procedure TFormMain.MenuItemReqIoMapClick(Sender: TObject);
begin
  UdpRxTxSend(TxtGpioPiReqFullMap);
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
  I,X,Y: Integer;
  CfgFil: TIniFile;

begin
  // Load setup from Ini Config File
  CfgFil:= TIniFile.Create(GetConfigFilNavn);
  CfgFil.Destroy;

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

  // Set RX callback and start network
  RxDataEvent:= @DataReceived;
  UdpRxTxInit;
end;



end.

