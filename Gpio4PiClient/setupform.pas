unit SetupForm;

// -----------------------------------------------------------------
//
// Setup Form
//
// All CheckBox'es on the Low View sheet are created runtime
// in Forms Create Event.
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// -----------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Menus, IniFiles;


type
  // TFormSetup
  TFormSetup = class(TForm)
    ButtonClose: TButton;
    ButtonRestart: TButton;
    EditIPaddr: TEdit;
    EditRxPort: TEdit;
    EditTxPort: TEdit;
    GroupLog1: TGroupBox;
    GroupLog2: TGroupBox;
    LabelNetOver: TLabel;
    LabelLog: TLabel;
    LabelNet: TLabel;
    LabelTX: TLabel;
    LabelRX: TLabel;
    MenuItemClear: TMenuItem;
    MenuItemSet: TMenuItem;
    PageControl: TPageControl;
    PopMenuSetClear: TPopupMenu;
    TabSheetNet: TTabSheet;
    TabSheetLog: TTabSheet;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonRestartClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemClearClick(Sender: TObject);
    procedure MenuItemSetClick(Sender: TObject);
  private
    ChkBoxLogText: Array of TCheckBox;
    procedure WrtLog(S: String);
    procedure CreateCheckBox(Own: TGroupBox; Row: Integer; Cap: String);
    procedure ChkBoxSetResetWithTag(OwnTag: Integer; Checked: Boolean);
    function MenuItemGetGroupOwner(Sender: TObject): TGroupBox;
  public
    function IsLogViewChecked(Txt: String): Boolean;
  end;


var
  FormSetup: TFormSetup;

implementation

{$R *.lfm}

uses
  MainForm, Common, UdpRxTxClient, GpioPiTxtCmd;



// -----------------------------------------------
// Print messages in MainForm
// -----------------------------------------------
procedure TFormSetup.WrtLog(S: String);
begin
  FormMain.WrtLog(S);
end;



// -------------------------------------------------------------
// Is the ComboBox for Text checked'ed
// Used to select/deselect log printing
// -------------------------------------------------------------
function TFormSetup.IsLogViewChecked(Txt: String): Boolean;
var
  I: Integer;

begin
  // Find the correct checkbox and return Checked state
  for I:= Low(ChkBoxLogText) to High(ChkBoxLogText) do
  begin
    if Pos(Txt, ChkBoxLogText[I].Caption) <> 0 then
    begin
      Exit(ChkBoxLogText[I].Checked);
    end;
  end;

  Exit(False);
end;



// -------------------------------------------------------------
// Button Events
// -------------------------------------------------------------

// Save data and Restart network
procedure TFormSetup.ButtonRestartClick(Sender: TObject);
var
  CfgFil: TIniFile;

begin
  UdpRxTxClose;
  WrtLog('TX and RX Socket Closed');

  // Get new values from edit boxes
  ToAddr:=   EditIPaddr.Text;
  TxPortNr:= StrToIntDef(EditTxPort.Text, 0);
  RxPortNr:= StrToIntDef(EditRxPort.Text, 0);

  // Save setup in Ini Config File
  CfgFil:= TIniFile.Create(GetConfigFilNavn);
  CfgFil.WriteString(SectionNet, IdentToAddr, ToAddr);
  CfgFil.WriteInteger(SectionNet, IdentTxPortNr, TxPortNr);
  CfgFil.WriteInteger(SectionNet, IdentRxPortNr, RxPortNr);
  CfgFil.Destroy;

  // Restart network
  UdpRxTxInit;
end;


// Close setupform
procedure TFormSetup.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;



// -------------------------------------------------------------
// Popup Menu for Log View
// -------------------------------------------------------------

// -------------------------------------------------------------
// Check/Uncheck all checkbox'es belong to OwnTag
// -------------------------------------------------------------
procedure TFormSetup.ChkBoxSetResetWithTag(OwnTag: Integer; Checked: Boolean);
var
  I: Integer;

begin
  for I:= Low(ChkBoxLogText) to High(ChkBoxLogText) do
  begin
    if ChkBoxLogText[I].Tag = OwnTag then
      ChkBoxLogText[I].Checked:= Checked;
  end;
end;


// -------------------------------------------------------------
// Get GroupBox where MenuItem is located (GroupBox->PopupMenu->MenuItem)
// -------------------------------------------------------------
function TFormSetup.MenuItemGetGroupOwner(Sender: TObject): TGroupBox;
begin
  if Sender is TMenuItem then with Sender as TMenuItem do
  begin
    if GetParentMenu is TPopupMenu then with GetParentMenu as TPopupMenu do
    begin
       if PopupComponent is TGroupBox then
       begin
         Exit(PopupComponent as TGroupBox);
       end;
    end;
  end;

  Exit(Nil);
end;


// -------------------------------------------------------------
// Check all checkbox'es in chosen GroupBox
// -------------------------------------------------------------
procedure TFormSetup.MenuItemSetClick(Sender: TObject);
var
  GrpBox: TGroupBox;

begin
  GrpBox:= MenuItemGetGroupOwner(Sender);
  if GrpBox <> Nil then ChkBoxSetResetWithTag(GrpBox.Tag, True);
end;


// -------------------------------------------------------------
// UnCheck all checkbox'es in chosen GroupBox
// -------------------------------------------------------------
procedure TFormSetup.MenuItemClearClick(Sender: TObject);
var
  GrpBox: TGroupBox;

begin
  GrpBox:= MenuItemGetGroupOwner(Sender);
  if GrpBox <> Nil then ChkBoxSetResetWithTag(GrpBox.Tag, False);
end;



// -------------------------------------------------------------
// FORM routines
// -------------------------------------------------------------

// -------------------------------------------------------------
// Create CheckBox on Log Wiew page
// -------------------------------------------------------------
procedure TFormSetup.CreateCheckBox(Own: TGroupBox; Row: Integer; Cap: String);
var
  I: Integer;

begin
  // Set new length in dynamic array
  I:= Length(ChkBoxLogText);
  SetLength(ChkBoxLogText, I+1);

  ChkBoxLogText[I]:= TCheckBox.Create(self);
  ChkBoxLogText[I].Parent:= Own;
  ChkBoxLogText[I].Top:=  4 + ((Row-1) * 19);  // Height: 19
  ChkBoxLogText[I].Left:= 8;
  ChkBoxLogText[I].Name:= 'CB' + IntToStr(I);
  ChkBoxLogText[I].Caption:= Cap;
  ChkBoxLogText[I].Checked:= true;
  ChkBoxLogText[I].Tag:= Own.Tag;
end;


// -------------------------------------------------------------
// Setup Form Create Event
// -------------------------------------------------------------
procedure TFormSetup.FormCreate(Sender: TObject);
var
  CfgFil: TIniFile;
  I: Integer;

begin
  // Remove the ugly white colour
  TabSheetNet.Color:= clDefault;
  TabSheetLog.Color:= clDefault;

  PageControl.ActivePage:= TabSheetNet;

  // Create CheckBox for Gpio Functions
  CreateCheckBox(GroupLog1, 1, TxtGpioPiFullGpioMap);
  CreateCheckBox(GroupLog1, 2, TxtGpioSetPinMode);
  CreateCheckBox(GroupLog1, 3, TxtGpioSetPull);
  CreateCheckBox(GroupLog1, 4, TxtGpioPinLevel);

  // Create CheckBox for Clock and PWM Functions
  CreateCheckBox(GroupLog2, 1, TxtGpioPiFullClockMap);
  CreateCheckBox(GroupLog2, 2, TxtGpioSetClock);
  CreateCheckBox(GroupLog2, 3, TxtPcmSetClock);
  CreateCheckBox(GroupLog2, 4, TxtPwmSetClock);

  CreateCheckBox(GroupLog2, 6, TxtGpioPiFullPwmMap);
  CreateCheckBox(GroupLog2, 7, TxtPwmWholeGroup);

  // Load setup of CheckBoxes from Config File
  CfgFil:= TIniFile.Create(GetConfigFilNavn);
  for I:= Low(ChkBoxLogText) to High(ChkBoxLogText) do
    ChkBoxLogText[I].Checked:= CfgFil.ReadBool(SectionSetupLog,
                                               ChkBoxLogText[I].Caption,
                                               ChkBoxLogText[I].Checked);
  CfgFil.Destroy;
end;


// -------------------------------------------------------------
// Setup Form Show Event
// -------------------------------------------------------------
procedure TFormSetup.FormShow(Sender: TObject);
begin
  EditIPaddr.Text:= ToAddr;
  EditTxPort.Text:= IntToStr(TxPortNr);
  EditRxPort.Text:= IntToStr(RxPortNr);
end;


// -------------------------------------------------------------
// Setup Form Close Event
// -------------------------------------------------------------
procedure TFormSetup.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  CfgFil: TIniFile;
  I: Integer;

begin
  // Save setup of CheckBoxes in Config File
  CfgFil:= TIniFile.Create(GetConfigFilNavn);
  for I:= Low(ChkBoxLogText) to High(ChkBoxLogText) do
    CfgFil.WriteBool(SectionSetupLog,
                     ChkBoxLogText[I].Caption,
                     ChkBoxLogText[I].Checked);
  CfgFil.Destroy;
end;



end.

