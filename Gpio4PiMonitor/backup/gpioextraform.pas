unit GpioExtraForm;

// -------------------------------------------------------------------
//
// Shows GPIO 28 to 57 which is not shown in MainForm
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// -------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  GPIOcheckbox;


type
  TFormGpioExtra = class(TForm)
    PanelForComboBoxe: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    PinCheckBoxe: Array [28..57] of TGpioCheckBox;  // Index = GPIO pin
    procedure InitAtStartup;
  public
    procedure UpdatePinCombo(GpioPin: Integer);
  end;

var
  FormGpioExtra: TFormGpioExtra;

implementation

{$R *.lfm}

uses
  GpioDefs, Gpio4Pi, RasPiMem, GpioExtraStuff;



// -------------------------------------------------------------
// A Pin Combo must be updated
// GpioPin: Gpio pin number (28-57). -1 = All combos
// -------------------------------------------------------------
procedure TFormGpioExtra.UpdatePinCombo(GpioPin: Integer);
var
  I: Integer;

procedure UpdateOneCombo(GP: Integer);
var
  ChBox: TGpioCheckBox;
  Data: TGpioPin;
  P: Integer;
  Mo,Pu: String[10];
  Cap,Hnt: String;

begin
  if not PiGpio.GetGpioPinData(GP, Data{%H-}) then Exit;
  if not (GP in [Low(PinCheckBoxe)..High(PinCheckBoxe)]) then Exit;

  ChBox:= PinCheckBoxe[GP];
  Cap:= 'GPIO ' + IntToStr(GP);
  Hnt:= 'GPIO ' + IntToStr(GP);

  if Odd(GP)
    then Cap:= '[U] ' + Cap
    else Cap:= Cap + ' [U]';

  Mo:= GpioModeToShortStr(Data.Mode);
  Pu:= GpioPullToShortStr(Data.Pull);
  Hnt:= Hnt + ', ' + GpioModeToLongStr(GP, Data.Mode);
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
    for I:= Low(PinCheckBoxe) to High(PinCheckBoxe) do UpdateOneCombo(I);
  end
  else
  begin
    if (GpioPin >= Low(PinCheckBoxe)) and (GpioPin <= High(PinCheckBoxe)) then
      UpdateOneCombo(GpioPin);
  end;
end;



// -------------------------------------------------------------
// Initialize
// -------------------------------------------------------------
procedure TFormGpioExtra.InitAtStartup;
var
  I,X,Y: Integer;

begin
  // Create all combos in Panel: PanelForComboBoxe
  For I:= Low(PinCheckBoxe) to High(PinCheckBoxe) do
  begin
    if Odd(I) then X:= 170 else X:= 8;
    Y:= 8 + (((I-Low(PinCheckBoxe)) Div 2) * 22);

    PinCheckBoxe[I]:= TGpioCheckBox.Create(self);
    PinCheckBoxe[I].Parent:= PanelForComboBoxe;
    PinCheckBoxe[I].Top:= Y;
    PinCheckBoxe[I].Left:= X;
    PinCheckBoxe[I].Name:= 'Gp' + IntToStr(I);
    PinCheckBoxe[I].Tag:= I;                     // Tag is used in the OnChange event
    PinCheckBoxe[I].ShowHint:= True;
    PinCheckBoxe[I].Caption:= 'GPIO ' + IntToStr(I);
    PinCheckBoxe[I].Hint:= 'GPIO ' + IntToStr(I);;

    if not Odd(I) then
    begin
      PinCheckBoxe[I].BiDiMode:= bdRightToLeft;    // Text BEFORE the box
    end;
  end;

  // Update all Combo's
  UpdatePinCombo(-1);
end;



// -------------------------------------------------------------
// FORM Events
// -------------------------------------------------------------

// Form Create event
procedure TFormGpioExtra.FormCreate(Sender: TObject);
begin
  PanelForComboBoxe.Caption:= '';
  InitAtStartup;
end;


end.

