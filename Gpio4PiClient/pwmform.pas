unit PwmForm;

// -------------------------------------------------------------------
//
// Shows all the PWM's in a Form
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// -------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  // TFormPwm
  TFormPwm = class(TForm)
    GroupBoxPwm0: TGroupBox;
    GroupBoxPwm1: TGroupBox;
    GroupBoxPwm2: TGroupBox;
    GroupBoxPwm3: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LabPwmCtl0: TLabel;
    LabPwmCtl1: TLabel;
    LabPwmCtl2: TLabel;
    LabPwmCtl3: TLabel;
    LabPwmData0: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    LabPwmData1: TLabel;
    LabPwmData2: TLabel;
    LabPwmData3: TLabel;
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure UpdatePwmBlock(PwmNo: Integer);
  end;

var
  FormPwm: TFormPwm;

implementation

{$R *.lfm}

uses
  Common, GpioPiCli, GpioDefs;


procedure TFormPwm.UpdatePwmBlock(PwmNo: Integer);
var
  LabCtl1, LabCtl2, LabDat1, LabDat2: TLabel;

begin
  case PwmNo of
    0: begin LabCtl1:= LabPwmCtl0; LabDat1:= LabPwmData0;
             LabCtl2:= LabPwmCtl1; LabDat2:= LabPwmData1; end;

    1: begin LabCtl1:= LabPwmCtl2; LabDat1:= LabPwmData2;
             LabCtl2:= LabPwmCtl3; LabDat2:= LabPwmData3; end;

    else exit;
  end;

  // Channel 1 Control
  LabCtl1.Caption:=
    LongToTrueFalse(AllPWMblocks[PwmNo].Control and PWM0_ENABLE) + #13#10 +
    LongToTrueFalse(AllPWMblocks[PwmNo].Control and PWM0_SERIAL) + #13#10 +
    LongToTrueFalse(AllPWMblocks[PwmNo].Control and PWM0_REPEATFF) + #13#10 +
    LongToTrueFalse(AllPWMblocks[PwmNo].Control and PWM0_SILENCE) + #13#10 +
    LongToTrueFalse(AllPWMblocks[PwmNo].Control and PWM0_REVPOLAR) + #13#10 +
    LongToTrueFalse(AllPWMblocks[PwmNo].Control and PWM0_USEFIFO) + #13#10 +
    LongToTrueFalse(AllPWMblocks[PwmNo].Control and PWM0_MS_MODE);

  // Channel 1 Data
  LabDat1.Caption:=
    IntToStr(AllPWMblocks[PwmNo].Channel[0].Range)  + #13#10 +
    IntToStr(AllPWMblocks[PwmNo].Channel[0].Data);

  // Channel 2 Control
  LabCtl2.Caption:=
    LongToTrueFalse(AllPWMblocks[PwmNo].Control and PWM1_ENABLE) + #13#10 +
    LongToTrueFalse(AllPWMblocks[PwmNo].Control and PWM1_SERIAL) + #13#10 +
    LongToTrueFalse(AllPWMblocks[PwmNo].Control and PWM1_REPEATFF) + #13#10 +
    LongToTrueFalse(AllPWMblocks[PwmNo].Control and PWM1_SILENCE) + #13#10 +
    LongToTrueFalse(AllPWMblocks[PwmNo].Control and PWM1_REVPOLAR) + #13#10 +
    LongToTrueFalse(AllPWMblocks[PwmNo].Control and PWM1_USEFIFO) + #13#10 +
    LongToTrueFalse(AllPWMblocks[PwmNo].Control and PWM1_MS_MODE);

  // Channel 2 Data
  LabDat2.Caption:=
    IntToStr(AllPWMblocks[PwmNo].Channel[1].Range)  + #13#10 +
    IntToStr(AllPWMblocks[PwmNo].Channel[1].Data);
end;



procedure TFormPwm.FormCreate(Sender: TObject);
var
  I: Integer;

begin
  for I:= Low(AllPWMblocks) to High(AllPWMblocks) do UpdatePwmBlock(I);
end;



end.

