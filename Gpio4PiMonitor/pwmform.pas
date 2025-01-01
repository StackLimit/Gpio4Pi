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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  GpioDefs;


const
  // We are using an array of LongWords, so we have to divide by 4
  // Overview Form also uses these const
  PWM_CONTROLd4 = PWM_CONTROL div 4;
  PWM_STATUSd4  = PWM_STATUS  div 4;
  PWM_DMACTLd4  = PWM_DMACTL  div 4;
  PWM0_RANGEd4  = PWM0_RANGE  div 4;
  PWM0_DATAd4   = PWM0_DATA   div 4;
  PWM_FIFOd4    = PWM_FIFO    div 4;
  PWM1_RANGEd4  = PWM1_RANGE  div 4;
  PWM1_DATAd4   = PWM1_DATA   div 4;



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
  Common, Gpio4Pi, RasPiMem;




procedure TFormPwm.UpdatePwmBlock(PwmNo: Integer);
var
  LabCtl1, LabCtl2, LabDat1, LabDat2: TLabel;
  Data: TPwmData;

begin
  if not PiGpio.GetRawPwmData(PwmNo, Data{%H-}) then exit;

  case PwmNo of
    0: begin LabCtl1:= LabPwmCtl0; LabDat1:= LabPwmData0;
             LabCtl2:= LabPwmCtl1; LabDat2:= LabPwmData1; end;

    1: begin LabCtl1:= LabPwmCtl2; LabDat1:= LabPwmData2;
             LabCtl2:= LabPwmCtl3; LabDat2:= LabPwmData3; end;

    else exit;
  end;

  // Channel 1 Control
  LabCtl1.Caption:=
    LongToTrueFalse(Data.Control and PWM0_ENABLE) + #13#10 +
    LongToTrueFalse(Data.Control and PWM0_SERIAL) + #13#10 +
    LongToTrueFalse(Data.Control and PWM0_REPEATFF) + #13#10 +
    LongToTrueFalse(Data.Control and PWM0_SILENCE) + #13#10 +
    LongToTrueFalse(Data.Control and PWM0_REVPOLAR) + #13#10 +
    LongToTrueFalse(Data.Control and PWM0_USEFIFO) + #13#10 +
    LongToTrueFalse(Data.Control and PWM0_MS_MODE);

  // Channel 1 Data
  LabDat1.Caption:=
    IntToStr(Data.Channels[0].Range)  + #13#10 +
    IntToStr(Data.Channels[0].Data);

  // Channel 2 Control
  LabCtl2.Caption:=
    LongToTrueFalse(Data.Control and PWM1_ENABLE) + #13#10 +
    LongToTrueFalse(Data.Control and PWM1_SERIAL) + #13#10 +
    LongToTrueFalse(Data.Control and PWM1_REPEATFF) + #13#10 +
    LongToTrueFalse(Data.Control and PWM1_SILENCE) + #13#10 +
    LongToTrueFalse(Data.Control and PWM1_REVPOLAR) + #13#10 +
    LongToTrueFalse(Data.Control and PWM1_USEFIFO) + #13#10 +
    LongToTrueFalse(Data.Control and PWM1_MS_MODE);

  // Channel 2 Data
  LabDat2.Caption:=
  IntToStr(Data.Channels[1].Range)  + #13#10 +
  IntToStr(Data.Channels[1].Data);
end;



procedure TFormPwm.FormCreate(Sender: TObject);
var
  I: Integer;

begin
  LabPwmCtl0.Caption:= 'NA' + #13#10 + 'NA' + #13#10 + 'NA' + #13#10 +
                       'NA' + #13#10 + 'NA' + #13#10 + 'NA' + #13#10 + 'NA';
  LabPwmCtl1.Caption:= LabPwmCtl0.Caption;
  LabPwmCtl2.Caption:= LabPwmCtl0.Caption;
  LabPwmCtl3.Caption:= LabPwmCtl0.Caption;

  LabPwmData0.Caption:= 'NA' + #13#10 + 'NA';
  LabPwmData1.Caption:= LabPwmData0.Caption;
  LabPwmData2.Caption:= LabPwmData0.Caption;
  LabPwmData3.Caption:= LabPwmData0.Caption;

  for I:= 0 to 1 do UpdatePwmBlock(I);
end;



end.

