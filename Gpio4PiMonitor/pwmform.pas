unit PwmForm;

// -------------------------------------------------------------------
//
// Shows all the PWM's in a Form
//
// Still under development and therefore not quite finished
// Copyright (c) 2024-2025 Jan Andersen
// -------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, StrUtils,
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
    procedure FormCreate(Sender: TObject);
  private
    GroupBox: array[0..7] of TGroupBox;
    LabelV:   array[0..7] of TLabel;
    LabelH:   array[0..7] of TLabel;
    BoxCount: Integer;
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
//  LabCtl1, LabCtl2: TLabel;
  Data: TPwmData;
  I,Idx: Integer;
  S: String;

begin
  if not PiGpio.GetRawPwmData(PwmNo, Data{%H-}) then exit;

  Idx:= PwmNo * (BoxCount div 2);

  if PiGpio.RPiModelInfo.Cpu = PI_CPU_BCM2712 then
  begin
    // RaspberryPi 5
    for I:= Low(Data.Rp1Channels) to High(Data.Rp1Channels) do
    begin
      // Global Control
      S:= '';
      if (Data.Rp1GlobCtrl and RP1_PWM_GLOBCTRL_CHAN0_EN) <> 0 then S:= S + '0,';
      if (Data.Rp1GlobCtrl and RP1_PWM_GLOBCTRL_CHAN1_EN) <> 0 then S:= S + '1,';
      if (Data.Rp1GlobCtrl and RP1_PWM_GLOBCTRL_CHAN2_EN) <> 0 then S:= S + '2,';
      if (Data.Rp1GlobCtrl and RP1_PWM_GLOBCTRL_CHAN3_EN) <> 0 then S:= S + '3';
      if S = '' then S:= 'None';
      If RPos(',', S) = Length(S) then Delete(S, Length(S), 1);      // Remove last ','
      S:= 'Ch: ' + S;

      LabelH[Idx+I].Caption:=
        '0x' + IntToHex(Data.Rp1GlobCtrl, 8) + #13#10 +
        S + #13#10 +

      // Channel Control
        '0x' + IntToHex(Data.Rp1Channels[I].Rp1Control, 8) + #13#10 +
        LongToTrueFalse(Data.Rp1Channels[I].Rp1Control and RP1_PWM_CHANCTRL_MODE_MASK) + #13#10 +

        'xx' + #13#10 +
        'xx' + #13#10 +

      // Channel Data
        IntToStr(Data.Rp1Channels[I].Rp1Phase)  + #13#10 +
        IntToStr(Data.Rp1Channels[I].Rp1Range)  + #13#10 +
        IntToStr(Data.Rp1Channels[I].Rp1Duty);

{
      PI_CPU_BCM2712: (Rp1GlobCtrl: LongWord;
                       Rp1FifoCtrl: LongWord;
                       Rp1ComRange: LongWord;
                       Rp1ComDuty:  LongWord;
                       Rp1DutyFifo: LongWord;
                       Rp1Channels: Array [0..3] of TPwmChannel);

                       PI_CPU_BCM2712: (Rp1Control: LongWord;
                                        Rp1Range:   LongWord;
                                        Rp1Phase:   LongWord;
                                        Rp1Duty:    LongWord);
}
    end;
  end

  else

  begin
    // RaspberryPi 1 to RaspberryPi 4
    // Channel 1 Control
    LabelH[Idx+0].Caption:=
      LongToTrueFalse(Data.Control and PWM0_ENABLE) + #13#10 +
      LongToTrueFalse(Data.Control and PWM0_SERIAL) + #13#10 +
      LongToTrueFalse(Data.Control and PWM0_REPEATFF) + #13#10 +
      LongToTrueFalse(Data.Control and PWM0_SILENCE) + #13#10 +
      LongToTrueFalse(Data.Control and PWM0_REVPOLAR) + #13#10 +
      LongToTrueFalse(Data.Control and PWM0_USEFIFO) + #13#10 +
      LongToTrueFalse(Data.Control and PWM0_MS_MODE) + #13#10 +

    // Channel 1 Data
      IntToStr(Data.Channels[0].Range)  + #13#10 +
      IntToStr(Data.Channels[0].Data);

    // Channel 2 Control
    LabelH[Idx+1].Caption:=
      LongToTrueFalse(Data.Control and PWM1_ENABLE) + #13#10 +
      LongToTrueFalse(Data.Control and PWM1_SERIAL) + #13#10 +
      LongToTrueFalse(Data.Control and PWM1_REPEATFF) + #13#10 +
      LongToTrueFalse(Data.Control and PWM1_SILENCE) + #13#10 +
      LongToTrueFalse(Data.Control and PWM1_REVPOLAR) + #13#10 +
      LongToTrueFalse(Data.Control and PWM1_USEFIFO) + #13#10 +
      LongToTrueFalse(Data.Control and PWM1_MS_MODE) + #13#10 +

    // Channel 2 Data
    IntToStr(Data.Channels[1].Range)  + #13#10 +
    IntToStr(Data.Channels[1].Data);
  end;
end;


const
  BoxWidth  = 180;
  BoxHeight = 210;
  BoxDist   = 8;


procedure TFormPwm.FormCreate(Sender: TObject);
var
  I,BoxNo: Integer;

procedure CreatePwmBox;
var
  X,Y: Integer;
begin
  X:= BoxDist + ((BoxNo mod 4) * (BoxWidth+BoxDist));
  Y:= BoxDist + ((BoxNo div 4) * (BoxHeight+BoxDist));

  GroupBox[BoxNo]:= TGroupBox.Create(Self);
  GroupBox[BoxNo].Parent:= Self;
  GroupBox[BoxNo].Top:= Y;
  GroupBox[BoxNo].Left:= X;
  GroupBox[BoxNo].Width:= BoxWidth;
  GroupBox[BoxNo].Height:= BoxHeight;

  LabelV[BoxNo]:= TLabel.Create(GroupBox[BoxNo]);
  LabelV[BoxNo].Parent:= GroupBox[BoxNo];
  LabelV[BoxNo].Top:= BoxDist;
  LabelV[BoxNo].Left:= BoxDist;

  LabelH[BoxNo]:= TLabel.Create(GroupBox[BoxNo]);
  LabelH[BoxNo].Parent:= GroupBox[BoxNo];
  LabelH[BoxNo].Top:= BoxDist;
  LabelH[BoxNo].Left:= BoxDist + (BoxWidth div 2);
end;


begin
  if PiGpio.RPiModelInfo.Cpu = PI_CPU_BCM2712 then
  begin
    // RaspberryPi 5
    BoxCount:= 8;
    for BoxNo:= 0 to BoxCount-1 do
    begin
      CreatePwmBox;
      GroupBox[BoxNo].Caption:= ' PWM_' + IntToStr(BoxNo div (BoxCount div 2)) +
                                '_' + IntToStr(BoxNo and 3) + ' ';

      LabelV[BoxNo].Caption:= 'Global Ctrl . . .'     + #13#10 +
                              'Global Enab . .'       + #13#10 +
                              'Chan Ctrl . . . .'     + #13#10 +
                              'Chan Enable . .'       + #13#10 +
                              'xxxxxx . . . . . .'    + #13#10 +
                              'xxxxxx . . . . .'      + #13#10 +
                              'Chan Phase . .'        + #13#10 +
                              'Chan Range . .'        + #13#10 +
                              'Chan Duty  . . .';

      LabelH[BoxNo].Caption:= 'NA' + #13#10 + 'NA' + #13#10 + 'NA' + #13#10 +
                              'NA' + #13#10 + 'NA' + #13#10 + 'NA' + #13#10 +
                              'NA' + #13#10 + 'NA' + #13#10 + 'NA';
    end;
  end

  else

  begin
    // RaspberryPi 1 to RaspberryPi 4
    BoxCount:= 4;
    for BoxNo:= 0 to BoxCount-1 do
    begin
      CreatePwmBox;
      GroupBox[BoxNo].Caption:= ' PWM_' + IntToStr(BoxNo div (BoxCount div 2)) +
                                '_' + IntToStr(BoxNo and 1) + ' ';

      LabelV[BoxNo].Caption:= 'Enable . . . . . . .' + #13#10 +
                              'Serial mode  .'       + #13#10 +
                              'Repeat . . . . . .'   + #13#10 +
                              'Silence  . . . . . .' + #13#10 +
                              'Polarity . . . . . .' + #13#10 +
                              'Use FIFO . . . . .'   + #13#10 +
                              'M/S Enable . .'       + #13#10 +
                              'Range . . . . . . .'  + #13#10 +
                              'Data  . . . . . . . .';

      LabelH[BoxNo].Caption:= 'NA' + #13#10 + 'NA' + #13#10 + 'NA' + #13#10 +
                              'NA' + #13#10 + 'NA' + #13#10 + 'NA' + #13#10 +
                              'NA' + #13#10 + 'NA' + #13#10 + 'NA';
    end;
  end;

  Self.Width:=  BoxDist + ((BoxCount div 2) * (BoxWidth+BoxDist));
  Self.Height:= BoxDist + (2 * (BoxHeight+BoxDist));

  for I:= 0 to 1 do UpdatePwmBlock(I);
end;



end.

