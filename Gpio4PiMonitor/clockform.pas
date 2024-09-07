unit ClockForm;

// -------------------------------------------------------------------
//
// Shows all the Clocks in a Form
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// -------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;


type
  // Clock Form
  TFormClocks = class(TForm)
    GroupBoxClock0: TGroupBox;
    GroupBoxClock1: TGroupBox;
    GroupBoxClock2: TGroupBox;
    GroupBoxClock3: TGroupBox;
    GroupBoxClock4: TGroupBox;
    LabClkCtl0: TLabel;
    LabClkDiv0: TLabel;
    LabClkCtl1: TLabel;
    LabClkDiv1: TLabel;
    LabClkCtl2: TLabel;
    LabClkDiv2: TLabel;
    LabClkCtl3: TLabel;
    LabClkDiv3: TLabel;
    LabClkCtl4: TLabel;
    LabClkDiv4: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure UpdateClock(ClkNo: Integer);
  end;

var
  FormClocks: TFormClocks;

implementation

{$R *.lfm}

uses
  Common, GpioDefs, Gpio4Pi, RasPiMem;


procedure TFormClocks.UpdateClock(ClkNo: Integer);
var
  LabCtl, LabDiv: TLabel;
  Txt: String;
  Di,Fr,Freq: LongWord;
  Data: TClock;

begin
  if not PiGpio.GetRawClockData(ClkNo, Data{%H-}) then exit;

  case ClkNo of
    CLK_GPIO0: begin LabCtl:= LabClkCtl0; LabDiv:= LabClkDiv0; end;
    CLK_GPIO1: begin LabCtl:= LabClkCtl1; LabDiv:= LabClkDiv1; end;
    CLK_GPIO2: begin LabCtl:= LabClkCtl2; LabDiv:= LabClkDiv2; end;
    CLK_PCM:   begin LabCtl:= LabClkCtl3; LabDiv:= LabClkDiv3; end;
    CLK_PWM:   begin LabCtl:= LabClkCtl4; LabDiv:= LabClkDiv4; end;
    else exit;
  end;

  // Source (B0-B3)
  case Data.Control and $0F of
    1:   Txt:= 'Oscillator';
    2:   Txt:= 'Testdebug0';
    3:   Txt:= 'Testdebug1';
    4:   Txt:= 'PLLA per';
    5:   Txt:= 'PLLC per';
    6:   Txt:= 'PLLD per';
    7:   Txt:= 'HDMI auxiliary';
    else Txt:= 'GND';
  end;

  // Enable bit (B4)
  Txt:= Txt + #13#10 +
        LongToTrueFalse((Data.Control shr 4) and 1);

  // Kill bit (B5)
  Txt:= Txt + #13#10 +
        LongToTrueFalse((Data.Control shr 5) and 1);

  // Flip bit (B8)
  Txt:= Txt + #13#10 +
        LongToTrueFalse((Data.Control shr 8) and 1);

  // MASH (B9-B10)
  case (Data.Control shr 9) and 3 of
    0: Txt:= Txt + #13#10 + 'Integer Division';
    1: Txt:= Txt + #13#10 + '1-stage MASH';
    2: Txt:= Txt + #13#10 + '2-stage MASH';
    3: Txt:= Txt + #13#10 + '3-stage MASH';
  end;

  // Password (B24-B31)
//  if Data.Control and $FF000000 = BCM_PASSWORD
//    then Txt:= Txt + #13#10 + 'OK (5A)'
//    else Txt:= Txt + #13#10 + 'Fail';

  LabCtl.Caption:= Txt;

  // Divisor (B12-B23)
  Di:= (Data.Divisor shr 12) and $FFF;
  Txt:= IntToStr(Di);

  // Fraction (B0-B11)
  Fr:= Data.Divisor and $FFF;
  Txt:= Txt + #13#10 + IntToStr(Fr);

  // Password (B24-B31)
//  if Divisor and $FF000000 = BCM_PASSWORD
//    then Txt:= Txt + #13#10 + 'OK (5A)'
//    else Txt:= Txt + #13#10 + 'Fail';

  // Calculate Frequency.
  Freq:= PiGpio.GetClockFrequency(ClkNo);
  Txt:= Txt + #13#10 + IntToStr(Freq);

  LabDiv.Caption:= Txt;
end;



procedure TFormClocks.FormCreate(Sender: TObject);
var
  I: Integer;

begin
  LabClkCtl0.Caption:= 'NA' + #13#10 + 'NA' + #13#10 + 'NA' + #13#10 +
                       'NA' + #13#10 + 'NA' + #13#10 + 'NA';
  LabClkCtl1.Caption:= LabClkCtl0.Caption;
  LabClkCtl2.Caption:= LabClkCtl0.Caption;
  LabClkCtl3.Caption:= LabClkCtl0.Caption;
  LabClkCtl4.Caption:= LabClkCtl0.Caption;

  LabClkDiv0.Caption:= 'NA' + #13#10 + 'NA' + #13#10 + 'NA' + #13#10 + 'NA';
  LabClkDiv1.Caption:= LabClkDiv0.Caption;
  LabClkDiv2.Caption:= LabClkDiv0.Caption;
  LabClkDiv3.Caption:= LabClkDiv0.Caption;
  LabClkDiv4.Caption:= LabClkDiv0.Caption;

  for I:= 0 to 4 do UpdateClock(I);
end;



end.

