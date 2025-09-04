unit ClockForm;

// -------------------------------------------------------------------
//
// Shows all the Clocks in a Form
//
// Still under development and therefore not quite finished
// Copyright (c) 2024-2025 Jan Andersen
// -------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;


type
  // Clock Form
  TFormClocks = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    GroupBox: array[0..8] of TGroupBox;
    LabelV:   array[0..8] of TLabel;
    LabelH:   array[0..8] of TLabel;
    BoxCount: Integer;
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
  Txt: String;
  Freq: LongWord;
  Data: TGpioClk;
  ClkIdx: Integer;

begin
  if not PiGpio.GetRawClockData(ClkNo, Data{%H-}) then exit;

  if PiGpio.RPiModelInfo.Cpu = PI_CPU_BCM2712 then
  begin
    // RaspberryPi 5
    case ClkNo of
      CLK_GPIO0: ClkIdx:= 0;
      CLK_GPIO1: ClkIdx:= 1;
      CLK_GPIO2: ClkIdx:= 2;
      CLK_GPIO3: ClkIdx:= 3;
      CLK_GPIO4: ClkIdx:= 4;
      CLK_GPIO5: ClkIdx:= 5;
      CLK_PWM:   ClkIdx:= 6;
      CLK_UART:  ClkIdx:= 7;
      CLK_PCM:   ClkIdx:= 8;
      else exit;
    end;

    // Source (B5-B8)
    Txt:= '0x' + IntToHex((Data.Control and RP1_CLK_CTRL_SRCMASK) shr 5, 2);

    // Enable bit (B11)
    Txt:= Txt + #13#10 +
          LongToTrueFalse((Data.Control shr 11) and 1);

    // Control
    Txt:= Txt + #13#10 +
          '0x' + IntToHex(Data.Control, 8);
  end

  else

  begin
    // RaspberryPi 1 to RaspberryPi 4
    case ClkNo of
      CLK_GPIO0: ClkIdx:= 0;
      CLK_GPIO1: ClkIdx:= 1;
      CLK_GPIO2: ClkIdx:= 2;
      CLK_PWM:   ClkIdx:= 3;
      CLK_UART:  ClkIdx:= 4;
      CLK_PCM:   ClkIdx:= 5;
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
      0: Txt:= Txt + #13#10 + 'Int. Division';
      1: Txt:= Txt + #13#10 + '1-stage MASH';
      2: Txt:= Txt + #13#10 + '2-stage MASH';
      3: Txt:= Txt + #13#10 + '3-stage MASH';
    end;
  end;

  // Divisor and Fraction
  Txt:= Txt + #13#10 + IntToStr(Data.Divisor);
  Txt:= Txt + #13#10 + IntToStr(Data.Fract);

  // Calculate Frequency.
  Freq:= PiGpio.GetClockFrequency(ClkNo);
  Txt:= Txt + #13#10 + IntToStr(Freq);

  LabelH[ClkIdx].Caption:= Txt;
end;


const
  BoxWidth  = 190;
  BoxHeight = 180;
  BoxDist   = 8;


procedure TFormClocks.FormCreate(Sender: TObject);
var
  I,BoxNo: Integer;

procedure CreateClockBox;
var
  X,Y: Integer;
begin
  X:= BoxDist + ((BoxNo mod 3) * (BoxWidth+BoxDist));
  Y:= BoxDist + ((BoxNo div 3) * (BoxHeight+BoxDist));

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
    BoxCount:= 9;
    for BoxNo:= 0 to BoxCount-1 do
    begin
      CreateClockBox;
      Case BoxNo of
        0: GroupBox[BoxNo].Caption:= ' GPIO Clock 0 ';
        1: GroupBox[BoxNo].Caption:= ' GPIO Clock 1 ';
        2: GroupBox[BoxNo].Caption:= ' GPIO Clock 2 ';
        3: GroupBox[BoxNo].Caption:= ' GPIO Clock 3 ';
        4: GroupBox[BoxNo].Caption:= ' GPIO Clock 4 ';
        5: GroupBox[BoxNo].Caption:= ' GPIO Clock 5 ';
        6: GroupBox[BoxNo].Caption:= ' PWM Clock ';
        7: GroupBox[BoxNo].Caption:= ' UART Clock ';
        8: GroupBox[BoxNo].Caption:= ' PCM Clock ';
      end;

      LabelV[BoxNo].Caption:= 'Clock Source .'       + #13#10 +
                              'Enable . . . . . .'   + #13#10 +
                              'Control . . . . . .'  + #13#10 +
                              'Divisor . . . . . .'  + #13#10 +
                              'Fraction . . . . .'   + #13#10 +
                              'Frequency Hz .';


      LabelH[BoxNo].Caption:= 'NA' + #13#10 + 'NA' + #13#10 + 'NA' + #13#10 +
                              'NA' + #13#10 + 'NA' + #13#10 + 'NA' + #13#10;
    end;
  end

  else

  begin
    // RaspberryPi 1 to RaspberryPi 4
    BoxCount:= 6;
    for BoxNo:= 0 to BoxCount-1 do
    begin
      CreateClockBox;
      Case BoxNo of
        0: GroupBox[BoxNo].Caption:= ' GPIO Clock 0 ';
        1: GroupBox[BoxNo].Caption:= ' GPIO Clock 1 ';
        2: GroupBox[BoxNo].Caption:= ' GPIO Clock 2 ';
        3: GroupBox[BoxNo].Caption:= ' PWM Clock ';
        4: GroupBox[BoxNo].Caption:= ' UART Clock ';
        5: GroupBox[BoxNo].Caption:= ' PCM Clock ';
      end;

      LabelV[BoxNo].Caption:= 'Clock Source .'           + #13#10 +
                              'Enable . . . . . . .'     + #13#10 +
                              'Kill . . . . . . . . . .' + #13#10 +
                              'Flip . . . . . . . . .'   + #13#10 +
                              'MASH . . . . . . .'       + #13#10 +
                              'Divisor . . . . . . .'    + #13#10 +
                              'Fraction . . . . . .'     + #13#10 +
                              'Frequency Hz .';

      LabelH[BoxNo].Caption:= 'NA' + #13#10 + 'NA' + #13#10 + 'NA' + #13#10 +
                              'NA' + #13#10 + 'NA' + #13#10 + 'NA' + #13#10 +
                              'NA' + #13#10 + 'NA';
    end;
  end;

  Self.Width:=  BoxDist + (3 * (BoxWidth+BoxDist));
  Self.Height:= BoxDist + ((BoxCount div 3) * (BoxHeight+BoxDist));

  for I:= 0 to 8 do UpdateClock(I);
end;



end.

