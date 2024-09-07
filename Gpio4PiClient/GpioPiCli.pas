unit GpioPiCli;

// -----------------------------------------------------------------
//
// Holds the data for all GpioPi pins
// Receives UDP data from an app using Gpio4Pi
// updates workspace based on received data
//
// All pins that calls into this unit are GPIO pins
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// -----------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils;



// -----------------------------------------------
// Workspace for GPIO pins
// -----------------------
// All pins in this workspace are GPIO pins
// A Gpio pin corresponds to the index in the AllPins[] array,
// eg. AllPins[3] is GPIO 3
// -----------------------------------------------

Type
  // Definition of ONE GPIO pin.
  TOnePiPin = record
    Mode:  Byte;   // Input, Output, etc.
    Pud:   Byte;   // PullUp, PullDown, etc.
    State: Byte;   // On, Off, etc.
  end;
  TAllPins = Array[0..63] of TOnePiPin;


  // Definition of ONE Clock.
  // Clock 0-2 are GPIO clocks, 3 are PCM clock and 4 are PWM clock
  TOneClock = record
    Control: LongWord;   // Control word from Pi Clock memory
    Divisor: LongWord;   // Divisor from Pi Clock memory
  end;
  TAllClocks = Array[0..4] of TOneClock;


  // Definition of ONE PWM block.
  TOnePWMchannel = record
    Range: LongWord;   // Range word from Pi PWM memory
    Data:  LongWord;   // Data from Pi PWM memory
  end;

  TOnePWMblock = record
    Control: LongWord;
    Status:  LongWord;
    DMA:     LongWord;
    FIFO:    LongWord;
    Channel: Array[0..1] of TOnePWMchannel;
  end;
  TAllPWMblocks = Array[0..1] of TOnePWMblock;


Var
  // Workspace
  AllPins:      TAllPins;
  AllClocks:    TAllClocks;
  AllPWMblocks: TAllPWMblocks;


function FreqFromDiv(ClkNo: Integer): LongWord;
function GetGpioPiData(GpioPin: Integer; Var Mode,Pud,State: Byte): Boolean;
procedure DataFromGpioPi(FromAdr, Data: String);


implementation

Uses
  GpioDefs, GpioPiTxtCmd,
  MainForm, SetupForm, ClockForm, PwmForm, OverviewForm;


Var
  // Received data. Command and up to 4 args
  Cmd:  String;
  Args: Array[1..4] of String;



// -----------------------------------------------
// Print messages in MainForm
// -----------------------------------------------
procedure WrtLog(S: String);
begin
  FormMain.WrtLog(S);
end;



// -----------------------------------------------
// Calculate Frequency og a clock
// From the manual: Freq:= Source / (DIVI + DIVF / 1024)
// This is dependent on MESH mode !!!
// -----------------------------------------------
function FreqFromDiv(ClkNo: Integer): LongWord;
var
  Di,Fr,PiFreq: LongWord;

begin
  if AllClocks[ClkNo].Control and $0F = 6
    then PiFreq:= CLK_PLLD_FREQ_2711
    else PiFreq:= CLK_OSC_FREQ_2711;

  // Divisor (B12-B23)
  Di:= (AllClocks[ClkNo].Divisor shr 12) and $FFF;

  // Fraction (B0-B11)
  Fr:= AllClocks[ClkNo].Divisor and $FFF;

  if Di > 0
    then Result:= Trunc(PiFreq / (Di + (Fr / 1024)))
    else Result:= 0;
end;


// -------------------------------------------------------------
// Return Data for Gpio Pin from workspace
// GpioPin: GPIO pin number (0-63).
// Used by Main
// -------------------------------------------------------------
function GetGpioPiData(GpioPin: Integer; Var Mode,Pud,State: Byte): Boolean;
begin
  if not (GpioPin in [Low(AllPins)..High(AllPins)]) then Exit(False);

  Mode:=  AllPins[GpioPin].Mode;
  Pud:=   AllPins[GpioPin].Pud;
  State:= AllPins[GpioPin].State;

  Exit(True);
end;



// -------------------------------------------------------------
// Full GPIO Map is received
// Args[1]: Mode for all pins           Hex Bytes
// Args[2]: PullUp/Down for all pins    Hex Bytes
// Args[3]: State for all pins          Hex Bytes
// -------------------------------------------------------------
procedure RxGpioPiFullGpioMap;
var
  Mo,Pu,St: String;
  I,Pos2,Pos8: Integer;

begin
  Pos2:= 1;
  Pos8:= 1;

  for I:= Low(AllPins) to High(AllPins) do
  begin
    Mo:= '$' + Copy(Args[1], Pos2, 2);    // Byte
    Pu:= '$' + Copy(Args[2], Pos2, 2);    // Byte
    St:= '$' + Copy(Args[3], Pos2, 2);    // Byte

    AllPins[I].Mode:=  StrToIntDef(Mo, $FF);
    AllPins[I].Pud:=   StrToIntDef(Pu, 0);
    AllPins[I].State:= StrToIntDef(St, 0);

    Pos2:= Pos2 + 2;
    Pos8:= Pos8 + 8;
  end;

  // Update all combo's
  FormMain.UpdatePinCombo(-1);
end;


// -------------------------------------------------------------
// A GPIO Pin Mode has been received
// Args[1]: Gpio Pin
// Args[2]: Mode (Input, Output, etc.)
// -------------------------------------------------------------
procedure RxGpioSetPinMode;
var
  GpioPin,Mode: Integer;

begin
  GpioPin:= StrToIntDef(Args[1], -1);
  Mode:=    StrToIntDef(Args[2], -1);

  if not (GpioPin in [Low(AllPins)..High(AllPins)]) then Exit;
  if Mode = -1 then Exit;

  // Update the Mode in the Workspace and then the Combo
  AllPins[GpioPin].Mode:= Mode;
  FormMain.UpdatePinCombo(GpioPin);
end;



// -------------------------------------------------------------
// A Pull Up/Down has been received
// Args[1]: Gpio Pin
// Args[2]: PullUp/Down Control (PUD_OFF, PUD_DOWN, PUD_UP)
// -------------------------------------------------------------
procedure RxGpioSetPull;
var
  GpioPin,Pud: Integer;

begin
  GpioPin:= StrToIntDef(Args[1], -1);
  Pud:=     StrToIntDef(Args[2], -1);

  if not (GpioPin in [Low(AllPins)..High(AllPins)]) then Exit;
  if Pud = -1 then Exit;

  // Update the Pud in the Workspace and then the Combo
  AllPins[GpioPin].Pud:= Pud;
  FormMain.UpdatePinCombo(GpioPin);
end;



// -------------------------------------------------------------
// A GPIO Pin Level has been received
// Args[1]: Gpio Pin
// Args[2]: State (On, Off, etc.)
// -------------------------------------------------------------
procedure RxGpioPinLevel;
var
  GpioPin,State: Integer;

begin
  GpioPin:= StrToIntDef(Args[1], -1);
  State:=   StrToIntDef(Args[2], -1);

  if not (GpioPin in [Low(AllPins)..High(AllPins)]) then Exit;
  if State = -1 then Exit;

  // Update the State in the Workspace and then the Combo
  AllPins[GpioPin].State:= State;
  FormMain.UpdatePinCombo(GpioPin);
end;



// -------------------------------------------------------------
// Full Clock Map is received
// Args[1]: Control words for clock 0-4      Hex LongWords
// Args[2]: Divisors for clock 0-4           Hex LongWords
// -------------------------------------------------------------
procedure RxGpioPiFullClockMap;
var
  Data: TOneClock;
  I: Integer;

begin
  for I:= Low(AllClocks) to High(AllClocks) do
  begin
    Data.Control:= StrToIntDef('$' + Copy(Args[1], (I*8)+1,  8), -1);
    Data.Divisor:= StrToIntDef('$' + Copy(Args[2], (I*8)+1,  8), -1);

    if (Data.Control <> LongWord(-1)) and
       (Data.Divisor <> LongWord(-1)) then
    begin
      // Store Data and update the clock in ClockForm
      AllClocks[I]:= Data;
      FormClocks.UpdateClock(I);
    end;
  end;
end;



// -------------------------------------------------------------
// A GPIO Set Clock has been received
// Args[1]: Clock number 0-2, 3, 4
// Args[2]: Clock Control  (LongWord)
// Args[3]: Clock Divisor  (LongWord)
// -------------------------------------------------------------
procedure RxGpioSetClock;
var
  ClkNr: Integer;
  Data: TOneClock;

begin
  ClkNr:= StrToIntDef(Args[1], -1);
  Data.Control:= StrToIntDef(Args[2], -1);
  Data.Divisor:= StrToIntDef(Args[3], -1);

  if Data.Control = LongWord(-1) then Exit;
  if Data.Divisor = LongWord(-1) then Exit;

  if not (ClkNr in [Low(AllClocks)..High(AllClocks)]) then exit;
  AllClocks[ClkNr]:= Data;

  // Update the clock in ClockForm
  FormClocks.UpdateClock(ClkNr);
end;



// -------------------------------------------------------------
// Full PWM Map is received
// Args[1]: PWM block 0     Hex LongWords
// Args[2]: PWM blobk 1     Hex LongWords
// -------------------------------------------------------------
procedure RxGpioPiFullPwmMap;
var
  Data: TOnePWMblock;
  I: Integer;

begin
  for I:= Low(AllPWMblocks) to High(AllPWMblocks) do
  begin
    Data.Control:=          StrToIntDef('$' + Copy(Args[I+1], 1,  8), -1);
    Data.Status:=           StrToIntDef('$' + Copy(Args[I+1], 9,  8), -1);
    Data.DMA:=              StrToIntDef('$' + Copy(Args[I+1], 17, 8), -1);
    Data.Channel[0].Range:= StrToIntDef('$' + Copy(Args[I+1], 25, 8), -1);
    Data.Channel[0].Data:=  StrToIntDef('$' + Copy(Args[I+1], 33, 8), -1);
    Data.FIFO:=             StrToIntDef('$' + Copy(Args[I+1], 41, 8), -1);
    Data.Channel[1].Range:= StrToIntDef('$' + Copy(Args[I+1], 49, 8), -1);
    Data.Channel[1].Data:=  StrToIntDef('$' + Copy(Args[I+1], 57, 8), -1);

    if (Data.Control          <> LongWord(-1)) and
       (Data.Status           <> LongWord(-1)) and
       (Data.DMA              <> LongWord(-1)) and
       (Data.Channel[0].Range <> LongWord(-1)) and
       (Data.Channel[0].Data  <> LongWord(-1)) and
       (Data.FIFO             <> LongWord(-1)) and
       (Data.Channel[1].Range <> LongWord(-1)) and
       (Data.Channel[1].Data  <> LongWord(-1)) then
     begin
       // Store Data and update the PWM in PwmForm
       AllPWMblocks[I]:= Data;
       FormPwm.UpdatePwmBlock(I);
     end;
  end;
end;



// -------------------------------------------------------------
// A PWM group has been received
// Args[1]: PWM group 0-1
// Args[2]: All PWM data in the group (8 x LongWord in Hex)
// -------------------------------------------------------------
procedure RxPwmWholeGroup;
var
  PwmNr: Integer;
  Data: TOnePWMblock;

begin
  PwmNr:= StrToIntDef(Args[1], -1);
  if not (PwmNr in [Low(AllPWMblocks)..High(AllPWMblocks)]) then exit;

  Data.Control:=          StrToIntDef('$' + Copy(Args[2], 1,  8), -1);
  Data.Status:=           StrToIntDef('$' + Copy(Args[2], 9,  8), -1);
  Data.DMA:=              StrToIntDef('$' + Copy(Args[2], 17, 8), -1);
  Data.Channel[0].Range:= StrToIntDef('$' + Copy(Args[2], 25, 8), -1);
  Data.Channel[0].Data:=  StrToIntDef('$' + Copy(Args[2], 33, 8), -1);
  Data.FIFO:=             StrToIntDef('$' + Copy(Args[2], 41, 8), -1);
  Data.Channel[1].Range:= StrToIntDef('$' + Copy(Args[2], 49, 8), -1);
  Data.Channel[1].Data:=  StrToIntDef('$' + Copy(Args[2], 57, 8), -1);

  if Data.Control          = LongWord(-1) then exit;
  if Data.Status           = LongWord(-1) then exit;
  if Data.DMA              = LongWord(-1) then exit;
  if Data.Channel[0].Range = LongWord(-1) then exit;
  if Data.Channel[0].Data  = LongWord(-1) then exit;
  if Data.FIFO             = LongWord(-1) then exit;
  if Data.Channel[1].Range = LongWord(-1) then exit;
  if Data.Channel[1].Data  = LongWord(-1) then exit;

  AllPWMblocks[PwmNr]:= Data;

  // Update the PWM in PwmForm
  FormPwm.UpdatePwmBlock(PwmNr);
end;




// -------------------------------------------------------------
// Receiving UDP Data from a Gpio4Pi app
// -------------------------------------------------------------
procedure DataFromGpioPi(FromAdr, Data: String);
var
  Pos,I: Integer;

begin
  // Extract data until first ","
  Pos:= 1;
  Cmd:= ExtractSubstr(Data, Pos, [',']);

  // Extract any arguments, up to 4 arguments
  for I:= Low(Args) to High(Args) do Args[I]:= '';
  I:= 0;
  Repeat
    Inc(I);
    Args[I]:= ExtractSubstr(Data, Pos, [',']);
  until Args[I] = '';

  if FormSetup.IsLogViewChecked(Cmd) then
  begin
    WrtLog('RX from: ' + FromAdr +
           ', Cmd: ' + Cmd + ',' + Args[1] + ',' + Args[2] + ',' + Args[3] + ',' + Args[4]);
  end;

  case Cmd of
    TxtGpioPiFullGpioMap:  RxGpioPiFullGpioMap;
    TxtGpioPiFullClockMap: RxGpioPiFullClockMap;
    TxtGpioPiFullPwmMap:   RxGpioPiFullPwmMap;

    TxtGpioSetPinMode:     RxGpioSetPinMode;
    TxtGpioSetPull:        RxGpioSetPull;
    TxtGpioPinLevel:       RxGpioPinLevel;

    TxtGpioSetClock:       RxGpioSetClock;
    TxtPcmSetClock:        RxGpioSetClock;
    TxtPwmSetClock:        RxGpioSetClock;

    TxtPwmWholeGroup:      RxPwmWholeGroup;

    else WrtLog('Unknown RX from: ' + FromAdr + ', Cmd: ' + Cmd + ',' +
                Args[1] + ',' + Args[2] + ',' + Args[3] + ',' + Args[4]);
  end;

  FormOverview.UpdateView;
end;



// -----------------------------------------------
// Initialize this module
// Called from Initialization
// -----------------------------------------------
procedure InitializeThisModule;
var
  I: Integer;

begin
  // Clear workspace and set all pins to Undefined
  FillChar(AllPins, SizeOf(AllPins), 0);
  for I:= Low(AllPins) to High(AllPins) do AllPins[I].Mode:= $FF;

  FillChar(AllClocks, SizeOf(AllClocks), 0);
  FillChar(AllPWMblocks, SizeOf(AllPWMblocks), 0);
end;



Initialization
  InitializeThisModule;


Finalization


end.

