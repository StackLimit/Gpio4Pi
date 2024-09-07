unit Unix;

// ------------------------------------------------------------------------
//
// A Dummy unit so it can compile under Windows...
//
// DO NOT USE THIS UNIT ON THE RASPBERRY!!!
// ----------------------------------------
//
// In this unit there are definitions witch are used by Gpio4Pi, for example
// FpOpen(), FpClose(), FpRead(), Fpmmap() and Fpmunmap().
//
// Furthermore, this unit also emulates memory for Raspberry Pi 4,
// so all Fpmmap() returns a valid pointer to some memory.
//
// This memory is regularly scanned for changes and changes are sent to
// Gpio4PiClient if this is used.
//
// Data is also received from Gpio4PiClient such as Input changes,
// and this is put in memory so that Gpio4Pi can read Input high/low values.
//
// Still under development and therefore not quite finished.
// Copyright (c) 2024 Jan Andersen
// ------------------------------------------------------------------------

{$mode ObjFPC}{$H+}
{$hints off}

interface

uses
  Classes, SysUtils, Windows;

const
  O_RdWr   = 2;
  O_Sync   = $1000;
  O_RdOnly = 0;

  PROT_READ  = $01;
  PROT_WRITE = $02;
  MAP_SHARED = $01;
  MAP_FAILED = Pointer(-1);


type
  // Some Unix / C stuff
  cint   = LongInt;
  off_t  = LongInt;
  TSize  = QWord;
  TsSize = LongInt;


function FpOpen(path: ShortString; flags: cint): cint;
function FpClose(fd: cint): cint;
function FpRead(fd: cint; var buf; nbytes: TSize): TsSize;

function Fpmmap(start: pointer; len: size_t; prot: cint; flags: cint;
                fd: cint; offst: off_t): pointer;
function Fpmunmap(start: pointer; len: size_t): cint;



implementation

uses
  StrUtils, UdpRxTx, GpioPiTxtCmd, GpioDefs;


var
  // Memory blocks for Pi used by Gpio4Pi
  MemGpio: Array[0..$FF div 4] of LongWord;
  MemClk:  Array[0..$AF div 4] of LongWord;
  MemPwm:  Array[0..$82F div 4] of LongWord;  // PWM1_OFFSET are at $800 !


// ------------------------------------------------------------------------

function FpOpen(path: ShortString; flags: cint): cint;
begin
  path:= path + #0;
  OutputDebugString(@path[1]);     // Print to Event Log
  exit(10);                        // Just return some file handle
end;

// ------------------------------------------------------------------------

function FpClose(fd: cint): cint;
begin
  exit(0);
end;

// ------------------------------------------------------------------------

// Dummy read for '/proc/device-tree/system/linux,revision'
function FpRead(fd: cint; var buf; nbytes: TSize): TsSize;
begin
  if nbytes = SizeOf(LongWord) then
  begin
    LongWord(buf):= SwapEndian($0C03115);  // Revision nr. for Pi 4B
    exit(SizeOf(LongWord));
  end;
  exit(0);
end;

// ------------------------------------------------------------------------

// Fpmmap. Return address for memory areas
function Fpmmap(start: pointer; len: size_t; prot: cint; flags: cint;
                fd: cint; offst: off_t): pointer;
begin
  case (offst and $0FFF) of
    CLOCK_BASE: exit(Addr(MemClk));
    GPIO_BASE:  exit(Addr(MemGpio));
    GPIO_PWM:   exit(Addr(MemPwm));
  end;
  exit(nil);
end;

// ------------------------------------------------------------------------

function Fpmunmap(start: pointer; len: size_t): cint;
begin
  exit(0);
end;


// ------------------------------------------------------------------------
//
// Some definitions to access memory areas
//
// Because we are using Array of LongWords, all offsets must be divided by 4.
//
// ------------------------------------------------------------------------

const
  // Offsets for GPIO Mem
  GPSET0d4    = GPSET0    div 4;   // GPIO Pin Output Set
  GPCLR0d4    = GPCLR0    div 4;   // GPIO Pin Output Clear
  GPLEV0d4    = GPLEV0    div 4;   // GPIO Pin Level
  GPPUPPDN0d4 = GPPUPPDN0 div 4;   // GPIO Pull-up / Pull-down Register (Pi4)

  // Offsets for Clocks
  CLK_GP0_CTLd4 = CLK_GP0_CTL div 4;   // GPIO clocks (0-2)
  CLK_GP0_DIVd4 = CLK_GP0_DIV div 4;
  CLK_PCM_CTLd4 = CLK_PCM_CTL div 4;   // PCM clock
  CLK_PCM_DIVd4 = CLK_PCM_DIV div 4;
  CLK_PWM_CTLd4 = CLK_PWM_CTL div 4;   // PWM clock
  CLK_PWM_DIVd4 = CLK_PWM_DIV div 4;

  // Offsets for PWM
  PWM0_OFFSETd4 = PWM0_OFFSET div 4;  // The PWM0 register base address is 0x7e20c000 and
  PWM1_OFFSETd4 = PWM1_OFFSET div 4;  // the PWM1 register base address is 0x7e20c800

  PWM_CONTROLd4 = PWM_CONTROL div 4;
  PWM_STATUSd4  = PWM_STATUS  div 4;
  PWM_DMACTLd4  = PWM_DMACTL  div 4;
  PWM0_RANGEd4  = PWM0_RANGE  div 4;
  PWM0_DATAd4   = PWM0_DATA   div 4;
  PWM_FIFOd4    = PWM_FIFO    div 4;
  PWM1_RANGEd4  = PWM1_RANGE  div 4;
  PWM1_DATAd4   = PWM1_DATA   div 4;



// ------------------------------------------------------------------------
//
// Transmit data to Client
//
// ------------------------------------------------------------------------

// -----------------------------------------------
// Send Gpio Pin State
// -----------------------------------------------
procedure SendGpioPinState(Pin, State: Byte);
var
  TxData: String;

begin
  TxData:= TxtGpioPinLevel + ',' + IntToStr(Pin) + ',';

  if State = 0
    then TxData:= TxData + '0'
    else TxData:= TxData + '1';

  UdpRxTxSend(TxData);
end;


// -----------------------------------------------
// Send Gpio Pin Mode
// -----------------------------------------------
procedure SendGpioPinMode(Pin, Mode: Byte);
var
  TxData: String;

begin
  TxData:= TxtGpioSetPinMode + ',' + IntToStr(Pin) + ',' + IntToStr(Mode);
  UdpRxTxSend(TxData);
end;


// -----------------------------------------------
// Send Gpio Pull Up/Down
// -----------------------------------------------
procedure SendGpioPinPull(Pin, Pull: Byte);
var
  TxData: String;

begin
  TxData:= TxtGpioSetPull + ',' + IntToStr(Pin) + ',' + IntToStr(Pull);
  UdpRxTxSend(TxData);
end;



// -----------------------------------------------
// Send Clock Control and Divisor
// -----------------------------------------------
procedure SendClockCtlDiv(ClkNr: Byte; Control, Divisor: LongWord);
var
  TxData: String;

begin
  case ClkNr of
    0..2: TxData:= TxtGpioSetClock;
    3:    TxData:= TxtPcmSetClock;
    4:    TxData:= TxtPwmSetClock;
    Else exit;
  end;

  TxData:= TxData + ',' + IntToStr(ClkNr) + ',' +
           IntToStr(Control) + ',' + IntToStr(Divisor);
  UdpRxTxSend(TxData);
end;



// -----------------------------------------------
// Send PWM data
// -----------------------------------------------
procedure SendPwmData(GrpNo: Integer; Data: Array of LongWord);
var
  TxData: String;
  I: Integer;

begin
  TxData:= TxtPwmWholeGroup + ',' + IntToStr(GrpNo) + ',';

  for I:= Low(Data) to High(Data) do
    TxData:= TxData + IntToHex(Data[I], 8);

  UdpRxTxSend(TxData);
end;



// ------------------------------------------------------------------------
//
// MEM scanner in Thread
//
// ------------------------------------------------------------------------

type
  TMemThread = class(TThread)
  private
    Step: Integer;
    OldGPFSEL:   Array[0..5] of LongWord;
    OldGPLEV:    Array[0..1] of LongWord;
    OldGPPUPPDN: Array[0..3] of LongWord;
    OldCLKCTL:   Array[0..4] of LongWord;
    OldCLKDIV:   Array[0..4] of LongWord;
    OldPWMblock: Array[0..1,0..7] of LongWord;
    procedure ScanGPIOMem;
    procedure ScanGPIOClockMem;
    procedure ScanPWMMem;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
  end;


var
  MemThread: TMemThread;


// -------------------------------------------------------------
// Scan GPIO Clock 0-2, PCM and PWM clocks for changes
// -------------------------------------------------------------
procedure TMemThread.ScanGPIOClockMem;
var
  I: Integer;

begin
  // GPIO Clocks 0 - 2
  for I:= 0 to 2 do
  begin
    if (MemClk[CLK_GP0_CTLd4+(I*2)] <> OldCLKCTL[I]) or
       (MemClk[CLK_GP0_DIVd4+(I*2)] <> OldCLKDIV[I]) then
    begin
      // Clock Changed, transmit to client
      SendClockCtlDiv(I, MemClk[CLK_GP0_CTLd4+(I*2)], MemClk[CLK_GP0_DIVd4+(I*2)]);
      OldCLKCTL[I]:= MemClk[CLK_GP0_CTLd4+(I*2)];
      OldCLKDIV[I]:= MemClk[CLK_GP0_DIVd4+(I*2)];
    end;
  end;

  // PCM Clock
  if (MemClk[CLK_PCM_CTLd4] <> OldCLKCTL[3]) or
     (MemClk[CLK_PCM_DIVd4] <> OldCLKDIV[3]) then
  begin
    // Clock Changed, transmit to client
    SendClockCtlDiv(3, MemClk[CLK_PCM_CTLd4], MemClk[CLK_PCM_DIVd4]);
    OldCLKCTL[3]:= MemClk[CLK_PCM_CTLd4];
    OldCLKDIV[3]:= MemClk[CLK_PCM_DIVd4];
  end;

  // PWM Clock
  if (MemClk[CLK_PWM_CTLd4] <> OldCLKCTL[4]) or
     (MemClk[CLK_PWM_DIVd4] <> OldCLKDIV[4]) then
  begin
    // Clock Changed, transmit to client
    SendClockCtlDiv(4, MemClk[CLK_PWM_CTLd4], MemClk[CLK_PWM_DIVd4]);
    OldCLKCTL[4]:= MemClk[CLK_PWM_CTLd4];
    OldCLKDIV[4]:= MemClk[CLK_PWM_DIVd4];
  end;
end;


// -------------------------------------------------------------
// Scan PWM blocks for changes
// -------------------------------------------------------------
procedure TMemThread.ScanPWMMem;
var
  I: Integer;
  Ofs: LongWord;

begin
  Ofs:= PWM0_OFFSETd4;

  for I:= 0 to 1 do
  begin
    if (MemPwm[Ofs+PWM_CONTROLd4] <> OldPWMblock[I,0]) or
       (MemPwm[Ofs+PWM_STATUSd4] <> OldPWMblock[I,1]) or
       (MemPwm[Ofs+PWM_DMACTLd4] <> OldPWMblock[I,2]) or
       (MemPwm[Ofs+PWM0_RANGEd4] <> OldPWMblock[I,3]) or
       (MemPwm[Ofs+PWM0_DATAd4] <> OldPWMblock[I,4]) or
       (MemPwm[Ofs+PWM_FIFOd4] <> OldPWMblock[I,5]) or
       (MemPwm[Ofs+PWM1_RANGEd4] <> OldPWMblock[I,6]) or
       (MemPwm[Ofs+PWM1_DATAd4] <> OldPWMblock[I,7]) then
    begin
      // PWM channel changed, transmit to client
      SendPwmData(I, [MemPwm[Ofs+PWM_CONTROLd4],
                      MemPwm[Ofs+PWM_STATUSd4],
                      MemPwm[Ofs+PWM_DMACTLd4],
                      MemPwm[Ofs+PWM0_RANGEd4],
                      MemPwm[Ofs+PWM0_DATAd4],
                      MemPwm[Ofs+PWM_FIFOd4],
                      MemPwm[Ofs+PWM1_RANGEd4],
                      MemPwm[Ofs+PWM1_DATAd4]
                      ]);

      OldPWMblock[I,0]:= MemPwm[Ofs+PWM_CONTROLd4];
      OldPWMblock[I,1]:= MemPwm[Ofs+PWM_STATUSd4];
      OldPWMblock[I,2]:= MemPwm[Ofs+PWM_DMACTLd4];
      OldPWMblock[I,3]:= MemPwm[Ofs+PWM0_RANGEd4];
      OldPWMblock[I,4]:= MemPwm[Ofs+PWM0_DATAd4];
      OldPWMblock[I,5]:= MemPwm[Ofs+PWM_FIFOd4];
      OldPWMblock[I,6]:= MemPwm[Ofs+PWM1_RANGEd4];
      OldPWMblock[I,7]:= MemPwm[Ofs+PWM1_DATAd4];
    end;

    Ofs:= PWM1_OFFSETd4;
  end;
end;



// -------------------------------------------------------------
// Scan all GPIO's for changes
// -------------------------------------------------------------
procedure TMemThread.ScanGPIOMem;
var
  I,J: Integer;
  Mask: LongWord;
  Diff: LongWord;

begin
  // GPIO Function Select 0 - 5
  for I:= Low(OldGPFSEL) to High(OldGPFSEL) do
  begin
    if MemGpio[I] <> OldGPFSEL[I] then
    begin
      Diff:= MemGpio[I] xor OldGPFSEL[I];
      Mask:= 7;
      for J:= 0 to 9 do
      begin
        if (Diff and Mask) <> 0 then
        begin
          // Mode Changed, transmit to Client
          SendGpioPinMode((I*10)+J, (MemGpio[I] and Mask) shr (J*3));
        end;
        Mask:= Mask shl 3;
      end;
      OldGPFSEL[I]:= MemGpio[I];
    end;
  end;

  // GPIO Pin Output Set 0 - 1 (One shut)
  for I:= 0 to 1 do
  begin
    if MemGpio[GPSET0d4+I] <> 0 then
    begin
      Mask:= 1;
      for J:= 0 to 31 do
      begin
        if (MemGpio[GPSET0d4+I] and Mask) <> 0 then
        begin
          // OutputSet Changed, Set bit in Level memory
          MemGpio[GPLEV0d4+I]:= MemGpio[GPLEV0d4+I] or Mask;
        end;
        Mask:= Mask shl 1;
      end;
      MemGpio[GPSET0d4+I]:= 0;
    end;
  end;

  // GPIO Pin Output Clear 0 - 1 (One shut)
  for I:= 0 to 1 do
  begin
    if MemGpio[GPCLR0d4+I] <> 0 then
    begin
      Mask:= 1;
      for J:= 0 to 31 do
      begin
        if (MemGpio[GPCLR0d4+I] and Mask) <> 0 then
        begin
          // OutputClear Changed, Reset bit in Level memory
          MemGpio[GPLEV0d4+I]:= MemGpio[GPLEV0d4+I] and (not Mask);
        end;
        Mask:= Mask shl 1;
      end;
      MemGpio[GPCLR0d4+I]:= 0;
    end;
  end;

  // GPIO Pin Level 0 - 1
  for I:= 0 to 1 do
  begin
    if MemGpio[GPLEV0d4+I] <> OldGPLEV[I] then
    begin
      Diff:= MemGpio[GPLEV0d4+I] xor OldGPLEV[I];
      Mask:= 1;
      for J:= 0 to 31 do
      begin
        if (Diff and Mask) <> 0 then
        begin
          // Pin Changed, transmit to Client
          SendGpioPinState((I*32)+J, (MemGpio[GPLEV0d4+I] and Mask) shr J);
        end;
        Mask:= Mask shl 1;
      end;
      OldGPLEV[I]:= MemGpio[GPLEV0d4+I];
    end;
  end;

  // GPIO Puul Up/Down 0 - 3
  for I:= 0 to 3 do
  begin
    if MemGpio[GPPUPPDN0d4+I] <> OldGPPUPPDN[I] then
    begin
      Diff:= MemGpio[GPPUPPDN0d4+I] xor OldGPPUPPDN[I];
      Mask:= 3;
      for J:= 0 to 15 do
      begin
        if (Diff and Mask) <> 0 then
        begin
          // Pull Up/Down Changed, transmit to Client
          SendGpioPinPull((I*15)+J, (MemGpio[GPPUPPDN0d4+I] and Mask) shr (J*2));
        end;
        Mask:= Mask shl 2;
      end;
      OldGPPUPPDN[I]:= MemGpio[GPPUPPDN0d4+I];
    end;
  end;
end;


// -------------------------------------------------------------
// Thread Execute loop
// -------------------------------------------------------------
procedure TMemThread.Execute;
begin
  Step:= 0;

  while (not Terminated) do
  begin
    Step:= Step + 1;
    if Step > 3 then Step:= 1;

    case Step of
      1: Synchronize(@ScanGPIOMem);       // GPIO Memory
      2: Synchronize(@ScanGPIOClockMem);  // GPIO Clock Memory
      3: Synchronize(@ScanPWMMem);        // PWM Memory
      else Step:= 0;
    end;

    Sleep(1);
//    DelayMicroseconds(10000);  // 10mS <- This uses a LOT of CPU time
  end;
end;


// -------------------------------------------------------------
// Thread Create
// -------------------------------------------------------------
constructor TMemThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate:= True;
end;



// ------------------------------------------------------------------------
//
// Callback from UDP RX thread
//
// ------------------------------------------------------------------------
var
  // Received data. Command and up to 4 args
  Cmd: String;
  Args: Array[1..4] of String;


// -----------------------------------------------
// Client asked for a full GPIO map
// Send Cmd,Modes,Puds,States return
// All pins i this map are GPIO pins
// All Modes,Puds,States are Byte Hex values
// -----------------------------------------------
procedure RxGpioPiReqFullMap;
var
  TxData: String;
  I,J: Integer;
  B: Byte;

begin
  TxData:= TxtGpioPiFullGpioMap + ',';

  // Get all modes from memory (60 pins total)
  for I:= 0 to 5 do
  begin
    for J:= 0 to 9 do
    begin
      B:= (MemGpio[I] and (7 shl (J*3))) shr (J*3);  // Each FSEL (mode) are 3 bits
      TxData:= TxData + IntToHex(B, 2);              // Byte
    end;
  end;
  TxData:= TxData + ',';

  // Get all PullUp/Down from memory (60 pins total)
  for I:= 0 to 3 do
  begin
    for J:= 0 to 15 do
    begin
      B:= (MemGpio[GPPUPPDN0d4+I] and (3 shl (J*2))) shr (J*2); // Each GPIO_PUP_PDN_CNTRL are 2 bits
      TxData:= TxData + IntToHex(B, 2);                         // Byte
    end;
  end;
  TxData:= TxData + ',';

  // Get all States from memory (60 pins total)
  for I:= 0 to 1 do
  begin
    for J:= 0 to 31 do
    begin
      B:= (MemGpio[GPLEV0d4+I] and (1 shl J)) shr J;    // Each LEV are 1 bits
      TxData:= TxData + IntToHex(B, 2);                 // Byte
    end;
  end;
  UdpRxTxSend(TxData);

  // Next, send full Clock map
  Sleep(1);
  TxData:= TxtGpioPiFullClockMap + ',';
  TxData:= TxData + IntToHex(MemClk[CLK_GP0_CTLd4+0], 8);
  TxData:= TxData + IntToHex(MemClk[CLK_GP0_CTLd4+2], 8);
  TxData:= TxData + IntToHex(MemClk[CLK_GP0_CTLd4+4], 8);
  TxData:= TxData + IntToHex(MemClk[CLK_PCM_CTLd4], 8);
  TxData:= TxData + IntToHex(MemClk[CLK_PWM_CTLd4], 8);
  TxData:= TxData + ',';
  TxData:= TxData + IntToHex(MemClk[CLK_GP0_DIVd4+0], 8);
  TxData:= TxData + IntToHex(MemClk[CLK_GP0_DIVd4+2], 8);
  TxData:= TxData + IntToHex(MemClk[CLK_GP0_DIVd4+4], 8);
  TxData:= TxData + IntToHex(MemClk[CLK_PCM_DIVd4], 8);
  TxData:= TxData + IntToHex(MemClk[CLK_PWM_DIVd4], 8);
  UdpRxTxSend(TxData);

  // Next, send full PWM map
  Sleep(1);
  TxData:= TxtGpioPiFullPwmMap + ',';
  TxData:= TxData + IntToHex(MemPwm[PWM0_OFFSETd4+PWM_CONTROLd4], 8);
  TxData:= TxData + IntToHex(MemPwm[PWM0_OFFSETd4+PWM_STATUSd4], 8);
  TxData:= TxData + IntToHex(MemPwm[PWM0_OFFSETd4+PWM_DMACTLd4], 8);
  TxData:= TxData + IntToHex(MemPwm[PWM0_OFFSETd4+PWM0_RANGEd4], 8);
  TxData:= TxData + IntToHex(MemPwm[PWM0_OFFSETd4+PWM0_DATAd4], 8);
  TxData:= TxData + IntToHex(MemPwm[PWM0_OFFSETd4+PWM_FIFOd4], 8);
  TxData:= TxData + IntToHex(MemPwm[PWM0_OFFSETd4+PWM1_RANGEd4], 8);
  TxData:= TxData + IntToHex(MemPwm[PWM0_OFFSETd4+PWM1_DATAd4], 8);
  TxData:= TxData + ',';
  TxData:= TxData + IntToHex(MemPwm[PWM1_OFFSETd4+PWM_CONTROLd4], 8);
  TxData:= TxData + IntToHex(MemPwm[PWM1_OFFSETd4+PWM_STATUSd4], 8);
  TxData:= TxData + IntToHex(MemPwm[PWM1_OFFSETd4+PWM_DMACTLd4], 8);
  TxData:= TxData + IntToHex(MemPwm[PWM1_OFFSETd4+PWM0_RANGEd4], 8);
  TxData:= TxData + IntToHex(MemPwm[PWM1_OFFSETd4+PWM0_DATAd4], 8);
  TxData:= TxData + IntToHex(MemPwm[PWM1_OFFSETd4+PWM_FIFOd4], 8);
  TxData:= TxData + IntToHex(MemPwm[PWM1_OFFSETd4+PWM1_RANGEd4], 8);
  TxData:= TxData + IntToHex(MemPwm[PWM1_OFFSETd4+PWM1_DATAd4], 8);
  UdpRxTxSend(TxData);
end;


// -----------------------------------------------
// Client sets a GPIO pin to On/Off
// -----------------------------------------------
procedure RxGpioSetGpioPin;
var
  Pin,Val: Integer;
  Mask: LongWord;

begin
  // Args[1] are Pin number, Args[2] are value
  Pin:= StrToIntDef(Args[1], -1);
  if (Pin = -1) then exit;

  Val:= StrToIntDef(Args[2], -1);
  if (Val = -1) then exit;

  Mask:=  1 shl (Pin and $1F);
  if Val = 0
    then MemGpio[GPLEV0d4+(Pin div 32)]:= MemGpio[GPLEV0d4+(Pin div 32)] and (not Mask)
    else MemGpio[GPLEV0d4+(Pin div 32)]:= MemGpio[GPLEV0d4+(Pin div 32)] or Mask;
end;


// -----------------------------------------------
// Data has been received from UDP
// -----------------------------------------------
procedure UDPdataRX(Data: String);
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

//  WrtLog('RxCmd:' + Cmd + ',' + Args[1] + ',' + Args[2] + ',' + Args[3] + ',' + Args[4]);

  case Cmd of
    TxtGpioPiReqFullMap: RxGpioPiReqFullMap;
    TxtGpioSetGpioPin:   RxGpioSetGpioPin;
  end;
end;



Initialization
  // Clear all memory
  FillChar(MemGpio, SizeOf(MemGpio), 0);
  FillChar(MemClk,  SizeOf(MemClk),  0);
  FillChar(MemPwm,  SizeOf(MemPwm),  0);

  // Start Memory scanning Thread
  MemThread:= TMemThread.Create(False);

  // Setup Network callback and Start UDP Rx/Tx
  RxDataEvent:= @UDPdataRX;
  UdpRxTxInit;


Finalization

  MemThread.Terminate;


end.

