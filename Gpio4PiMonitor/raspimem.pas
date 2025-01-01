unit RasPiMem;

// -------------------------------------------------------------------
//
// Memory scanner
// Scans PI's memory for changes and updates Forms when changes occur
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// -------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Gpio4Pi;


type
  // Special GPIO Class exposing the Mem Pointers
  TPiGpioMem = class(TPiGpio)
  private
  public
  end;


var
  PiGpio: TPiGpioMem;


implementation

Uses
  GpioDefs, MainForm, ClockForm, PwmForm, OverviewForm;


// ------------------------------------------------------------------------

function SetPtr(BasePtr: Pointer; Ofs: LongWord): Pointer; inline;
begin
  Result:= BasePtr + Ofs;
end;

function SetPtr(BasePtr: Pointer; Ofs1,Ofs2: LongWord): Pointer; inline; overload;
begin
  Result:= BasePtr + Ofs1 + Ofs2;
end;

// ------------------------------------------------------------------------


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
    procedure ScanGPIOClockMem;
    procedure ScanPWMMem;
    procedure ScanGPIOMem;
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
  pCtl,pDiv: ^LongWord;

begin
  if PiGpio.UsingGpioMem then exit;

  // GPIO Clocks 0 - 2
  for I:= 0 to 2 do
  begin
    pCtl:= SetPtr(PiGpio.PClkMem, CLK_GP0_CTL, I*8);
    pDiv:= SetPtr(PiGpio.PClkMem, CLK_GP0_DIV, I*8);

    if (pCtl^ <> OldCLKCTL[I]) or
       (pDiv^ <> OldCLKDIV[I]) then
    begin
      // Clock Changed, update Clock Form and Overview
      FormClocks.UpdateClock(I);
      FormOverview.UpdateView;
      OldCLKCTL[I]:= pCtl^;
      OldCLKDIV[I]:= pDiv^;
    end;
  end;

  // PCM Clock
  pCtl:= SetPtr(PiGpio.PClkMem, CLK_PCM_CTL);
  pDiv:= SetPtr(PiGpio.PClkMem, CLK_PCM_DIV);

  if (pCtl^ <> OldCLKCTL[3]) or
     (pDiv^ <> OldCLKDIV[3]) then
  begin
    // Clock Changed, update Clock Form and Overview
    FormClocks.UpdateClock(3);
    FormOverview.UpdateView;
    OldCLKCTL[3]:= pCtl^;
    OldCLKDIV[3]:= pDiv^;
  end;

  // PWM Clock
  pCtl:= SetPtr(PiGpio.PClkMem, CLK_PWM_CTL);
  pDiv:= SetPtr(PiGpio.PClkMem, CLK_PWM_DIV);

  if (pCtl^ <> OldCLKCTL[4]) or
     (pDiv^ <> OldCLKDIV[4]) then
  begin
    // Clock Changed, update Clock Form and Overview
    FormClocks.UpdateClock(4);
    FormOverview.UpdateView;
    OldCLKCTL[4]:= pCtl^;
    OldCLKDIV[4]:= pDiv^;
  end;
end;


// -------------------------------------------------------------
// Scan PWM blocks for changes
// -------------------------------------------------------------
procedure TMemThread.ScanPWMMem;
var
  I: Integer;
  Ofs: LongWord;
  pCtl,pSta,pDma,pRng0,pDat0,pFif,pRng1,pDat1: ^LongWord;

begin
  if PiGpio.UsingGpioMem then exit;

  Ofs:= PWM0_OFFSET;

  for I:= 0 to 1 do
  begin
    pCtl:=  SetPtr(PiGpio.PPwmMem, Ofs, PWM_CONTROL);
    pSta:=  SetPtr(PiGpio.PPwmMem, Ofs, PWM_STATUS);
    pDma:=  SetPtr(PiGpio.PPwmMem, Ofs, PWM_DMACTL);
    pRng0:= SetPtr(PiGpio.PPwmMem, Ofs, PWM0_RANGE);
    pDat0:= SetPtr(PiGpio.PPwmMem, Ofs, PWM0_DATA);
    pFif:=  SetPtr(PiGpio.PPwmMem, Ofs, PWM_FIFO);
    pRng1:= SetPtr(PiGpio.PPwmMem, Ofs, PWM1_RANGE);
    pDat1:= SetPtr(PiGpio.PPwmMem, Ofs, PWM1_DATA);

    if (pCtl^  <> OldPWMblock[I,0]) or
       (pSta^  <> OldPWMblock[I,1]) or
       (pDma^  <> OldPWMblock[I,2]) or
       (pRng0^ <> OldPWMblock[I,3]) or
       (pDat0^ <> OldPWMblock[I,4]) or
       (pFif^  <> OldPWMblock[I,5]) or
       (pRng1^ <> OldPWMblock[I,6]) or
       (pDat1^ <> OldPWMblock[I,7]) then
    begin
      // PWM channel changed, update PWM Form and Overview
      FormPwm.UpdatePwmBlock(I);
      FormOverview.UpdateView;

      OldPWMblock[I,0]:= pCtl^;
      OldPWMblock[I,1]:= pSta^;
      OldPWMblock[I,2]:= pDma^;
      OldPWMblock[I,3]:= pRng0^;
      OldPWMblock[I,4]:= pDat0^;
      OldPWMblock[I,5]:= pFif^;
      OldPWMblock[I,6]:= pRng1^;
      OldPWMblock[I,7]:= pDat1^;
    end;

    Ofs:= PWM1_OFFSET;
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
  pMem: ^LongWord;

begin
  // GPIO Function Select 0 - 5
  for I:= Low(OldGPFSEL) to High(OldGPFSEL) do
  begin
    pMem:= SetPtr(PiGpio.PGpioMem, I*4);
    if pMem^ <> OldGPFSEL[I] then
    begin
      Diff:= pMem^ xor OldGPFSEL[I];
      Mask:= 7;
      for J:= 0 to 9 do
      begin
        if (Diff and Mask) <> 0 then
        begin
          // Mode Changed, Update Combo and Overview
          FormMain.UpdatePinCombo((I*10) + J);
          FormOverview.UpdateView;
        end;
        Mask:= Mask shl 3;
      end;
      OldGPFSEL[I]:= pMem^;
    end;
  end;


  // GPIO Pin Level 0 - 1
  for I:= 0 to 1 do
  begin
    pMem:= SetPtr(PiGpio.PGpioMem, GPLEV0+(I*4));
    if pMem^ <> OldGPLEV[I] then
    begin
      Diff:= pMem^ xor OldGPLEV[I];
      Mask:= 1;
      for J:= 0 to 31 do
      begin
        if (Diff and Mask) <> 0 then
        begin
          // Pin Changed, Update Combo
          FormMain.UpdatePinCombo((I*32) + J);
        end;
        Mask:= Mask shl 1;
      end;
      OldGPLEV[I]:= pMem^;
    end;
  end;

  // GPIO Puul Up/Down 0 - 3
  for I:= 0 to 3 do
  begin
    pMem:= SetPtr(PiGpio.PGpioMem, GPPUPPDN0+(I*4));
    if pMem^ <> OldGPPUPPDN[I] then
    begin
      Diff:= pMem^ xor OldGPPUPPDN[I];
      Mask:= 3;
      for J:= 0 to 15 do
      begin
        if (Diff and Mask) <> 0 then
        begin
          // Pull Up/Down Changed, Update Combo
          FormMain.UpdatePinCombo((I*15) + J);
        end;
        Mask:= Mask shl 2;
      end;
      OldGPPUPPDN[I]:= pMem^;
    end;
  end;
end;


// -------------------------------------------------------------
// Thread Execute loop
// -------------------------------------------------------------
procedure TMemThread.Execute;
begin
  Sleep(1000);  // Wait until FormMain are ready
  Step:= 0;

  while (not Terminated) do
  begin
    if PiGpio <> Nil then
    begin
      Step:= Step + 1;
      if Step > 3 then Step:= 1;

      case Step of
        1: Synchronize(@ScanGPIOMem);       // GPIO Memory
        2: Synchronize(@ScanGPIOClockMem);  // GPIO Clock Memory
        3: Synchronize(@ScanPWMMem);        // PWM Memory
        else Step:= 0;
      end;
    end;

    Sleep(10);
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




Initialization
  PiGpio:= Nil;

  // Start Memory scanning Thread
  MemThread:= TMemThread.Create(False);


Finalization
  if MemThread <> nil then MemThread.Terminate;


end.
