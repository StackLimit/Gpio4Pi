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
  Toff   = off_t;


function FpOpen(path: ShortString; flags: cint): cint;
function FpClose(fd: cint): cint;
function FpRead(fd: cint; var buf; nbytes: TSize): TsSize;

function Fpmmap(start: pointer; len: size_t; prot: cint; flags: cint;
                fd: cint; offst: off_t): pointer;
function Fpmunmap(start: pointer; len: size_t): cint;



implementation

uses
  StrUtils, GpioDefs;


var
  // Memory blocks for Pi used by Gpio4Pi
  MemGpio: Array[0..$FF div 4] of LongWord;
  MemClk:  Array[0..$AF div 4] of LongWord;
  MemPwm:  Array[0..$82F div 4] of LongWord;  // PWM1_OFFSET are at $800 !
  MemUart: Array[0..$BFF div 4] of LongWord;


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
var
  Mask: off_t;

begin
{$ifdef CPU64}
  Mask:= $0FFF000;
{$else}
  Mask:= $0FFF;
{$endif}

  case (offst and Mask) of
    CLOCK_BASE: exit(Addr(MemClk));
    GPIO_BASE:  exit(Addr(MemGpio));
    PWM_BASE:   exit(Addr(MemPwm));
    UART_BASE:  exit(Addr(MemUart));
  end;
  exit(nil);
end;

// ------------------------------------------------------------------------

function Fpmunmap(start: pointer; len: size_t): cint;
begin
  exit(0);
end;


// ------------------------------------------------------------------------


// ------------------------------------------------------------------------
//
// Some definitions to access memory areas
//
// Because we are using Array of LongWords, all offsets must be divided by 4.
//
// ------------------------------------------------------------------------

const
  // Offsets for UART
  UART0_OFFSETd4 = UART0_OFFSET div 4;
  UART2_OFFSETd4 = UART2_OFFSET div 4;
  UART3_OFFSETd4 = UART3_OFFSET div 4;
  UART4_OFFSETd4 = UART4_OFFSET div 4;
  UART5_OFFSETd4 = UART5_OFFSET div 4;

  UART_DRd4     = UART_DR     div 4;
  UART_RSRECRd4 = UART_RSRECR div 4;
  UART_FRd4     = UART_FR     div 4;
  UART_IBRDd4   = UART_IBRD   div 4;
  UART_FBRDd4   = UART_FBRD   div 4;
  UART_LCRHd4   = UART_LCRH   div 4;
  UART_CRd4     = UART_CR     div 4;
  UART_IFLSd4   = UART_IFLS   div 4;
  UART_IMSCd4   = UART_IMSC   div 4;
  UART_RISd4    = UART_RIS    div 4;
  UART_MISd4    = UART_MIS    div 4;
  UART_ICRd4    = UART_ICR    div 4;
  UART_DMACRd4  = UART_DMACR  div 4;
  UART_ITCR     = UART_ITCR   div 4;
  UART_ITIPd4   = UART_ITIP   div 4;
  UART_ITOPd4   = UART_ITOP   div 4;
  UART_TDRd4    = UART_TDR    div 4;

// ------------------------------------------------------------------------



Initialization
  // Clear all memory
  FillChar(MemGpio, SizeOf(MemGpio), 0);
  FillChar(MemClk,  SizeOf(MemClk),  0);
  FillChar(MemPwm,  SizeOf(MemPwm),  0);
  FillChar(MemUart, SizeOf(MemUart), 0);

  // Some default bits in Uart ram
  MemUart[UART0_OFFSETd4 + UART_FRd4]:= UART_FR_RXFE or UART_FR_TXFE;
  MemUart[UART2_OFFSETd4 + UART_FRd4]:= UART_FR_RXFE or UART_FR_TXFE;
  MemUart[UART3_OFFSETd4 + UART_FRd4]:= UART_FR_RXFE or UART_FR_TXFE;
  MemUart[UART4_OFFSETd4 + UART_FRd4]:= UART_FR_RXFE or UART_FR_TXFE;
  MemUart[UART5_OFFSETd4 + UART_FRd4]:= UART_FR_RXFE or UART_FR_TXFE;


Finalization


end.

