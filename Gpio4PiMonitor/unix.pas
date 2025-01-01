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


Initialization
  // Clear all memory
  FillChar(MemGpio, SizeOf(MemGpio), 0);
  FillChar(MemClk,  SizeOf(MemClk),  0);
  FillChar(MemPwm,  SizeOf(MemPwm),  0);

Finalization


end.

