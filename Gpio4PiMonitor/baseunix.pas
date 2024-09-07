unit baseUnix;

// ------------------------------------------------------------------------
//
// A dummy unit so it can compile under Windows...
//
// DO NOT USE THIS UNIT ON THE RASPBERRY!!!
// ----------------------------------------
//
// There is a single routine in this unit to silence out the compiler.
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// ------------------------------------------------------------------------

{$mode ObjFPC}{$H+}
{$hints off}

interface

uses
  Classes, SysUtils;


type
  // Some Unix / C stuff
  cint   = LongInt;
  time_t = LongInt;

  timespec = record
    tv_sec:  time_t;  // Seconds
    tv_nsec: cint;    // Nanoseconds
  end;
  ptimespec = ^timespec;


function FpNanoSleep(req: ptimespec; rem: ptimespec): cint;


implementation


function FpNanoSleep(req: ptimespec; rem: ptimespec): cint;
begin
  exit(0);
end;


end.

