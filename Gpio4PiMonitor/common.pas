unit Common;

// -----------------------------------------------------------------
//
// Various helper routines
//
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// -----------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, SysUtils;



function LongToTrueFalse(L: LongWord): String;

function GetExeDir: String;
function GetConfigFilNavn: String;

function ArgsToStr(Args: array of LongInt): String;


implementation

uses
  GpioDefs;



// -------------------------------------------------------------
// Return text "True" or "False"
// -------------------------------------------------------------
function LongToTrueFalse(L: LongWord): String;
begin
  if L = 0
    then exit('False')
    else exit('True');
end;



// -------------------------------------------------------------
// Return Directory for our Exe file
// -------------------------------------------------------------
function GetExeDir: String;
begin
  Result:= ExtractFilePath(Application.Params[0]);
end;


// -------------------------------------------------------------
// Return full name for config file
// -------------------------------------------------------------
function GetConfigFilNavn: String;
begin
  Result:= GetExeDir + 'Gpio4PiMonitor.cfg';
end;



// -----------------------------------------------
// Make arguments as a comma-separated string
// -----------------------------------------------
function ArgsToStr(Args: array of LongInt): String;
var
  RetVal: String;
  I: Integer;

begin
  RetVal:= '';
  for I:= Low(Args) to High(Args) do RetVal:= RetVal + ',' + IntToStr(Args[I]);
  Result:= RetVal;
end;





end.

