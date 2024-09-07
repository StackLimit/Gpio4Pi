unit GPIOcheckbox;

// -----------------------------------------------------------------
//
// Our own "CheckBox" for GPIO pins
// Behaves (almost) like a CheckBox
// However, we can change the colors in this one
// Build on a TPanel were do all the stuff in the OnPaint Event
//
// And then it has the extra stuff associated with GPIO
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// -----------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls;


const
  // Colors. $BBGGRR
  clLightRed    = TColor($B0B0FF);
  clLightGreen  = TColor($B0FFB0);
  clLightBlue   = TColor($FFB0B0);
  clLightYellow = TColor($B0FFFF);


type
  // Our GPIO CheckBox
  TGpioCheckBox = class(TPanel)
  private
    FChecked:   Boolean;
    FOnChange:  TNotifyEvent;
    FGpioMode:  Integer;                  // Input, Output, etc.
    FGpioPud:   Integer;                  // PullUp, PullDown, etc.
    FGpioState: Integer;                  // On, Off, etc.
    procedure GpioPinClick(Sender: TObject);
    procedure GpioPinPaint(Sender: TObject);
  protected
    procedure SetGpioMode(GpioMode: Integer);
    procedure SetGpioPud({%H-}GpioMode: Integer);
    procedure SetGpioState({%H-}GpioMode: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Checked: Boolean read FChecked write FChecked;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property GpioMode: Integer read FGpioMode write SetGpioMode;
    property GpioPud: Integer read FGpioPud write SetGpioPud;
    property GpioState: Integer read FGpioState write SetGpioState;
  end;


  // Helper functions
  function TextForPinCombo(Phys: Integer): String;
  function HintForPinCombo(Phys: Integer): String;


implementation

uses
  GpioDefs;


// -----------------------------------------------
// OnPaint - We decide the content entirely ourselves
// -----------------------------------------------
procedure TGpioCheckBox.GpioPinPaint(Sender: TObject);
var
  RectSize: Integer;
  Rx,Ry,Tx,Ty:Integer;

begin
  RectSize:= Height - 4;
  Rx:= 1;            Ry:= 1;
  Tx:= RectSize + 4; Ty:= 1;

  // If it is the left side, text must be before the square
  if BiDiMode = bdRightToLeft then
  begin
    Rx:= Width - RectSize - 1;
    Tx:= Width - RectSize - Canvas.TextWidth(Caption) - 4;
  end;

  // Paint entire background
  Canvas.Brush.Color:= clDefault;
  Canvas.FillRect(0, 0, width, height);

  // Draw text with default background
  Canvas.Brush.Color:= clDefault;
  Canvas.TextOut(Tx, Ty, Caption);

  // Paint square with a border
  if Enabled then
  begin
    case GpioMode of
      FSEL_INPUT:  Canvas.Brush.Color:= clLightGreen;
      FSEL_OUTPUT: Canvas.Brush.Color:= clLightBlue;
      2..7:        Canvas.Brush.Color:= clLightYellow; // Alt 0-5
      else         Canvas.Brush.Color:= clLightRed;
    end;
  end
  else Canvas.Brush.Color:= clDefault;

  Canvas.Pen.Color:= clBlack;
  Canvas.Rectangle(Rx, Ry, Rx + RectSize, Ry + RectSize);

  // If Checked, a smaller black square is drawn inside the large one
  if Checked then
  begin
    Canvas.Brush.Color:= clBlack;
    Canvas.Rectangle(Rx+4, Ry+4, Rx+RectSize-4, Ry+RectSize-4);
  end;

  // Canvas.FillRect(1,1,20,20);
  // Canvas.FrameRect(1,1,20,20);
  // Use Rectangle to draw a filled rectangle with borders.
  // Use FloodFill to fill a rectangular area bounded by a specified color value.
end;



// -----------------------------------------------
// SetGpioMode
// -----------------------------------------------
procedure TGpioCheckBox.SetGpioMode(GpioMode: Integer);
begin
  FGpioMode:= GpioMode;
  Repaint;
end;


// -----------------------------------------------
// SetGpioPud
// -----------------------------------------------
procedure TGpioCheckBox.SetGpioPud(GpioMode: Integer);
begin
  FGpioPud:= GpioPud;
  Repaint;
end;


// -----------------------------------------------
// SetGpioState
// -----------------------------------------------
procedure TGpioCheckBox.SetGpioState(GpioMode: Integer);
begin
  FGpioState:= GpioState;
  Repaint;
end;



// -----------------------------------------------
// OnClick event
// -----------------------------------------------
procedure TGpioCheckBox.GpioPinClick(Sender: TObject);
begin
  Checked:= Not Checked;
  Repaint;
  if assigned(FOnChange) then FOnChange(Sender);
end;



// -----------------------------------------------
// Create
// -----------------------------------------------
constructor TGpioCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height:= 22;
  Width:= 144;
  BevelInner:= bvNone;
  BevelOuter:= bvNone;

  OnClick:= @GpioPinClick;
  OnPaint:= @GpioPinPaint;
end;


// -----------------------------------------------
// Destroy
// -----------------------------------------------
destructor TGpioCheckBox.Destroy;
begin
  inherited Destroy;
end;


// ---------------------------------------------------------
//
// Helper functions
//
// ---------------------------------------------------------

// -------------------------------------------------------------
// Return text for Pin ComboBox
// Phys: Header Pin which corresponds to the ComboBox number
// -------------------------------------------------------------
function TextForPinCombo(Phys: Integer): String;
var
  S: String = '';

begin
  case Phys of
    1,17:          S:= '3V3 [X]';
    2,4:           S:= '[X] 5V';
    6,14,20,30,34: S:= '[X] GND';
    9,25,39:       S:= 'GND [X]';
    else
    begin
      S:= 'GPIO ' + IntToStr(ConvertPinFromTo(RPI_PIN_PHYS, RPI_PIN_GPIO, Phys));

      if Odd(Phys)
        then S:= S + ' [U]'
        else S:= '[U] ' + S;
    end;
  end;

  Result:= S;
end;



// -------------------------------------------------------------
// Return Hint for Pin ComboBox
// Phys: Header Pin which corresponds to the ComboBox number
// -------------------------------------------------------------
function HintForPinCombo(Phys: Integer): String;
var
  S: String = '';

begin
  case Phys of
    1,17:                  S:= '3,3 Volt';
    2,4:                   S:= '5 Volt';
    6,9,14,20,25,30,34,39: S:= 'Ground';
    else
    begin
      S:= 'GPIO ' + IntToStr(ConvertPinFromTo(RPI_PIN_PHYS, RPI_PIN_GPIO, Phys));
    end;
  end;

  // Apply hardware pin number
  Result:= 'Header Pin ' + IntToStr(Phys) + ', ' + S;
end;




end.

