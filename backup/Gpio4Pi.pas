unit Gpio4Pi;

// ------------------------------------------------------------------------
//
// GPIO for Raspberry Pi 1 to 4
// ----------------------------
//
// Can be used on:
// - Pi 1 (BCM2835) - Tested with Pi 1.2
// - Pi 2 (BCM2836) - I don't have a Pi 2
// - Pi 3 (BCM2837) - Tested with Pi 3B+
// - Pi 4 (BCM2711) - Tested with Pi 4B
// - Pi 5 (BCM2712) - NO SUPPORT YET
//
// This unit / object uses ONLY GPIO pin numbers which directly refer
// to BCM GPIO numbers.
//
// This unit/object is inspired by WiringPi and parts of the code and definitions
// have been converted to Pascal.
//
// There is support for GPIO Clock, where the frequency for the GPIO pin is specified.
// The divisor is calculated automatically for Pi1-3 and Pi4,
// which run with different clock frequencies.
//
// There is also support for PWM, where the PWM channel is selected automatically
// based on the pin number.
// Both PWM channels can therefore be used with different frequencies and Pulse/Pause ratios.
//
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// ------------------------------------------------------------------------

{$Mode objfpc}{$H+}
{$inline on}

interface
 
uses
  Classes, SysUtils, GpioDefs;


type
  IntArray = Array of Integer;

  // One GPIO returned by GetGpioPinData()
  TGpioPin = Record
    Mode:  Byte;     // Input, Output, Alt0 ... Alt5
    Pull:  Byte;     // Pull Up/Down. ONLY on Pi4 (BCM2711)
    Level: Byte;     // 0 or 1
  end;

  // One Clock
  TClock = Record
    Control: LongWord;
    Divisor: LongWord;
  end;

  // One PWM Channel
  TPwmChannel = Record
    Range: LongWord;
    Data:  LongWord;
  end;

  // One PWM Block
  TPwmData = Record
    Control:  LongWord;
    Status:   LongWord;
    DMA:      LongWord;
    FIFO:     LongWord;
    Channels: Array [0..1] of TPwmChannel;
  end;

  // Raspberry PI info
  TRPiModelInfo = record
    Rev:   Integer;    // Revision number. 0,1,2, etc.
    Model: Integer;    // PI_MODEL_A, PI_MODEL_B, etc.
    Cpu:   Integer;    // PI_CPU_BCM2835, PI_CPU_BCM2836, etc.
    Maker: Integer;    // PI_MAKER_SONY, PI_MAKER_EGOMAN, etc.
    Mem:   Integer;    // PI_MEM_256, PI_MEM_512, etc.
  end;


  // GPIO Class
  TPiGpio = class(TObject)
  private
    FRPiModel:     TRPiModelInfo;
    FGpioBaseMem:  LongWord;
    FUsingGpioMem: Boolean;
    FLastErrorStr: String;

    function GetPwmBasePtr(Gpin: Byte; Ofs: Word): Pointer;
    function LoadRasPiData: Boolean;
    function GetGpioHighestPin: Byte;
    function IsGpioPinOk(Gpin: Byte): Boolean;
    function IsCpuOk: Boolean;
    function UsingGpioMemCheck: Boolean;
    function SetBaseClock(ClockOfs,DivOfs: Word; Freq: Integer): Boolean;

  protected
    PGpioMem: ^LongWord;     // Mapped GPIO mem.  Both in /dev/mem and /dev/gpiomem
    PClkMem:  ^LongWord;     // Mapped Clock mem. Only in /dev/mem
    PPwmMem:  ^LongWord;     // Mapped PWM mem.   Only in /dev/mem

  public
    // ----------------------------------------
    // Create:
    // Must be called once to create the TPiGpio object
    // ----------------------------------------
    constructor Create;

    // ----------------------------------------
    // Destroy:
    // Destroy the TPiGpio object when the program closes
    // ----------------------------------------
    destructor Destroy; override;

    // ----------------------------------------
    // SetPinMode:
    // Sets the selected GPIO pin to the desired Mode
    // Gpin: GPIO pin number, 0-53/0-57 depending on CPU
    // Mode: PM_INPUT, PM_OUTPUT, PM_ALT0...PM_ALT5,
    //       PM_PWMOUT_MS, PM_PWMOUT_BAL, PM_GPIO_CLOCK
    // Return: True on success, False on error
    // ----------------------------------------
    function SetPinMode(Gpin, Mode: Byte): Boolean;

    // ----------------------------------------
    // SetPullMode:
    // Sets the selected GPIO pin to the desired Pull-Down/Up/None
    // Gpin: GPIO pin number, 0-53/0-57 depending on CPU
    // Mode: PUD_OFF, PUD_DOWN, PUD_UP
    // Return: True on success, False on error
    // ----------------------------------------
    function SetPullMode(Gpin, Mode: Byte): Boolean;

    // ----------------------------------------
    // GpioWrite:
    // Sets the selected GPIO pin to low or high
    // Gpin: GPIO pin number, 0-53/0-57 depending on CPU
    // Value: PIN_LOW, PIN_HIGH, 0, 1. (Only bit 0 are used)
    // Only makes sense if the pin is set to Output mode
    // Return: True on success, False on error
    // ----------------------------------------
    function GpioWrite(Gpin, Value: Byte): Boolean;

    // ----------------------------------------
    // GpioRead:
    // Returns the Low/High state of selected GPIO pin
    // Can read both Input and Output pins
    // Gpin: GPIO pin number, 0-53/0-57 depending on CPU
    // Return: PIN_LOW,PIN_HIGH,0,1. $FF on Error
    // ----------------------------------------
    function GpioRead(Gpin: Byte): Byte;

    // ----------------------------------------
    // SetGpioClock:
    // Output the given frequency on the selected pin
    // Gpin: GPIO pin number, can be one of the following:
    //       4,5,6,20,21,32,34,42,43,44
    // Freq: The frequency in Hz. Can be in the range for
    //       Pi 1-3:  4.688 Hz to 250 MHz
    //       Pi 4:   13.186 Hz to 375 MHz
    // The BCM2711 manual says:
    // "The maximum operating frequency of the General Purpose clocks is ~125MHz at 1.2V but
    //  this will be reduced if the GPIO pins are heavily loaded or have a capacitive load"
    // If the frequency is set outside these ranges,
    // it will not be set and false will be returned.
    // If set to 0 the Clock is turned off.
    // Return: True on success, False on error
    // ----------------------------------------
    function SetGpioClock(Gpin: Byte; Freq: Integer): Boolean;

    // ----------------------------------------
    // SetPwmMasterClock:
    // Sets the given frequency on the PWM master clock generator
    // The PWM clock is a common clock for all PWM channels
    // Freq: The frequency in Hz. Can be in the range for
    //       Pi 1-3:  4.688 Hz to 250 MHz
    //       Pi 4:   13.186 Hz to 375 MHz
    // If the frequency is set outside these ranges,
    // it will not be set and false will be returned.
    // If set to 0 the Clock is turned off.
    // Return: True on success, False on error
    // ----------------------------------------
    function SetPwmMasterClock(Freq: Integer): Boolean;

    // ----------------------------------------
    // SetPwmRange:
    // In Mark/Space mode it behaves as a divider for the PWM master clock,
    // so the PWM frequency = PwmMasterClock / Range
    // Gpin: GPIO pin number, can be one of the following:
    //       12,13,18,19,40,41,45
    // Range: 2 - 4294967295 ($FFFFFFFF)
    // Return: True on success, False on error
    // ----------------------------------------
    function SetPwmRange(Gpin: Byte; Range: LongWord): Boolean;

    // ----------------------------------------
    // SetPwmValue:
    // In Mark/Space mode, this Value determines how long time the GPIO pin are high
    // When it is set to 0 the pin is constantly low and
    // when it has the same value as Range the pin is constantly high
    // Anything between 1 and Range-1 determines the pulse width
    // If it is set to a quarter of Range, the pulse width is 25%
    // Half of the Range, the pulse width is 50%, etc.
    // Gpin: GPIO pin number, can be one of the following:
    //       12,13,18,19,40,41,45
    // Value: 0 to PWM Range
    // Return: True on success, False on error
    // ----------------------------------------
    function SetPwmValue(Gpin: Byte; Value: LongWord): Boolean;

    // ----------------------------------------
    // SetPwmDutyCycle:
    // Just a helper function whitch calculate the PWM value
    // and calls SetPwmValue()
    // Gpin: GPIO pin number, can be one of the following:
    //       12,13,18,19,40,41,45
    // DutyCycle: 0 to 100%
    // Return: True on success, False on error
    // ----------------------------------------
    function SetPwmDutyCycle(Gpin: Byte; DutyCycle: Byte): Boolean;

    // ----------------------------------------
    // SetPwmMode:
    // Sets the selected GPIO pin to the desired PWM Mode
    // There are 2 PWM channels available and the right channel is automatically
    // selected based on the pin number.
    // On the BCM2711 Cpu (Pi 4) there are 2*2 PWM channels
    // Be aware that different pins shares the same channel,
    // look further down in the source code
    // Gpin: GPIO pin number, can be one of the following:
    //       12,13,18,19,40,41,45
    // Mode: PWM_MODE_BAL, PWM_MODE_MS  (use PWM_MODE_MS)
    // Return: True on success, False on error
    // ----------------------------------------
    function SetPwmMode(Gpin: Byte; Mode: Byte): Boolean;

    // ----------------------------------------
    // GetGpioPinData:
    // Get the Data for a GPIO pin no matter who has set the pin
    // Gpin: GPIO pin number, 0-53/0-57 depending on CPU
    // Data: Data returned to the caller:
    //       .Mode: PM_INPUT, PM_OUTPUT, PM_ALT0..PM_ALT5
    //             IT DOES NOT RETURN: PM_PWMOUT_MS,PM_PWMOUT_BAL,PM_GPIO_CLOCK
    //       .Pull: PUD_OFF, PUD_DOWN, PUD_UP   (ONLY Pi4 - BCM2711)
    //       .Level: The level of the pin, 0 or 1 (PIN_LOW, PIN_HIGH)
    // Return: True on success, False on error
    // ----------------------------------------
    function GetGpioPinData(Gpin: Integer; var Data: TGpioPin): Boolean;

    // -------------------------------------------------------------
    // FindGpiosForGpioClock:
    // Find all the GPIOs that are assigned to a GpioClock
    // GpioClk: CLK_GPIO_0, CLK_GPIO_1, CLK_GPIO_2
    // Return: Array of GPIO's. Empty = No GPIOs assigned
    // -------------------------------------------------------------
    function GetGpiosForGpioClock(GpioClk: Integer): IntArray;

    // -------------------------------------------------------------
    // FindGpiosForPwm:
    // Find all the GPIOs that are assigned to a PWM channel
    // PwmChan: PWM_CHANNEL_0_0, PWM_CHANNEL_0_1
    //          PWM_CHANNEL_1_0, PWM_CHANNEL_1_1   (Only BCM2711)
    // Return: Array of GPIO's. Empty = No GPIOs assigned
    // -------------------------------------------------------------
    function GetGpiosForPwm(PwmChan: Integer): IntArray;

    // -----------------------------------------------
    // GetRawClockData:
    // Returns the Raw Control and Divisor for a clock
    // ClkNo: CLK_GPIO0, CLK_GPIO1, CLK_GPIO2, CLK_PCM, CLK_PWM
    // Data:  Data returned to the caller:
    //        .Control: Control word from the clock
    //        .Divisor: Divisor and Fraction from the clock
    // Consult the BCM manual for description of Control and Divisor words
    // Return: True on success, False on error
    // -----------------------------------------------
    function GetRawClockData(ClkNo: Integer; var Data: TClock): Boolean;

    // -----------------------------------------------
    // GetClockFrequency:
    // Calculate Frequency of a clock
    // ClkNo: CLK_GPIO0, CLK_GPIO1, CLK_GPIO2, CLK_PCM, CLK_PWM
    // Return: The frequency in Hz
    // -----------------------------------------------
    function GetClockFrequency(ClkNo: Integer): LongWord;

    // -----------------------------------------------
    // GetRawPwmData:
    // Returns the Raw data for a PWM group with 2 channels
    // PwmBlock: PWM_GROUP_0 or PWM_GROUP_1 (Group 1 only on Pi4 (BCM2711))
    // Data: Data returned to the caller:
    //       .Control:  PWM Control word
    //       .Status:   PWM Status word
    //       .DMA:      PWM DMA Configuration
    //       .FIFO:     PWM FIFO Input
    //       .Channels: PWM Channel 1 and Channel 2 (Array[0..1])
    //         [x].Range: PWM Channel 1/2 Range
    //         [x].Data:  PWM Channel 1/2 Data
    // Consult the BCM manual for description of a PWM block
    // Return: True on success, False on error
    // -----------------------------------------------
    function GetRawPwmData(PwmGroup: Integer; var Data: TPwmData): Boolean;

    // ----------------------------------------
    // Return Info about the RaspBerry Pi
    // ----------------------------------------
    property RPiModelInfo: TRPiModelInfo Read FRPiModel;

    // ----------------------------------------
    // Are we using global memory or just GPIO memory
    // If using GPIO memory only basic input/output works
    // ----------------------------------------
    property UsingGpioMem: Boolean Read UsingGpioMemCheck;

    // ----------------------------------------
    // Returns the higest GPIO pin depending on CPU
    // ----------------------------------------
    property GpioHighestPin: Byte Read GetGpioHighestPin;

    // ----------------------------------------
    // If one of the above functions return False, you can read
    // this property to see what the error was....
    // ----------------------------------------
    property LastErrorStr: String Read FLastErrorStr;
  end;


   
procedure DelayMicroSeconds(HowLong: LongWord);


implementation

Uses
  baseUnix, Unix;



procedure DelayMicroSeconds(HowLong: LongWord);
var
  sleeper: TimeSpec;

begin
  // From wiringPi.c:
  // "This is somewhat intersting. It seems that on the Pi, a single call
  //  to nanosleep takes some 80 to 130 microseconds anyway."
  // I have measured on an Pi 4 and this extra time seems to be 70us so we subtract 70.
  // Short times below 100uS are very uncertain and the minimum time will be 70uS.
  if HowLong > 70
    then HowLong:= HowLong - 70
    else HowLong:= 1;

  sleeper.tv_sec:=  HowLong div 1000000;
  sleeper.tv_nsec:= (HowLong mod 1000000) * 1000;
  fpnanosleep (@sleeper, nil);
end;

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

constructor TPiGpio.Create;
var
  Fd: Integer;
  memGpio,memClk,memPwm: LongWord;

begin
  inherited Create;

  // Load Raspberry Data
  If not LoadRasPiData then
  begin
    FreeAndNil(Self);
    Exit;
  end;

  // Load base memory
  Case FRPiModel.Cpu of
    PI_CPU_BCM2835: FGpioBaseMem:= GPIO_PERI_BASE_2835;
    PI_CPU_BCM2836,
    PI_CPU_BCM2837: FGpioBaseMem:= GPIO_PERI_BASE_2836;
    PI_CPU_BCM2711: FGpioBaseMem:= GPIO_PERI_BASE_2711;
//  PI_CPU_BCM2712: FGpioBaseMem:= GPIO_PERI_BASE_2712;  NO SUPPORT YET
    Else
    begin                   // Unknown CPU
      FreeAndNil(Self);
      Exit;
    end;
  end;

  // Open the master /dev/ memory control device
  // Try /dev/mem. If that fails, then
  // try /dev/gpiomem. If that fails then game over.
  Fd:= fpOpen('/dev/mem', O_RdWr or O_Sync); // Try to open the master /dev/mem device
  if Fd < 0 then
  begin
    Fd:= fpOpen('/dev/gpiomem', O_RdWr or O_Sync); // Open the /dev/gpiomem
    if Fd >= 0 then
    begin
      FGpioBaseMem:= 0;
      FUsingGpioMem:= True;
    end
    else
    begin                    // Unable to open any mem blocks, game over
      FreeAndNil(Self);
      Exit;
    end;
  end;

  // Set the offsets into the memory interface.
  memGpio:= FGpioBaseMem + GPIO_BASE;
  memClk:=  FGpioBaseMem + CLOCK_BASE;
  memPwm:=  FGpioBaseMem + GPIO_PWM;

  PGpioMem:= FpMmap(Nil, PAGE_SIZE, PROT_READ or PROT_WRITE, MAP_SHARED, Fd, memGpio);
  PClkMem:=  FpMmap(Nil, PAGE_SIZE, PROT_READ or PROT_WRITE, MAP_SHARED, Fd, memClk);
  PPwmMem:=  FpMmap(Nil, PAGE_SIZE, PROT_READ or PROT_WRITE, MAP_SHARED, Fd, memPwm);

  // Check for mapping ok
  if (PGpioMem = MAP_FAILED) or (PClkMem = MAP_FAILED) or (PPwmMem = MAP_FAILED) then
  begin
    Self.Destroy;
    FreeAndNil(Self);
    Exit;
  end;
end;


// ------------------------------------------------------------------------


destructor TPiGpio.Destroy;
begin
  if PGpioMem <> nil then
 begin
   fpMUnmap(PGpioMem, PAGE_SIZE);
   PGpioMem:= Nil;
 end;

 if PClkMem <> nil then
 begin
   fpMUnmap(PClkMem, PAGE_SIZE);
   PClkMem:= Nil;
 end;

 if PPwmMem <> nil then
 begin
   fpMUnmap(PPwmMem, PAGE_SIZE);
   PPwmMem:= Nil;
 end;

  inherited Destroy;
end;


// ------------------------------------------------------------------------


// Fill out Info about the Raspberry Pi
function TPiGpio.LoadRasPiData: Boolean;
var
  Fd,Cnt,I,J: Integer;
  Rev: LongWord;
  Buf: Array [0..2048] of char;
  S: String;

begin
  FillChar(Buf{%H-}, SizeOf(Buf), 0);
  Cnt:= 0;
  Rev:= 0;

  // First try device-tree
  Fd:= fpOpen('/proc/device-tree/system/linux,revision', O_RdOnly);
  if Fd >= 0 then
  begin
    // Read binary file direct into Rev variable
    Cnt:= fpRead(Fd, Rev, SizeOf(Rev));
    FpClose(Fd);
    if Cnt <> SizeOf(Rev) then Rev:= 0;
    Rev:= SwapEndian(Rev);
  end;

  // If device-tree failed, then try /proc/cpuinfo
  if Rev = 0 then
  begin
    Fd:= fpOpen('/proc/cpuinfo', O_RdOnly);
    if Fd >= 0 then
    begin
      // Read text file into temp buffer
      Cnt:= fpRead(Fd, Buf, SizeOf(Buf));
      FpClose(Fd);

      // Find and convert Revision
      I:= Pos('Revision', Buf, 1);           // Find 'Revision'
      I:= Pos(':', Buf, I);                  // Advance to ':'
      J:= Pos(#$0A, Buf, I);                 // Find LF (EOL)
      S:= TrimLeft(Copy(Buf, I+1, J-I-1));   // Get Rev. number
      Rev:= StrToIntDef('$' + S, 0);
    end;
  end;

  if Rev = 0 then Exit(False);

  // Check for New / Old style
  //
  // 5 4  3  2 1 0  9 8 7 6  5 4 3 2  1 0 9 8 7 6 5 4  3 2 1 0
  // W W  S  M M M  B B B B  P P P P  T T T T T T T T  R R R R
  //
  // W  warranty void if either bit is set
  // S  0=old (bits 0-22 are revision number) 1=new (following fields apply)
  // M  0=256 1=512 2=1024 3=2GB 4=4GB
  // B  0=Sony 1=Egoman 2=Embest 3=Sony Japan 4=Embest 5=Stadium
  // P  0=2835, 1=2836, 2=2837 3=2711
  // T  0=A 1=B 2=A+ 3=B+ 4=Pi2B 5=Alpha 6=CM1 8=Pi3B 9=Zero etc.
  // R  PCB board revision
  //
  if (Rev and (1 shl 23)) <> 0 then
  begin
    // New style
    FRPiModel.Rev:=   (Rev and $0000000F);
    FRPiModel.Model:= (Rev and $00000FF0) shr 4;
    FRPiModel.Cpu:=   (Rev and $0000F000) shr 12;
    FRPiModel.Maker:= (Rev and $000F0000) shr 16;
    FRPiModel.Mem:=   (Rev and $00700000) shr 20;
  end
  else
  begin
    // Old way, Raspberry Pi 1B/A, CM (from wiringpi)
    Rev:= Rev and $FF;
    FillChar(FRPiModel, SizeOf(FRPiModel), 0);
    FRPiModel.Cpu:= PI_CPU_BCM2835;

    case Rev of
      $02: begin FRPiModel.Rev:=   PI_VERSION_1;    FRPiModel.Model:= PI_MODEL_B;
                 FRPiModel.Maker:= PI_MAKER_EGOMAN; FRPiModel.Mem:= 0; end;

      $03: begin FRPiModel.Rev:=   PI_VERSION_1_1;  FRPiModel.Model:= PI_MODEL_B;
                 FRPiModel.Maker:= PI_MAKER_EGOMAN; FRPiModel.Mem:= 0; end;
      // ------------------------------------------------------------------------
      $04: begin FRPiModel.Rev:=   PI_VERSION_1_2;  FRPiModel.Model:= PI_MODEL_B;
                 FRPiModel.Maker:= PI_MAKER_SONY;   FRPiModel.Mem:= 0; end;
      $05,
      $06: begin FRPiModel.Rev:=   PI_VERSION_1_2;  FRPiModel.Model:= PI_MODEL_B;
                 FRPiModel.Maker:= PI_MAKER_EGOMAN; FRPiModel.Mem:= 0; end;
      // ------------------------------------------------------------------------
      $07,
      $09: begin FRPiModel.Rev:=   PI_VERSION_1_2;  FRPiModel.Model:= PI_MODEL_A;
                 FRPiModel.Maker:= PI_MAKER_EGOMAN; FRPiModel.Mem:= 0; end;

      $08: begin FRPiModel.Rev:=   PI_VERSION_1_2;  FRPiModel.Model:= PI_MODEL_A;
                 FRPiModel.Maker:= PI_MAKER_SONY;   FRPiModel.Mem:= 0; end;
      // ------------------------------------------------------------------------
      $0D,
      $0F: begin FRPiModel.Rev:=   PI_VERSION_1_2;  FRPiModel.Model:= PI_MODEL_B;
                 FRPiModel.Maker:= PI_MAKER_EGOMAN; FRPiModel.Mem:= 1; end;

      $0E: begin FRPiModel.Rev:=   PI_VERSION_1_2;  FRPiModel.Model:= PI_MODEL_B;
                 FRPiModel.Maker:= PI_MAKER_SONY;   FRPiModel.Mem:= 1; end;
      // ------------------------------------------------------------------------
      $10,
      $16: begin FRPiModel.Rev:=   PI_VERSION_1_2;  FRPiModel.Model:= PI_MODEL_BP;
                 FRPiModel.Maker:= PI_MAKER_SONY;   FRPiModel.Mem:= 1; end;
      $13,
      $19: begin FRPiModel.Rev:=   PI_VERSION_1_2;  FRPiModel.Model:= PI_MODEL_BP;
                 FRPiModel.Maker:= PI_MAKER_EGOMAN; FRPiModel.Mem:= 1; end;
      // ------------------------------------------------------------------------
      $11,
      $17: begin FRPiModel.Rev:=   PI_VERSION_1_1;  FRPiModel.Model:= PI_MODEL_CM1;
                 FRPiModel.Maker:= PI_MAKER_SONY;   FRPiModel.Mem:= 1; end;
      $14,
      $1A: begin FRPiModel.Rev:=   PI_VERSION_1_1;  FRPiModel.Model:= PI_MODEL_CM1;
                 FRPiModel.Maker:= PI_MAKER_EGOMAN; FRPiModel.Mem:= 1; end;
      // ------------------------------------------------------------------------
      $12,
      $18: begin FRPiModel.Rev:=   PI_VERSION_1_1;  FRPiModel.Model:= PI_MODEL_AP;
                 FRPiModel.Maker:= PI_MAKER_SONY;   FRPiModel.Mem:= 0; end;

      $15: begin FRPiModel.Rev:=   PI_VERSION_1_1;  FRPiModel.Model:= PI_MODEL_AP;
                 FRPiModel.Maker:= PI_MAKER_EMBEST; FRPiModel.Mem:= 1; end;

      $1B: begin FRPiModel.Rev:=   PI_VERSION_1_1;  FRPiModel.Model:= PI_MODEL_AP;
                 FRPiModel.Maker:= PI_MAKER_EGOMAN; FRPiModel.Mem:= 0; end;
    end;
  end;

  Exit(True);
end;



// ------------------------------------------------------------------------


function TPiGpio.UsingGpioMemCheck: Boolean;
begin
  if FUsingGpioMem then
  begin
    FLastErrorStr:= 'Not supported when using /dev/gpiomem, Try sudo';
    Exit(True);
  end
  else Exit(False);
end;


// ------------------------------------------------------------------------


function TPiGpio.IsCpuOk: Boolean;
begin
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    FLastErrorStr:= 'No Support for BCM2712';
    Exit(False);
  end;

  Exit(True);
end;


// ------------------------------------------------------------------------


function TPiGpio.GetGpioHighestPin: Byte;
begin
  Case FRPiModel.Cpu of
    PI_CPU_BCM2835,
    PI_CPU_BCM2836,
    PI_CPU_BCM2837: Result:= 53;
    PI_CPU_BCM2711: Result:= 57;
    PI_CPU_BCM2712: Result:= 0;
    else            Result:= 0;
  end;
end;


// ------------------------------------------------------------------------


function TPiGpio.IsGpioPinOk(Gpin: Byte): Boolean;
var
  MaxPin: Byte;

begin
  if not IsCpuOk then Exit(False);

  MaxPin:= GpioHighestPin;

  if Gpin > MaxPin then
  begin
    FLastErrorStr:= 'Pin numbers in range 0...' + IntToStr(MaxPin);
    Exit(False);
  end
  else Exit(True);
end;


// ------------------------------------------------------------------------

// ----------------------------------------
// Internal function for set pointer to right PWM channel
// ----------------------------------------
function TPiGpio.GetPwmBasePtr(Gpin: Byte; Ofs: Word): Pointer;
var
  ChanTwoOfs: Word;

begin
  if not IsCpuOk then Exit(Nil);
  if UsingGpioMem then Exit(Nil);

  Case FRPiModel.Cpu of
    PI_CPU_BCM2835,
    PI_CPU_BCM2836,
    PI_CPU_BCM2837: ChanTwoOfs:= PWM0_OFFSET;   // Pi 1-3. Only one set of PWM channels
    PI_CPU_BCM2711: ChanTwoOfs:= PWM1_OFFSET;   // Pi 4. Two sets of PWM channels
    PI_CPU_BCM2712:
    begin
      Exit(Nil);
    end;
  end;

  Case Gpin of
    12,13,18,19,45: Exit(SetPtr(PPwmMem, PWM0_OFFSET, Ofs));
    40,41:          Exit(SetPtr(PPwmMem, ChanTwoOfs, Ofs));
    else
    begin
      FLastErrorStr:= 'PWM only on GPIO 12,13,18,19,40,41,45';
      Exit(Nil);
    end;
  end;
end;


// ------------------------------------------------------------------------


function TPiGpio.SetPwmMode(Gpin: Byte; Mode: Byte): Boolean;
var
  pPwm: ^LongWord;
  Mask: LongWord;

begin
  pPwm:= GetPwmBasePtr(Gpin, PWM_CONTROL);   // Get PWM0_x group / PWM1_x group pointer
  if pPwm = Nil then Exit(False);

  Case Gpin of
    12,18,40:
    begin
      Mask:= PWM0_ENABLE or PWM0_SERIAL or PWM0_REPEATFF or PWM0_SILENCE or
             PWM0_REVPOLAR or PWM0_USEFIFO or PWM0_MS_Mode;

      if (Mode = PWM_MODE_MS)
        then pPwm^:= (pPwm^ and (not Mask)) or PWM0_ENABLE or PWM0_MS_MODE
        else pPwm^:= (pPwm^ and (not Mask)) or PWM0_ENABLE;
    end;

    13,19,41,45:
    begin
      Mask:= PWM1_ENABLE or PWM1_SERIAL or PWM1_REPEATFF or PWM1_SILENCE or
             PWM1_REVPOLAR or PWM1_USEFIFO or PWM1_MS_Mode;

      if (Mode = PWM_MODE_MS)
        then pPwm^:= (pPwm^ and (not Mask)) or PWM1_ENABLE or PWM1_MS_MODE
        else pPwm^:= (pPwm^ and (not Mask)) or PWM1_ENABLE;
    end;

    else Exit(False);
  end;

  Exit(True);
end;


// ------------------------------------------------------------------------


function TPiGpio.SetPwmRange(Gpin: Byte; Range: LongWord): Boolean;
var
  pPwm: ^LongWord;
  PwmReg: Word;

begin
  Case Gpin of
    12,18,40:    PwmReg:= PWM0_RANGE;
    13,19,41,45: PwmReg:= PWM1_RANGE;
    else         PwmReg:= 0;
  end;

  pPwm:= GetPwmBasePtr(Gpin, PwmReg);
  if pPwm = Nil then Exit(False);

  pPwm^:= Range;

  Exit(True);
end;


// ------------------------------------------------------------------------


function TPiGpio.SetPwmValue(Gpin: Byte; Value: Longword): Boolean;
var
  pPwm: ^LongWord;
  PwmReg: Word;

begin
  Case Gpin of
    12,18,40:    PwmReg:= PWM0_DATA;
    13,19,41,45: PwmReg:= PWM1_DATA;
    else         PwmReg:= 0;
  end;

  pPwm:= GetPwmBasePtr(Gpin, PwmReg);
  if pPwm = Nil then Exit(False);

  pPwm^:= Value;

  Exit(True);
end;


// ------------------------------------------------------------------------


function TPiGpio.SetPwmDutyCycle(Gpin: Byte; DutyCycle: Byte): Boolean;
var
  pPwm: ^LongWord;
  PwmReg: Word;
  Value: LongWord;

begin
  Case Gpin of
    12,18,40:    PwmReg:= PWM0_RANGE;
    13,19,41,45: PwmReg:= PWM1_RANGE;
    else         PwmReg:= 0;
  end;

  pPwm:= GetPwmBasePtr(Gpin, PwmReg);
  if pPwm = Nil then Exit(False);
  if pPwm^ = 0 then Exit(False);

  Value:= (pPwm^ div 100) * DutyCycle;
  Result:= SetPwmValue(Gpin, Value);
end;


// ------------------------------------------------------------------------


function TPiGpio.SetBaseClock(ClockOfs,DivOfs: Word; Freq: Integer): Boolean;
var
  PiFreq,DivI,DivF: Integer;
  ClkSrc: LongWord;
  pClk: ^LongWord;

begin
  if not IsCpuOk then Exit(False);
  if UsingGpioMem then Exit(False);
  Result:= True;

  // If Freq = 0, then stop the Clock
  if Freq = 0 then
  begin
    pClk:= SetPtr(PClkMem, ClockOfs);
    pClk^:= (pClk^ and $0000000F) or BCM_PASSWORD;   // Stop Clock (b4=0)

    while (pClk^ and $00000080) <> 0 do              // Wait for clock to be not BUSY
      DelayMicroSeconds(10);

    Exit;
  end;

  // Calculate Freq. Pi1-3 and Pi4 are different
  Case FRPiModel.Cpu of
    PI_CPU_BCM2835,
    PI_CPU_BCM2836,
    PI_CPU_BCM2837: PiFreq:= CLK_OSC_FREQ;       // Pi 1-3.  4.688 Hz to 9.6 MHz
    PI_CPU_BCM2711: PiFreq:= CLK_OSC_FREQ_2711;  // Pi 4.   13.186 Hz to 27 MHz
    PI_CPU_BCM2712:
    begin
      Exit(False);
    end;
  end;

  // If we are going fast, use PLLD source clock to avoid jitter on the clock
  ClkSrc:= CLK_SRC_OSC;

  if Freq > (PiFreq div 10) then
  begin
    ClkSrc:= CLK_SRC_PLLD;
    if FRPiModel.Cpu = PI_CPU_BCM2711
      then PiFreq:= CLK_PLLD_FREQ_2711         // Pi 4.   183.150 Hz to 375 MHz
      else PiFreq:= CLK_PLLD_FREQ;             // Pi 1-3. 122.100 Hz to 250 MHz
  end;

  // Calculate Divisor and Fraction
  // From the manual: Freq:= Source / (DIVI + DIVF / 1024) in MASH-1 mode
  DivI:= PiFreq div Freq;
  DivF:= Trunc(((PiFreq / Freq) - DivI) * 1024);

  if (DivI < 2) then
  begin
    FLastErrorStr:= 'Requested frequency too high, max. freq = ' + IntToStr(PiFreq div 2);
    Exit(False);
  end;

  if (DivI > $0FFF) then
  begin
    FLastErrorStr:= 'Requested frequency too low, lowest freq = ' + IntToStr(PiFreq div $FFF);
    Exit(False);
  end;

  if (DivF > $0FFF) then
  begin                   // we should never come here!
    DivF:= $0FFF;
    FLastErrorStr:= 'Clock Fraction Overflow';
    Result:= False;
  end;

  pClk:= SetPtr(PClkMem, ClockOfs);
  pClk^:= (pClk^ and $0000000F) or BCM_PASSWORD;   // Stop Clock  (b4 = 0)

  while (pClk^ and $00000080) <> 0 do              // Wait for clock to be not BUSY
    DelayMicroSeconds(10);

  pClk:= SetPtr(PClkMem, DivOfs);
  pClk^:= BCM_PASSWORD or (DivI shl 12) or DivF;   // Set Divisor and Fraction

  pClk:= SetPtr(PClkMem, ClockOfs);
  pClk^:= BCM_PASSWORD or CLK_CTL_MASH1 or ClkSrc; // Set Source and MASH1

  while (pClk^ and $00000080) <> 0 do              // Wait for clock to be not BUSY
    DelayMicroSeconds(10);

  pClk^:= pClk^ or BCM_PASSWORD or (1 shl 4);      // Start Clock  (b4 = 1)
end;


// ------------------------------------------------------------------------


function TPiGpio.SetPwmMasterClock(Freq: Integer): Boolean;
var
  PwmCont: LongWord;
  pPwm: ^LongWord;

begin
  if not IsCpuOk then Exit(False);
  if UsingGpioMem then Exit(False);

  pPwm:= SetPtr(PPwmMem, PWM_CONTROL);
  PwmCont:= pPwm^;                  // preserve PWM_CONTROL
  pPwm^:= 0;                        // Stop PWM

  // Set PWM Clock
  Result:= SetBaseClock(CLK_PWM_CTL, CLK_PWM_DIV, Freq);

  // restore PWM_CONTROL
  pPwm^:= PwmCont;
end;


// ------------------------------------------------------------------------


function TPiGpio.SetGpioClock(Gpin: Byte; Freq: Integer): Boolean;
var
  OfsClk, OfsDiv: Word;

begin
  if not IsCpuOk then Exit(False);
  if UsingGpioMem then Exit(False);

  // Load Clock group
  Case Gpin of
    4,20,32,34: begin OfsClk:= CLK_GP0_CTL; OfsDiv:= CLK_GP0_DIV; end;
    5,21,42,44: begin OfsClk:= CLK_GP1_CTL; OfsDiv:= CLK_GP1_DIV; end;
    6,43:       begin OfsClk:= CLK_GP2_CTL; OfsDiv:= CLK_GP2_DIV; end;
    else
    begin
      FLastErrorStr:= 'GPIO Clock only on GPIO 4,5,6,20,21,32,34,42,43,44';
      Exit(False);
    end;
  end;

  Result:= SetBaseClock(OfsClk, OfsDiv, Freq);
end;


// ------------------------------------------------------------------------


function TPiGpio.SetPinMode(Gpin, Mode: Byte): Boolean;
var
  fSel, Shift, Alt: Byte;
  pGpio: ^LongWord;

begin
  if not IsGpioPinOk(Gpin) then Exit(False);
  Result:= True;

  fSel:=  (Gpin div 10) * 4;    // Select Gpfsel 0 to 5 register. *4 = LongWord
  Shift:= (Gpin mod 10) * 3;    // 0-9 pin shift. *3 = 3 bits
  pGpio:= SetPtr(PGpioMem, fSel);

  case Mode of
    PM_INPUT, PM_OUTPUT,
    PM_ALT0,  PM_ALT1, PM_ALT2,
    PM_ALT3,  PM_ALT4, PM_ALT5:
    begin
      pGpio^:= pGpio^ and ($FFFFFFFF - (7 shl Shift)) or (Mode shl Shift);
    end;

    PM_PWMOUT_MS,
    PM_PWMOUT_BAL:
    begin
      if UsingGpioMem then Exit(False);

      Case Gpin of
        12,13,40,41,45: Alt:= FSEL_ALT0;
        18,19:          Alt:= FSEL_ALT5;
        else
        begin
          FLastErrorStr:= 'PWM Output only on GPIO 12,13,18,19,40,41,45';
          Exit(False);
        end;
      end;

      // Set Alt0 / Alt5 Mode
      pGpio^:= pGpio^ and ($FFFFFFFF - (7 shl Shift)) or (Alt shl Shift);
      DelayMicroSeconds(110);

      // Set PWM Mode
      if Mode = PM_PWMOUT_MS
        then Result:= SetPwmMode(Gpin, PWM_MODE_MS)     // Mark/Space
        else Result:= SetPwmMode(Gpin, PWM_MODE_BAL);   // Balanced

  //    SetPwmRange(Gpin, 200);         // Clock / 200 = 100 Hz
  //    SetPwmValue(Gpin, 100);         // Duty cycle 50%
  //    Result:= SetPwmClock(20000);    // 20KHz - Also starts the PWM
    end;

    PM_GPIO_CLOCK:
    begin
      if UsingGpioMem then Exit(False);

      // Load Alt group
      Case Gpin of
        4,5,6,32,34,42,43,44: Alt:= FSEL_ALT0;
        20,21:                Alt:= FSEL_ALT5;
        else
        begin
          FLastErrorStr:= 'GPIO Clock only on GPIO 4,5,6,20,21,32,34,42,43,44';
          Exit(False);
        end;
      end;

      // Set Alt0 / Alt5 Mode
      pGpio^:= pGpio^ and ($FFFFFFFF - (7 shl Shift)) or (Alt shl Shift);
      DelayMicroSeconds(110);

  //    Result:= SetGpioClock(Gpin, 20000);   // 20 KHz - Also starts the Clock
    end;
  end;
end;


// ------------------------------------------------------------------------


function TPiGpio.SetPullMode(Gpin, Mode: Byte): Boolean;
var
  fSel, Shift, Pud: Byte;
  pPud, pPudClk: ^LongWord;

begin
  if not IsGpioPinOk(Gpin) then Exit(False);

  // NB: PullUp/Down are different for PI1-3 and PI4
  Case FRPiModel.Cpu of
    PI_CPU_BCM2835,
    PI_CPU_BCM2836,
    PI_CPU_BCM2837:
    begin
      case Mode of              // Pud = 0,1,2: Off,Down,Up
        PUD_DOWN: Pud:= %0001;
        PUD_UP:   Pud:= %0010;
        else      Pud:= %0000;
      end;

      pPud:= SetPtr(PGpioMem, GPPUD);
      pPud^:= Pud;
      DelayMicroSeconds(5);

      pPudClk:= SetPtr(PGpioMem, GPPUDCLK0 + ((Gpin shr 5) shl 2));  // (Gpin Div 32) * 4
      pPudClk^:= 1 shl (Gpin and $1F);
      DelayMicroSeconds(5);

      pPud^:= 0;
      DelayMicroSeconds(5);
      pPudClk^:= 0;
      DelayMicroSeconds(5);
    end;

    PI_CPU_BCM2711:
    begin
      case Mode of              // Pud = 0,1,2: Off,Up,Down
        PUD_DOWN: Pud:= %0010;
        PUD_UP:   Pud:= %0001;
        else      Pud:= %0000;
      end;

      fSel:=  (Gpin div 16) * 4;    // Select Gpfsel 0 to 15 register. *4 = LongWord
      Shift:= (Gpin mod 16) * 2;    // 0-9 pin shift. *2 = 2 bits

      pPud:= SetPtr(PGpioMem, GPPUPPDN0 + fSel);
      pPud^:= pPud^ and ($FFFFFFFF - (3 shl Shift)) or (Pud shl Shift);
    end;

    PI_CPU_BCM2712:
    begin
      // No support
      Exit(False);
    end;
  end;

  Exit(True);
end;


// ------------------------------------------------------------------------


function TPiGpio.GpioWrite(Gpin, Value: Byte): Boolean;
var
  pGpio: ^LongWord;

begin
  if not IsGpioPinOk(Gpin) then Exit(False);
  Result:= True;

  if (Value and $01) = 0
    then pGpio:= SetPtr(PGpioMem, GPCLR0 + ((Gpin shr 5) shl 2))   // (Gpin Div 32) * 4
    else pGpio:= SetPtr(PGpioMem, GPSET0 + ((Gpin shr 5) shl 2));

  pGpio^:= 1 shl (Gpin and $1F);
end;


// ------------------------------------------------------------------------


function TPiGpio.GpioRead(Gpin: Byte): Byte;
var
  pGpio: ^LongWord;

begin
  if not IsGpioPinOk(Gpin) then Exit($FF);

  pGpio:= SetPtr(PGpioMem, GPLEV0 + ((Gpin shr 5) shl 2));   // (Gpin Div 32) * 4
  if (pGpio^ and (1 shl (Gpin and $1F))) = 0
    then Result:= PIN_LOW
    else Result:= PIN_HIGH;
end;


// ------------------------------------------------------------------------


function TPiGpio.GetGpioPinData(Gpin: Integer; var Data: TGpioPin): Boolean;
var
  fSel, Shift: Byte;
  pGpio: ^LongWord;

begin
  if not IsGpioPinOk(Gpin) then Exit(False);
  Result:= True;

  // GPIO Mode
  fSel:=  (Gpin div 10) * 4;    // Select Gpfsel 0 to 5 register. *4 = LongWord
  Shift:= (Gpin mod 10) * 3;    // 0-9 pin shift. *3 = 3 bits
  pGpio:= SetPtr(PGpioMem, fSel);

  Data.Mode:= (pGpio^ shr Shift) and 7;

  // Pull Up/Down
  // We can only read PullUp/Down for Pi4 !!!
  if FRPiModel.Cpu = PI_CPU_BCM2711 then
  begin
    fSel:=  (Gpin div 16) * 4;    // Select GpioPupPdn 0 to 3 register. *4 = LongWord
    Shift:= (Gpin mod 16) * 2;    // 0-15 pin shift. *2 = 2 bits

    pGpio:= SetPtr(PGpioMem, GPPUPPDN0, fSel);
    Data.Pull:= (pGpio^ shr Shift) and 3;

    case Data.Pull of
      %0010: Data.Pull:= PUD_DOWN;
      %0001: Data.Pull:= PUD_UP;
      else   Data.Pull:= PUD_OFF;
    end;
  end
  else Data.Pull:= PUD_OFF;

  // State, High / Low
  pGpio:= SetPtr(PGpioMem, GPLEV0 + ((Gpin shr 5) shl 2));   // (Gpin Div 32) * 4
  if (pGpio^ and (1 shl (Gpin and $1F))) = 0
    then Data.Level:= 0
    else Data.Level:= 1;
end;


// ------------------------------------------------------------------------


function TPiGpio.GetGpiosForGpioClock(GpioClk: Integer): IntArray;
var
  Gpio: Integer;
  Ok:   Boolean;
  Data: TGpioPin;

begin
  Result:= [];
  if not IsCpuOk then Exit;

  for Gpio in [4,5,6,20,21,32,34,42,43,44] do
  begin
    Ok:= False;
    GetGpioPinData(Gpio, Data{%H-});

    case GpioClk of
      CLK_GPIO0:
        Ok:= ((Gpio = 4)  and (Data.Mode = FSEL_ALT0)) or
             ((Gpio = 20) and (Data.Mode = FSEL_ALT5)) or
             ((Gpio = 32) and (Data.Mode = FSEL_ALT0)) or
             ((Gpio = 34) and (Data.Mode = FSEL_ALT0));

      CLK_GPIO1:
        Ok:= ((Gpio = 5)  and (Data.Mode = FSEL_ALT0)) or
             ((Gpio = 21) and (Data.Mode = FSEL_ALT5)) or
             ((Gpio = 42) and (Data.Mode = FSEL_ALT0)) or
             ((Gpio = 44) and (Data.Mode = FSEL_ALT0));

      CLK_GPIO2:
        Ok:= ((Gpio = 6)  and (Data.Mode = FSEL_ALT0)) or
             ((Gpio = 43) and (Data.Mode = FSEL_ALT0));
    end;

    If Ok then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result)-1]:= Gpio;
    end;
  end;
end;


// ------------------------------------------------------------------------


function TPiGpio.GetGpiosForPwm(PwmChan: Integer): IntArray;
var
  Gpio: Integer;
  Ok: Boolean;
  Data: TGpioPin;

begin
  Result:= [];
  if not IsCpuOk then Exit;

  for Gpio in [12,13,18,19,40,41,45] do
  begin
    Ok:= False;
    GetGpioPinData(Gpio, Data{%H-});

    if FRPiModel.Cpu = PI_CPU_BCM2711 then
    begin
      case PwmChan of
        PWM_CHANNEL_0_0:
          Ok:= ((Gpio = 12) and (Data.Mode = FSEL_ALT0)) or
               ((Gpio = 18) and (Data.Mode = FSEL_ALT5));

        PWM_CHANNEL_0_1:
          Ok:= ((Gpio = 13) and (Data.Mode = FSEL_ALT0)) or
               ((Gpio = 19) and (Data.Mode = FSEL_ALT5)) or
               ((Gpio = 45) and (Data.Mode = FSEL_ALT0));

        PWM_CHANNEL_1_0:
          Ok:= ((Gpio = 40) and (Data.Mode = FSEL_ALT0));

        PWM_CHANNEL_1_1:
          Ok:= ((Gpio = 41) and (Data.Mode = FSEL_ALT0));
      end;
    end
    else
    begin
      case PwmChan of
        PWM_CHANNEL_0_0:
          Ok:= ((Gpio = 12) and (Data.Mode = FSEL_ALT0)) or
               ((Gpio = 18) and (Data.Mode = FSEL_ALT5)) or
               ((Gpio = 40) and (Data.Mode = FSEL_ALT0));

        PWM_CHANNEL_0_1:
          Ok:= ((Gpio = 13) and (Data.Mode = FSEL_ALT0)) or
               ((Gpio = 19) and (Data.Mode = FSEL_ALT5)) or
               ((Gpio = 45) and (Data.Mode = FSEL_ALT0)) or
               ((Gpio = 41) and (Data.Mode = FSEL_ALT0));
      end;
    end;

    If Ok then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result)-1]:= Gpio;
    end;
  end;
end;


// ------------------------------------------------------------------------


function TPiGpio.GetRawClockData(ClkNo: Integer; var Data: TClock): Boolean;
var
  pCtl,pDiv: ^LongWord;

begin
  if not IsCpuOk then Exit(False);
  if UsingGpioMem then Exit(False);
  Result:= True;

  case ClkNo of
    CLK_GPIO0: begin pCtl:= SetPtr(PClkMem, CLK_GP0_CTL); pDiv:= SetPtr(PClkMem, CLK_GP0_DIV); end;
    CLK_GPIO1: begin pCtl:= SetPtr(PClkMem, CLK_GP1_CTL); pDiv:= SetPtr(PClkMem, CLK_GP1_DIV); end;
    CLK_GPIO2: begin pCtl:= SetPtr(PClkMem, CLK_GP2_CTL); pDiv:= SetPtr(PClkMem, CLK_GP2_DIV); end;
    CLK_PCM:   begin pCtl:= SetPtr(PClkMem, CLK_PCM_CTL); pDiv:= SetPtr(PClkMem, CLK_PCM_DIV); end;
    CLK_PWM:   begin pCtl:= SetPtr(PClkMem, CLK_PWM_CTL); pDiv:= SetPtr(PClkMem, CLK_PWM_DIV); end;
    else Exit(False);
  end;

  Data.Control:= pCtl^;
  Data.Divisor:= pDiv^;
end;


// ------------------------------------------------------------------------


// -----------------------------------------------
// Calculate Frequency of a clock
// From the manual: Freq:= Source / (DIVI + DIVF / 1024)
// This is dependent on MESH mode !!! Assumes MESH1 mode
// -----------------------------------------------
function TPiGpio.GetClockFrequency(ClkNo: Integer): LongWord;
var
  ClkData: TClock;
  Di,Fr,PiFreq: LongWord;

begin
  if not GetRawClockData(ClkNo, ClkData{%H-}) then Exit(0);

  // Check for Clock running (b4=1)
  if ClkData.Control and (1 shl 4) = 0 then
  begin
    Exit(0);          // Clock not running
  end;

  if FRPiModel.Cpu = PI_CPU_BCM2711 then
  begin
    if ClkData.Control and $0F = CLK_SRC_PLLD
      then PiFreq:= CLK_PLLD_FREQ_2711
      else PiFreq:= CLK_OSC_FREQ_2711;
  end
  else
  begin
    if ClkData.Control and $0F = CLK_SRC_PLLD
      then PiFreq:= CLK_PLLD_FREQ
      else PiFreq:= CLK_OSC_FREQ;
  end;

  // Divisor (B12-B23)
  Di:= (ClkData.Divisor shr 12) and $FFF;

  // Fraction (B0-B11)
  Fr:= ClkData.Divisor and $FFF;

  if Di > 0
    then Result:= Trunc(PiFreq / (Di + (Fr / 1024)))
    else Result:= 0;
end;



// ------------------------------------------------------------------------


function TPiGpio.GetRawPwmData(PwmGroup: Integer; var Data: TPwmData): Boolean;
var
  pPwm: ^LongWord;
  Ofs: LongWord;

begin
  if not IsCpuOk then Exit(False);
  if UsingGpioMem then Exit(False);

  if (PwmGroup = PWM_GROUP_1) and
     (FRPiModel.Cpu <> PI_CPU_BCM2711) then
  begin
    FLastErrorStr:= 'PWM Group 1 not supported on this CPU';
    Exit(False);
  end;

  Result:= True;

  case PwmGroup of
    PWM_GROUP_0: Ofs:= PWM0_OFFSET;
    PWM_GROUP_1: Ofs:= PWM1_OFFSET;
    else Exit(False);
  end;

  pPwm:= SetPtr(PPwmMem, Ofs, PWM_CONTROL);
  Data.Control:= pPwm^;

  pPwm:= SetPtr(PPwmMem, Ofs, PWM_STATUS);
  Data.Status:= pPwm^;

  pPwm:= SetPtr(PPwmMem, Ofs, PWM_DMACTL);
  Data.DMA:= pPwm^;

  pPwm:= SetPtr(PPwmMem, Ofs, PWM_FIFO);
  Data.FIFO:= pPwm^;

  pPwm:= SetPtr(PPwmMem, Ofs, PWM0_RANGE);
  Data.Channels[0].Range:= pPwm^;

  pPwm:= SetPtr(PPwmMem, Ofs, PWM0_DATA);
  Data.Channels[0].Data:= pPwm^;

  pPwm:= SetPtr(PPwmMem, Ofs, PWM1_RANGE);
  Data.Channels[1].Range:= pPwm^;

  pPwm:= SetPtr(PPwmMem, Ofs, PWM1_DATA);
  Data.Channels[1].Data:= pPwm^;
end;



end.
