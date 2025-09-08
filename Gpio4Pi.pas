unit Gpio4Pi;

// ------------------------------------------------------------------------
//
// GPIO for Raspberry Pi 1 to 5
// ----------------------------
//
// Can be used on:
// - Pi 1 (BCM2835) - Tested with Pi 1.2
// - Pi 2 (BCM2836) - I don't have a Pi 2
// - Pi 3 (BCM2837) - Tested with Pi 3B+. Both 32 and 64 bit
// - Pi 4 (BCM2711) - Tested with Pi 4B. Both 32 and 64 bit
// - Pi 5 (BCM2712) - Tested with Pi 5. Both 32 and 64 bit
//
// This unit / object uses ONLY GPIO pin numbers which directly refer
// to BCM GPIO numbers.
//
// This unit/object is inspired by WiringPi and parts of the code and definitions
// have been converted to Pascal.
//
// There is support for GPIO Clock, where the frequency for the GPIO pin is specified.
// The divisor is calculated automatically on all Pi's,
// which run with different clock frequencies.
//
// There is also support for PWM, where the PWM channel is selected automatically
// based on the pin number.
// Both PWM channels can therefore be used with different frequencies and Pulse/Pause ratios.
//
//
// Still under development and therefore not quite finished
// Copyright (c) 2024-2025 Jan Andersen
// ------------------------------------------------------------------------

{$Mode objfpc}{$H+}
{$inline on}

interface
 
uses
  Classes, SysUtils, GpioDefs;


type
  // Pi1-Pi5: One GPIO returned by GetGpioPinData()
  TGpioPin = Record
    Mode:  Byte;     // Input, Output, Alt0 ... Alt8
    Pull:  Byte;     // Pull Up/Down. ONLY on Pi4 and Pi5
    Level: Byte;     // 0 or 1
  end;

  // Pi1-Pi5: One Clock
  TGpioClk = Record
    Control: LongWord;
    Divisor: LongWord;
    Fract:   LongWord;
  end;

  // Pi1-Pi5: One PWM Channel
  TPwmChannel = Record
    Case Cpu: Integer Of
      PI_CPU_BCM2835,
      PI_CPU_BCM2836,
      PI_CPU_BCM2837,
      PI_CPU_BCM2711: (Range: LongWord;
                       Data:  LongWord);

      PI_CPU_BCM2712: (Rp1Control: LongWord;
                       Rp1Range:   LongWord;
                       Rp1Phase:   LongWord;
                       Rp1Duty:    LongWord);
  end;

  // Pi1-Pi5: One PWM Block
  TPwmData = Record
    Case Cpu: Integer Of
      PI_CPU_BCM2835,
      PI_CPU_BCM2836,
      PI_CPU_BCM2837,
      PI_CPU_BCM2711: (Control:  LongWord;
                       Status:   LongWord;
                       DMA:      LongWord;
                       FIFO:     LongWord;
                       Channels: Array [0..1] of TPwmChannel);

      PI_CPU_BCM2712: (Rp1GlobCtrl: LongWord;
                       Rp1FifoCtrl: LongWord;
                       Rp1ComRange: LongWord;
                       Rp1ComDuty:  LongWord;
                       Rp1DutyFifo: LongWord;
                       Rp1Channels: Array [0..3] of TPwmChannel);
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
  protected
    FRPiModel:     TRPiModelInfo;
    FGpioBaseMem:  QWord;
    FUsingGpioMem: Boolean;
    FLastErrorStr: String;

    pGpioMem:   pLongWord;     // Mapped GPIO mem.  Both in /dev/mem and /dev/gpiomem
    pClkMem:    pLongWord;     // Mapped Clock mem. Only in /dev/mem
    pPwmMem:    pLongWord;     // Mapped PWM mem.   Only in /dev/mem
    pUartMem:   pLongWord;     // Mapped UART mem.  Only in /dev/mem
    pRP1Mem:    pLongWord;     // PI5: Start of mapped memory
    pRP1RioMem: pLongWord;     // PI5: SysRio memory
    pRP1PadMem: pLongWord;     // PI5: Pad memory

    function LoadRasPiData: Boolean;
    function GetGpioHighestPin: Byte;
    function IsGpioPinOk(Gpin: Byte): Boolean;
    function IsCpuOk: Boolean;
    function UsingGpioMemCheck: Boolean;
    function GetPwmBasePtr(Gpin: Byte; Ofs: Word): Pointer;
    function SetBaseClock(ClkNo: Integer; Freq: Integer): Boolean;
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
    // Gpin: GPIO pin number, 0-53/0-57/0-27 depending on CPU
    // Mode: PM_INPUT, PM_OUTPUT, PM_ALT0...PM_ALT8,
    //       PM_PWMOUT_MS, PM_PWMOUT_BAL, PM_GPIO_CLOCK,
    //       PM_GPIO_OFF (Pi5)
    // Return: True on success, False on error
    // ----------------------------------------
    function SetPinMode(Gpin, Mode: Byte): Boolean;

    // ----------------------------------------
    // SetPullMode:
    // Sets the selected GPIO pin to the desired Pull-Down/Up/None
    // Gpin: GPIO pin number, 0-53/0-57/0-27 depending on CPU
    // Mode: PUD_OFF, PUD_DOWN, PUD_UP
    // Return: True on success, False on error
    // ----------------------------------------
    function SetPullMode(Gpin, Mode: Byte): Boolean;

    // ----------------------------------------
    // GpioWrite:
    // Sets the selected GPIO pin to low or high
    // Gpin: GPIO pin number, 0-53/0-57/0-27 depending on CPU
    // Value: PIN_LOW, PIN_HIGH, 0, 1. (Only bit 0 are used)
    // Only makes sense if the pin is set to Output mode
    // Return: True on success, False on error
    // ----------------------------------------
    function GpioWrite(Gpin, Value: Byte): Boolean;

    // ----------------------------------------
    // GpioRead:
    // Returns the Low/High state of selected GPIO pin
    // Can read both Input and Output pins
    // Gpin: GPIO pin number, 0-53/0-57/0-27 depending on CPU
    // Return: PIN_LOW,PIN_HIGH,0,1. $FF on Error
    // ----------------------------------------
    function GpioRead(Gpin: Byte): Byte;

    // ----------------------------------------
    // GetGpioPinData:
    // Get the Data for a GPIO pin no matter who has set the pin
    // Gpin: GPIO pin number, 0-53/0-57/0-27 depending on CPU
    // Data: Data returned to the caller:
    //       .Mode: PM_INPUT, PM_OUTPUT, PM_ALT0..PM_ALT8
    //             IT DOES NOT RETURN: PM_PWMOUT_MS,PM_PWMOUT_BAL,PM_GPIO_CLOCK
    //       .Pull: PUD_OFF, PUD_DOWN, PUD_UP   (ONLY Pi4 and Pi5)
    //       .Level: The level of the pin, 0 or 1 (PIN_LOW, PIN_HIGH)
    // Return: True on success, False on error
    // ----------------------------------------
    function GetGpioPinData(Gpin: LongWord; var Data: TGpioPin): Boolean;

    // ----------------------------------------
    // SetGpioClock:
    // Output the given frequency on the selected pin
    // Gpin: GPIO pin number, can be one of the following:
    //       Pi 1-4: 4,5,6,20,21,32,34,42,43,44
    //       Pi 5:   4,5,6,18,20,21
    // Freq: The frequency in Hz. Can be in the range for
    //       Pi 1-3:  4.688 Hz to 250 MHz
    //       Pi 4:   13.186 Hz to 375 MHz
    //       Pi 5:      763 Hz to  50 MHz
    // The BCM2711 manual says:
    // "The maximum operating frequency of the General Purpose clocks is ~125MHz at 1.2V but
    //  this will be reduced if the GPIO pins are heavily loaded or have a capacitive load"
    // If the frequency is set outside these ranges,
    // it will not be set and false will be returned.
    // If set to 0 the Clock is turned off.
    // Return: True on success, False on error
    // ----------------------------------------
    function SetGpioClock(Gpin: Byte; Freq: Integer): Boolean;

    // -------------------------------------------------------------
    // GetGpiosForGpioClock:
    // Find all the GPIOs that are assigned to a GpioClock
    // GpioClk: CLK_GPIO_0, CLK_GPIO_1, CLK_GPIO_2
    // Return: Array of GPIO's. Empty = No GPIOs assigned
    // -------------------------------------------------------------
    function GetGpiosForGpioClock(GpioClk: Integer): TIntArray;

    // ----------------------------------------
    // SetPwmMasterClock:
    // Sets the given frequency on the PWM master clock generator
    // The PWM clock is a common clock for all PWM channels
    // Freq: The frequency in Hz. Can be in the range for
    //       Pi 1-3:  4.688 Hz to 250 MHz
    //       Pi 4:   13.186 Hz to 375 MHz
    //       Pi 5:      382 Hz to  25 MHz
    // If the frequency is set outside these ranges,
    // it will not be set and false will be returned.
    // If set to 0 the Clock is turned off.
    // Return: True on success, False on error
    // ----------------------------------------
    function SetPwmMasterClock(Freq: Integer): Boolean;

    // ----------------------------------------
    // SetUartMasterClock:
    // Sets the given frequency on the UART master clock generator
    // The UART clock is a common clock for all UART's
    // Freq: The frequency in Hz. Can be in the range for
    //       Pi 1-3:  4.688 Hz to 250 MHz
    //       Pi 4:   13.186 Hz to 375 MHz
    //       Pi 5:       ?? Hz to  ?? MHz
    // If the frequency is set outside these ranges,
    // it will not be set and false will be returned.
    // If set to 0 the Clock is turned off.
    // Return: True on success, False on error
    // ----------------------------------------
    function SetUartMasterClock(Freq: Integer): Boolean;

    // -----------------------------------------------
    // GetRawClockData:
    // Returns the Raw Control and Divisor for a clock
    // ClkNo: CLK_GPIO0, CLK_GPIO1, CLK_GPIO2, CLK_PWM, CLK_UART, CLK_PCM
    // Data:  Data returned to the caller:
    //        .Control: Control word from the clock
    //        .Divisor: Int Divisor from the clock
    //        .Fract:   Fraction from the clock
    // Consult the BCM manual for description of Control words
    // Return: True on success, False on error
    // -----------------------------------------------
    function GetRawClockData(ClkNo: Integer; var Data: TGpioClk): Boolean;

    // -----------------------------------------------
    // GetClockFrequency:
    // Calculate Frequency of a clock
    // ClkNo: CLK_GPIO0, CLK_GPIO1, CLK_GPIO2, CLK_PWM, CLK_UART, CLK_PCM
    // Return: The frequency in Hz
    // -----------------------------------------------
    function GetClockFrequency(ClkNo: Integer): LongWord;

    // ----------------------------------------
    // SetPwmRange:
    // In Mark/Space mode it behaves as a divider for the PWM master clock,
    // so the PWM frequency = PwmMasterClock / Range
    // Gpin: GPIO pin number, can be one of the following:
    //       Pi 1-4: 12,13,18,19,40,41,45
    //       Pi 5:   12,13,14,15,18,19
    // Range: Pi 1-4: 2 - 4294967295 ($FFFFFFFF)
    //        Pi 5:   2 - 4294967295 ($FFFFFFFF)
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
    //       Pi 1-4: 12,13,18,19,40,41,45
    //       Pi 5:   12,13,14,15,18,19
    // Value: 0 to PWM Range
    // Return: True on success, False on error
    // ----------------------------------------
    function SetPwmValue(Gpin: Byte; Value: LongWord): Boolean;

    // ----------------------------------------
    // SetPwmDutyCycle:
    // Just a helper function whitch calculate the PWM value
    // and calls SetPwmValue()
    // Gpin: GPIO pin number, can be one of the following:
    //       Pi 1-4: 12,13,18,19,40,41,45
    //       Pi 5:   12,13,14,15,18,19
    // DutyCycle: 0.00 to 100.00%
    // Return: True on success, False on error
    // ----------------------------------------
    function SetPwmDutyCycle(Gpin: Byte; DutyCycle: Single): Boolean;

    // ----------------------------------------
    // SetPwmMode:
    // Sets the selected GPIO pin to the desired PWM Mode
    // There are 2 PWM channels available and the right channel is automatically
    // selected based on the pin number.
    // On the BCM2711 Cpu (Pi 4) there are 2*2 PWM channels
    // Be aware that different pins shares the same channel,
    // look further down in the source code
    // Gpin: GPIO pin number, can be one of the following:
    //       Pi 1-4: 12,13,18,19,40,41,45
    //       Pi 5:   12,13,14,15,18,19
    // Mode: Pi 1-4: PWM_MODE_OFF, PWM_MODE_BAL, PWM_MODE_MS  (use PWM_MODE_MS)
    //       Pi 5:   Only PWM_MODE_OFF, PWM_MODE_MS is supported
    // Return: True on success, False on error
    // ----------------------------------------
    function SetPwmMode(Gpin: Byte; Mode: Byte): Boolean;

    // -------------------------------------------------------------
    // GetGpiosForPwm:
    // Find all the GPIOs that are assigned to a PWM channel
    // PwmChan: Pi 1-3: PWM_CHANNEL_0_0, PWM_CHANNEL_0_1
    //          Pi 4:   PWM_CHANNEL_0_0, PWM_CHANNEL_0_1
    //                  PWM_CHANNEL_1_0, PWM_CHANNEL_1_1
    //          Pi 5:   PWM_CHANNEL_0_0, PWM_CHANNEL_0_1, PWM_CHANNEL_0_2, PWM_CHANNEL_0_3
    //                  PWM_CHANNEL_1_0, PWM_CHANNEL_1_1, PWM_CHANNEL_1_2, PWM_CHANNEL_1_3
    // Return: Array of GPIO's. Empty = No GPIOs assigned
    // -------------------------------------------------------------
    function GetGpiosForPwm(PwmChan: Integer): TIntArray;

    // -----------------------------------------------
    // GetRawPwmData:
    // Returns the Raw data for a PWM group with 2/4 channels
    // PwmBlock: PWM_GROUP_0 or PWM_GROUP_1 (Group 1 only on Pi4,Pi5)
    // Data: Pi 1-4: Data returned to the caller:
    //       .Cpu:      PI_CPU_BCM2835, PI_CPU_BCM2836, PI_CPU_BCM2837, PI_CPU_BCM2711
    //       .Control:  PWM Control word
    //       .Status:   PWM Status word
    //       .DMA:      PWM DMA Configuration
    //       .FIFO:     PWM FIFO Input
    //       .Channels: PWM Channel 0 and Channel 1 (Array[0..1])
    //         [x].Range: PWM Channel 0/1 Range
    //         [x].Data:  PWM Channel 0/1 Data
    // Data: Pi 5: Data returned to the caller:
    //       .Cpu:         PI_CPU_BCM2712
    //       .Rp1GlobCtrl: GLOBAL_CTRL Register
    //       .Rp1FifoCtrl: FIFO_CTRL Register
    //       .Rp1ComRange: COMMON_RANGE Register
    //       .Rp1ComDuty:  COMMON_DUTY Register
    //       .Rp1DutyFifo: DUTY_FIFO Register
    //       .Rp1Channels: PWM Channel 0 to Channel 3 (Array[0..3])
    //         [x].Rp1Control: CHANx_CTRL Register
    //         [x].Rp1Range:   CHANx_RANGE Register
    //         [x].Rp1Phase:   CHANx_PHASE Register
    //         [x].Rp1Duty:    CHANx_DUTY Register
    // Consult the BCM / RP1 manual for description of a PWM block
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

    // Make all Pointers public if someone needed it ........
    property GetGpioMem:   pLongWord Read pGpioMem;
    property GetClkMem:    pLongWord Read pClkMem;
    property GetPwmMem:    pLongWord Read pPwmMem;
    property GetUartMem:   pLongWord Read pUartMem;
    property GetRP1Mem:    pLongWord Read pRP1Mem;
    property GetRP1RioMem: pLongWord Read pRP1RioMem;
    property GetRP1PadMem: pLongWord Read pRP1PadMem;
  end;


   
procedure DelayMicroSeconds(HowLong: LongWord);
function SetPtr(BasePtr: Pointer; Ofs: LongWord): Pointer; inline;
function SetPtr(BasePtr: Pointer; Ofs1,Ofs2: LongWord): Pointer; inline; overload;


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
  MemSize: Size_T;
  DevMem, DevGpioMem: String;

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
    PI_CPU_BCM2712: FGpioBaseMem:= GPIO_PERI_BASE_2712;
    Else
    begin                   // Unknown CPU
      FreeAndNil(Self);
      Exit;
    end;
  end;

  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    DevMem:=     '/dev/mem';
    DevGpioMem:= '/dev/gpiomem0';
  end
  else
  begin
    DevMem:=     '/dev/mem';
    DevGpioMem:= '/dev/gpiomem';
  end;

  // Open the master /dev/ memory control device
  // Try /dev/mem. If that fails, then
  // try /dev/gpiomem. If that fails then game over.
  Fd:= {%H-}fpOpen(DevMem, O_RdWr or O_Sync);    // Try to open the master /dev/mem device
  if Fd < 0 then
  begin
    Fd:= {%H-}fpOpen(DevGpioMem, O_RdWr or O_Sync);  // Open the /dev/gpiomem
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


  // RaspberryPi 1 to RaspberryPi 4
  if FRPiModel.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
  begin
    // Allways map GPIO memory
    PGpioMem:= FpMmap(Nil, PAGE_SIZE, PROT_READ or PROT_WRITE, MAP_SHARED,
                      Fd, FGpioBaseMem + GPIO_BASE);
    if (PGpioMem = MAP_FAILED) then PGpioMem:= nil;

    // Only map all oyhers if not using GpioMem
    if not FUsingGpioMem then
    begin
      PClkMem:= FpMmap(Nil, PAGE_SIZE, PROT_READ or PROT_WRITE, MAP_SHARED,
                       Fd, FGpioBaseMem + CLOCK_BASE);
      if (PClkMem = MAP_FAILED) then PClkMem:= nil;

      PPwmMem:= FpMmap(Nil, PAGE_SIZE, PROT_READ or PROT_WRITE, MAP_SHARED,
                       Fd, FGpioBaseMem + PWM_BASE);
      if (PPwmMem = MAP_FAILED) then PPwmMem:= nil;

      PUartMem:= FpMmap(Nil, PAGE_SIZE, PROT_READ or PROT_WRITE, MAP_SHARED,
                        Fd, FGpioBaseMem + UART_BASE);
      if (PUartMem = MAP_FAILED) then PUartMem:= nil;
    end;
  end;


  // RaspberryPi 5
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    // Pi5. Map the hole memory space
    if FUsingGpioMem
      then MemSize:= PAGE_SIZE_RP1_GPIO
      else MemSize:= PAGE_SIZE_RP1_MEM;

    PRP1Mem:= FpMmap(Nil, MemSize, PROT_READ or PROT_WRITE, MAP_SHARED, Fd, FGpioBaseMem);
    if (PRP1Mem = MAP_FAILED) then PRP1Mem:= nil;

    if PRP1Mem <> nil then
    begin
      if FUsingGpioMem then
      begin
        PGpioMem:=   PRP1Mem;
        PRP1RioMem:= PRP1Mem + (RP1_RIO_BASE-RP1_GPIO_BASE)  div 4;
        PRP1PadMem:= PRP1Mem + (RP1_PADS_BASE-RP1_GPIO_BASE) div 4;
      end
      else
      begin
        PGpioMem:=   PRP1Mem + RP1_GPIO_BASE  div 4;
        PRP1RioMem:= PRP1Mem + RP1_RIO_BASE   div 4;
        PRP1PadMem:= PRP1Mem + RP1_PADS_BASE  div 4;
        PClkMem:=    PRP1Mem + RP1_CLOCK_BASE div 4;
        PPwmMem:=    PRP1Mem + RP1_PWM_BASE   div 4;
        PUartMem:=   PRP1Mem + RP1_UART_BASE  div 4;
      end;
    end;
  end;

  // After mapping we can close the file
  if Fd >= 0 then
  begin
    fpClose(Fd);
    Fd:= 0;
  end;

  // Check for mapping ok
  if (PGpioMem = nil) or
     ((not FUsingGpioMem) and
      ((PClkMem = Nil) or (PPwmMem = Nil) or (PUartMem = Nil))) then
  begin
    Self.Destroy;
{$ifdef CPU32}
    FreeAndNil(Self);
{$endif}
    Self:= Nil;
    Exit;
  end;
end;

// ------------------------------------------------------------------------

destructor TPiGpio.Destroy;
var
  MemSize: Size_T;

begin
  if PRP1Mem <> nil then
  begin
    if FUsingGpioMem
      then MemSize:= PAGE_SIZE_RP1_GPIO
      else MemSize:= PAGE_SIZE_RP1_MEM;

    fpMUnmap(PRP1Mem, MemSize);
    PRP1Mem:=  Nil;
    PGpioMem:= Nil;
    PClkMem:=  Nil;
    PPwmMem:=  Nil;
    PUartMem:= Nil;
  end;

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

  if PUartMem <> nil then
  begin
    fpMUnmap(PUartMem, PAGE_SIZE);
    PUartMem:= Nil;
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
  Fd:= {%H-}fpOpen('/proc/device-tree/system/linux,revision', O_RdOnly);
  if Fd >= 0 then
  begin
    // Read binary file direct into Rev variable
    Cnt:= {%H-}fpRead(Fd, Rev, SizeOf(Rev));
    FpClose(Fd);
    if Cnt <> SizeOf(Rev) then Rev:= 0;
    Rev:= SwapEndian(Rev);
  end;

  // If device-tree failed, then try /proc/cpuinfo
  if Rev = 0 then
  begin
    Fd:= {%H-}fpOpen('/proc/cpuinfo', O_RdOnly);
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
    PI_CPU_BCM2712: Result:= 53;
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
//
// GPIO STUFF
//
// ------------------------------------------------------------------------

// Helpers for Raspberry Pi 5 to set correct Bank and adjust Gpin
procedure Rp1SetOffsAndPin(var BankOffs: LongWord; var Gpin: Byte); inline;
begin
  case Gpin of
    0..27:  begin BankOffs:= $0000;          Gpin:= Gpin;    end;
    28..33: begin BankOffs:= RP1_GPIO1_OFFS; Gpin:= Gpin-28; end;
    34..53: begin BankOffs:= RP1_GPIO2_OFFS; Gpin:= Gpin-34; end;
    else          BankOffs:= $0000;
  end;
end;

procedure Rp1SetOffsAndPin(var BankOffs: LongWord; var Gpin: LongWord); inline; overload;
var
  Gp: Byte;
begin
  Gp:= Gpin;
  Rp1SetOffsAndPin(BankOffs, Gp);
  Gpin:= Gp;
end;

// ------------------------------------------------------------------------

function TPiGpio.SetPinMode(Gpin, Mode: Byte): Boolean;
var
  fSel, Shift, Alt: Byte;
  pGpio, pRio, pPad: ^LongWord;
  BankOffs: LongWord;

begin
  if not IsGpioPinOk(Gpin) then Exit(False);
  Result:= False;

  // RaspberryPi 1 to RaspberryPi 4
  if FRPiModel.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
  begin
    Result:= True;
    fSel:=  (Gpin div 10) * 4;    // Select Gpfsel 0 to 5 register. *4 = LongWord
    Shift:= (Gpin mod 10) * 3;    // 0-9 pin shift. *3 = 3 bits
    pGpio:= SetPtr(PGpioMem, fSel);

    Case Mode of
      PM_INPUT, PM_OUTPUT,
      PM_ALT0,  PM_ALT1, PM_ALT2,
      PM_ALT3,  PM_ALT4, PM_ALT5:
      begin
        Case Mode of                  // Convert PinMode to FselMode
          PM_INPUT:  Alt:= FSEL_INPUT;
          PM_OUTPUT: Alt:= FSEL_OUTPUT;
          PM_ALT0:   Alt:= FSEL_ALT0;
          PM_ALT1:   Alt:= FSEL_ALT1;
          PM_ALT2:   Alt:= FSEL_ALT2;
          PM_ALT3:   Alt:= FSEL_ALT3;
          PM_ALT4:   Alt:= FSEL_ALT4;
          PM_ALT5:   Alt:= FSEL_ALT5;
        end;

        pGpio^:= pGpio^ and ($FFFFFFFF - (7 shl Shift)) or (Alt shl Shift);
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
      end;

      PM_GPIO_OFF:
      begin
        // We can't turn the GPIO Off
      end;
    end;
  end;


  // RaspberryPi 5
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    Result:= True;

    Rp1SetOffsAndPin(BankOffs{%H-}, Gpin);
    pGpio:= SetPtr(PGpioMem,   BankOffs, RP1_GPIO_CTRL + (Gpin * 8));
    pRio:=  SetPtr(PRP1RioMem, BankOffs, RP1_RIO_OE);
    pPad:=  SetPtr(PRP1PadMem, BankOffs, RP1_PAD_PADS_OFFS + (Gpin * 4));

    case Mode of
      PM_INPUT:
      begin
        pGpio^:= RP1_GPIO_FSEL_GPIO or RP1_GPIO_FILTER;
        pPad^:=  RP1_PAD_DRV_4MA or RP1_PAD_SCHMITT or RP1_PAD_OD or RP1_PAD_IE;
        pRio^:=  pRio^ and (not (1 shl Gpin));
      end;

      PM_OUTPUT:
      begin
        pGpio^:= RP1_GPIO_FSEL_GPIO;
        pPad^:=  RP1_PAD_DRV_4MA or RP1_PAD_SCHMITT or RP1_PAD_IE;
        pRio^:=  pRio^ or LongWord(1 shl Gpin);
      end;

      PM_ALT0: pGpio^:= pGpio^ and (not RP1_GPIO_FSEL_NONE);
      PM_ALT1: pGpio^:= pGpio^ and (not RP1_GPIO_FSEL_NONE) or 1;
      PM_ALT2: pGpio^:= pGpio^ and (not RP1_GPIO_FSEL_NONE) or 2;
      PM_ALT3: pGpio^:= pGpio^ and (not RP1_GPIO_FSEL_NONE) or 3;
      PM_ALT4: pGpio^:= pGpio^ and (not RP1_GPIO_FSEL_NONE) or 4;
      PM_ALT5: pGpio^:= pGpio^ and (not RP1_GPIO_FSEL_NONE) or 5;
      PM_ALT6: pGpio^:= pGpio^ and (not RP1_GPIO_FSEL_NONE) or 6;
      PM_ALT7: pGpio^:= pGpio^ and (not RP1_GPIO_FSEL_NONE) or 7;
      PM_ALT8: pGpio^:= pGpio^ and (not RP1_GPIO_FSEL_NONE) or 8;

      PM_PWMOUT_MS:
      begin
        if UsingGpioMem then Exit(False);

        Case Gpin of
          12,13,14,15: Alt:= 0;
          18,19:       Alt:= 3;
          else
          begin
            FLastErrorStr:= 'PWM Output only on GPIO 12,13,14,15,18,19';
            Exit(False);
          end;
        end;

        // Set Alt0 / Alt3 Mode
        pGpio^:= Alt or RP1_GPIO_FILTER;
        pPad^:=  RP1_PAD_DRV_4MA or RP1_PAD_SCHMITT or RP1_PAD_PDE or RP1_PAD_IE;
        pRio^:=  pRio^ or LongWord(1 shl Gpin);

        // Set PWM Mode. ONLY MarkSpace Mode.
        Result:= SetPwmMode(Gpin, PWM_MODE_MS);
      end;

      PM_PWMOUT_BAL:
      begin
        FLastErrorStr:= 'PWM Balanced Mode not supported';
        Exit(False);
      end;

      PM_GPIO_CLOCK:
      begin
        if UsingGpioMem then Exit(False);

        // Load Alt group
        Case Gpin of
          4,5,6: Alt:= 0;
          18:    Alt:= 8;
          20,21: Alt:= 3;
          else
          begin
            FLastErrorStr:= 'GPIO Clock only on GPIO 4,5,6,18,20,21';
            Exit(False);
          end;
        end;

        // Set Alt0 / Alt3 / Alt8 Mode
        pGpio^:= pGpio^ and (not RP1_GPIO_FSEL_NONE) or Alt or RP1_GPIO_FILTER;
        pPad^:=  RP1_PAD_DRV_4MA or RP1_PAD_SCHMITT or RP1_PAD_PDE or RP1_PAD_IE;
        pRio^:=  pRio^ or LongWord(1 shl Gpin);
      end;

      PM_GPIO_OFF:
      begin
        pGpio^:= RP1_GPIO_FSEL_NONE;
      end;
    end;
  end;
end;

// ------------------------------------------------------------------------

function TPiGpio.SetPullMode(Gpin, Mode: Byte): Boolean;
var
  fSel, Shift, Pud: Byte;
  pPud, pPudClk: ^LongWord;
  BankOffs: LongWord;

begin
  if not IsGpioPinOk(Gpin) then Exit(False);
  Result:= False;

  // NB: PullUp/Down are different for PI1-3 and PI4 and PI5

  // RaspberryPi 1-3
  Case FRPiModel.Cpu of
    PI_CPU_BCM2835,
    PI_CPU_BCM2836,
    PI_CPU_BCM2837:
    begin
      Result:= True;
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

    // RaspberryPi 4
    PI_CPU_BCM2711:
    begin
      Result:= True;
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

    // RaspberryPi 5
    PI_CPU_BCM2712:
    begin
      Result:= True;

      Rp1SetOffsAndPin(BankOffs{%H-}, Gpin);
      pPud:= SetPtr(PRP1PadMem, BankOffs, RP1_PAD_PADS_OFFS + (Gpin * 4));

      case Mode of
        PUD_DOWN: pPud^:= pPud^ and (not RP1_PAD_PULL_MASK) or RP1_PAD_PDE;
        PUD_UP:   pPud^:= pPud^ and (not RP1_PAD_PULL_MASK) or RP1_PAD_PUE;
        else      pPud^:= pPud^ and (not RP1_PAD_PULL_MASK);
      end;
    end;
  end;
end;

// ------------------------------------------------------------------------

function TPiGpio.GpioWrite(Gpin, Value: Byte): Boolean;
var
  pGpio: ^LongWord;
  BankOffs: LongWord;

begin
  if not IsGpioPinOk(Gpin) then Exit(False);
  Result:= False;

  // RaspberryPi 1 to RaspberryPi 4
  if FRPiModel.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
  begin
    if (Value and $01) = 0
      then pGpio:= SetPtr(PGpioMem, GPCLR0 + ((Gpin shr 5) shl 2))   // (Gpin Div 32) * 4
      else pGpio:= SetPtr(PGpioMem, GPSET0 + ((Gpin shr 5) shl 2));

    pGpio^:= 1 shl (Gpin and $1F);
    Result:= True;
  end;

  // RaspberryPi 5
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    Rp1SetOffsAndPin(BankOffs{%H-}, Gpin);

    if (Value and $01) = 0
      then pGpio:= SetPtr(PRP1RioMem, BankOffs, RP1_RIO_CLR_OFFS)
      else pGpio:= SetPtr(PRP1RioMem, BankOffs, RP1_RIO_SET_OFFS);

    pGpio^:= 1 shl Gpin;
    Result:= True;
  end;
end;

// ------------------------------------------------------------------------

function TPiGpio.GpioRead(Gpin: Byte): Byte;
var
  pGpio: ^LongWord;
  BankOffs: LongWord;

begin
  if not IsGpioPinOk(Gpin) then Exit($FF);
  Result:= $FF;

  // RaspberryPi 1 to RaspberryPi 4
  if FRPiModel.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
  begin
    pGpio:= SetPtr(PGpioMem, GPLEV0 + ((Gpin shr 5) shl 2));   // (Gpin Div 32) * 4

    if (pGpio^ and (1 shl (Gpin and $1F))) = 0
      then Result:= PIN_LOW
      else Result:= PIN_HIGH;
  end;

  // RaspberryPi 5
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    Rp1SetOffsAndPin(BankOffs{%H-}, Gpin);
    pGpio:= SetPtr(PRP1RioMem, BankOffs, RP1_RIO_IN);

    if (pGpio^ and (1 shl Gpin)) = 0
      then Result:= PIN_LOW
      else Result:= PIN_HIGH;
  end;
end;

// ------------------------------------------------------------------------

function TPiGpio.GetGpioPinData(Gpin: LongWord; var Data: TGpioPin): Boolean;
var
  fSel, Shift: Byte;
  pGpio, pPad: ^LongWord;
  BankOffs: LongWord;

begin
  if not IsGpioPinOk(Gpin) then Exit(False);
  Result:= False;

  // RaspberryPi 1 to RaspberryPi 4
  if FRPiModel.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
  begin
    Result:= True;

    // GPIO Mode
    fSel:=  (Gpin div 10) * 4;    // Select Gpfsel 0 to 5 register. *4 = LongWord
    Shift:= (Gpin mod 10) * 3;    // 0-9 pin shift. *3 = 3 bits
    pGpio:= SetPtr(PGpioMem, fSel);

    Case (pGpio^ shr Shift) and $07 of    // Convert FselMode to PinMode
      FSEL_INPUT:  Data.Mode:= PM_INPUT;
      FSEL_OUTPUT: Data.Mode:= PM_OUTPUT;
      FSEL_ALT0:   Data.Mode:= PM_ALT0;
      FSEL_ALT1:   Data.Mode:= PM_ALT1;
      FSEL_ALT2:   Data.Mode:= PM_ALT2;
      FSEL_ALT3:   Data.Mode:= PM_ALT3;
      FSEL_ALT4:   Data.Mode:= PM_ALT4;
      FSEL_ALT5:   Data.Mode:= PM_ALT5;
    end;

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

  // RaspberryPi 5
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    Result:= True;

    Rp1SetOffsAndPin(BankOffs{%H-}, Gpin);
    pGpio:= SetPtr(PGpioMem,   BankOffs, RP1_GPIO_CTRL + (Gpin * 8));
    pPad:=  SetPtr(PRP1PadMem, BankOffs, RP1_PAD_PADS_OFFS + (Gpin * 4));

    // GPIO Mode
    case (pGpio^ and $1F) of          // Convert FuncSel to PinMode
      0:   Data.Mode:= PM_ALT0;
      1:   Data.Mode:= PM_ALT1;
      2:   Data.Mode:= PM_ALT2;
      3:   Data.Mode:= PM_ALT3;
      4:   Data.Mode:= PM_ALT4;
      5:   Data.Mode:= PM_ALT5;
      6:   Data.Mode:= PM_ALT6;
      7:   Data.Mode:= PM_ALT7;
      8:   Data.Mode:= PM_ALT8;
      else Data.Mode:= PM_GPIO_OFF;
    end;

    if Data.Mode = PM_ALT5 then
    begin
      if (pPad^ and RP1_PAD_OD) = 0 then Data.Mode:= PM_OUTPUT
      else
      if (pPad^ and RP1_PAD_IE) <> 0 then Data.Mode:= PM_INPUT;
    end;

    // Pull Up/Down
    if (pPad^ and RP1_PAD_PUE) <> 0 then Data.Pull:= PUD_UP
    else
    if (pPad^ and RP1_PAD_PDE) <> 0 then Data.Pull:= PUD_DOWN
    else Data.Pull:= PUD_OFF;

    // State, High / Low
    pGpio:= SetPtr(PRP1RioMem, BankOffs, RP1_RIO_IN);
    if (pGpio^ and (1 shl Gpin)) = 0
      then Data.Level:= 0
      else Data.Level:= 1;
  end;
end;

// ------------------------------------------------------------------------

function TPiGpio.GetGpiosForGpioClock(GpioClk: Integer): TIntArray;
var
  Gpio: Integer;
  Ok:   Boolean;
  Data: TGpioPin;

begin
  Result:= [];
  if not IsCpuOk then Exit;

  // RaspberryPi 1 to RaspberryPi 4
  if FRPiModel.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
  begin
    for Gpio in [4,5,6,20,21,32,34,42,43,44] do
    begin
      Ok:= False;
      GetGpioPinData(Gpio, Data{%H-});

      case GpioClk of
        CLK_GPIO0:
          Ok:= ((Gpio = 4)  and (Data.Mode = PM_ALT0)) or
               ((Gpio = 20) and (Data.Mode = PM_ALT5)) or
               ((Gpio = 32) and (Data.Mode = PM_ALT0)) or
               ((Gpio = 34) and (Data.Mode = PM_ALT0));

        CLK_GPIO1:
          Ok:= ((Gpio = 5)  and (Data.Mode = PM_ALT0)) or
               ((Gpio = 21) and (Data.Mode = PM_ALT5)) or
               ((Gpio = 42) and (Data.Mode = PM_ALT0)) or
               ((Gpio = 44) and (Data.Mode = PM_ALT0));

        CLK_GPIO2:
          Ok:= ((Gpio = 6)  and (Data.Mode = PM_ALT0)) or
               ((Gpio = 43) and (Data.Mode = PM_ALT0));
      end;

      If Ok then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result)-1]:= Gpio;
      end;
    end;
  end;

  // RaspberryPi 5
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    for Gpio in [4,5,6,18,20,21,32,33,34,42,43,44,46,47] do
    begin
      Ok:= False;
      GetGpioPinData(Gpio, Data{%H-});

      case GpioClk of
        CLK_GPIO0:
          Ok:= ((Gpio = 4)  and (Data.Mode = PM_ALT0)) or
               ((Gpio = 20) and (Data.Mode = PM_ALT3));

        CLK_GPIO1:
          Ok:= ((Gpio = 5)  and (Data.Mode = PM_ALT0)) or
               ((Gpio = 21) and (Data.Mode = PM_ALT3)) or
               ((Gpio = 18) and (Data.Mode = PM_ALT8));

        CLK_GPIO2:
          Ok:= ((Gpio = 6)  and (Data.Mode = PM_ALT0));

        CLK_GPIO3:
          Ok:= ((Gpio = 32) and (Data.Mode = PM_ALT1)) or
               ((Gpio = 34) and (Data.Mode = PM_ALT1)) or
               ((Gpio = 46) and (Data.Mode = PM_ALT0));

        CLK_GPIO4:
          Ok:= ((Gpio = 33) and (Data.Mode = PM_ALT1)) or
               ((Gpio = 43) and (Data.Mode = PM_ALT0));

        CLK_GPIO5:
          Ok:= ((Gpio = 42) and (Data.Mode = PM_ALT0)) or
               ((Gpio = 44) and (Data.Mode = PM_ALT0)) or
               ((Gpio = 47) and (Data.Mode = PM_ALT0));
      end;

      If Ok then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result)-1]:= Gpio;
      end;
    end;
  end;
end;


// ------------------------------------------------------------------------
//
// CLOCK STUFF
//
// ------------------------------------------------------------------------

function TPiGpio.SetGpioClock(Gpin: Byte; Freq: Integer): Boolean;
var
  ClkNo: Integer;

begin
  if not IsCpuOk then Exit(False);
  if UsingGpioMem then Exit(False);

  // RaspberryPi 1 to RaspberryPi 4
  if FRPiModel.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
  begin
    // Load Clock group
    Case Gpin of
      4,20{,32,34}: ClkNo:= CLK_GPIO0;
      5,21{,42,44}: ClkNo:= CLK_GPIO1;
      6{,43}:       ClkNo:= CLK_GPIO2;
      // We also have GPIO clock on pin 32,34,42,43,44.
      // These pins are not on the pin header, so we don't use it
      else
      begin
        FLastErrorStr:= 'GPIO Clock only on GPIO 4,5,6,20,21';
        Exit(False);
      end;
    end;
  end;

  // RaspberryPi 5
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    // Load Clock group
    Case Gpin of
      4,20:    ClkNo:= CLK_GPIO0;
      5,18,21: ClkNo:= CLK_GPIO1;
      6:       ClkNo:= CLK_GPIO2;
      // We also have GPIO clock 3,4,5.
      // These pins are not on the pin header.
      else
      begin
        FLastErrorStr:= 'GPIO Clock only on GPIO 4,5,6,18,20,21';
        Exit(False);
      end;
    end;
  end;

  Result:= SetBaseClock(ClkNo, Freq);
end;

// ------------------------------------------------------------------------

function TPiGpio.SetPwmMasterClock(Freq: Integer): Boolean;
var
  PwmCont: LongWord;
  pPwm: ^LongWord;

begin
  if not IsCpuOk then Exit(False);
  if UsingGpioMem then Exit(False);
  Result:= False;

  // RaspberryPi 1 to RaspberryPi 4
  if FRPiModel.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
  begin
    pPwm:= SetPtr(PPwmMem, PWM_CONTROL);
    PwmCont:= pPwm^;                  // preserve PWM_CONTROL
    pPwm^:= 0;                        // Stop PWM

    // Set PWM Clock
    Result:= SetBaseClock(CLK_PWM, Freq);

    // restore PWM_CONTROL
    pPwm^:= PwmCont;
  end;


  // RaspberryPi 5
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    // Do we need to stop PWM before setting the clock?

    // Set PWM Clock
    Result:= SetBaseClock(CLK_PWM, Freq);
  end;
end;

// ------------------------------------------------------------------------

function TPiGpio.SetUartMasterClock(Freq: Integer): Boolean;
begin
  if not IsCpuOk then Exit(False);
  if UsingGpioMem then Exit(False);

  // Set PWM Clock
  Result:= SetBaseClock(CLK_UART, Freq);
end;

// ------------------------------------------------------------------------

function TPiGpio.SetBaseClock(ClkNo: Integer; Freq: Integer): Boolean;
var
  pCtl,pDivI,pDivF,pSel,pOE: ^LongWord;
  PiFreq,DivI,DivF: LongWord;
  ClkSrc: LongWord;
  MinDiv: LongWord;
  Rp1ClkDef: TRp1Clk;

begin
  if not IsCpuOk then Exit(False);
  if UsingGpioMem then Exit(False);
  Result:= False;

  // RaspberryPi 1 to RaspberryPi 4
  if FRPiModel.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
  begin
    Result:= True;

    case ClkNo of
      CLK_GPIO0: pCtl:= SetPtr(PClkMem, CLK_GP0_CTL);
      CLK_GPIO1: pCtl:= SetPtr(PClkMem, CLK_GP1_CTL);
      CLK_GPIO2: pCtl:= SetPtr(PClkMem, CLK_GP2_CTL);
      CLK_PWM:   pCtl:= SetPtr(PClkMem, CLK_PWM_CTL);
      CLK_UART:  pCtl:= SetPtr(PClkMem, CLK_UART_CTL);
      CLK_PCM:   pCtl:= SetPtr(PClkMem, CLK_PCM_CTL);
      else Exit(False);
    end;

    pDivI:= SetPtr(pCtl, 4);     // Clock Divisor

    // If Freq = 0, then stop the Clock
    if Freq = 0 then
    begin
      pCtl^:= (pCtl^ and $0000000F) or BCM_PASSWORD;   // Stop Clock (b4=0)
      while (pCtl^ and $00000080) <> 0 do              // Wait for clock to be not BUSY
        DelayMicroSeconds(10);

      pCtl^:= BCM_PASSWORD;                            // Set source to GND
      while (pCtl^ and $00000080) <> 0 do              // Wait for clock to be not BUSY
        DelayMicroSeconds(10);

      Exit(True);
    end;

    // Calculate Freq. Pi1-3 and Pi4 are different
    ClkSrc:= CLK_SRC_OSC;

    if FRPiModel.Cpu = PI_CPU_BCM2711
      then PiFreq:= CLK_OSC_FREQ_2711   // Pi 4.   13.186 Hz to 27 MHz
      else PiFreq:= CLK_OSC_FREQ;       // Pi 1-3.  4.688 Hz to 9.6 MHz

    // If we are going fast, use PLLD source clock to avoid jitter on the clock
    if Freq > LongInt(PiFreq div 10) then
    begin
      ClkSrc:= CLK_SRC_PLLD;

      if FRPiModel.Cpu = PI_CPU_BCM2711
        then PiFreq:= CLK_PLLD_FREQ_2711   // Pi 4.   183.150 Hz to 375 MHz
        else PiFreq:= CLK_PLLD_FREQ;       // Pi 1-3. 122.100 Hz to 250 MHz
    end;

    // Calculate Divisor and Fraction
    // From the manual: Freq:= Source / (DIVI + DIVF / 1024??) in MASH-1 mode
    // It seems to be: Freq:= Source / (DIVI + DIVF / 4096) in MASH-1 mode
    DivI:= PiFreq div LongWord(Freq);
    DivF:= Round(((PiFreq / Freq) - DivI) * 4096);  // Round up here
    if DivF > 4095 then DivF:= 4095;

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

    pCtl^:= (pCtl^ and $0000000F) or BCM_PASSWORD;   // Stop Clock  (b4 = 0)

    while (pCtl^ and $00000080) <> 0 do              // Wait for clock to be not BUSY
      DelayMicroSeconds(10);

    pDivI^:= BCM_PASSWORD or (DivI shl 12) or DivF;   // Set Divisor and Fraction

    pCtl^:= BCM_PASSWORD or CLK_CTL_MASH1 or ClkSrc; // Set Source and MASH1

    while (pCtl^ and $00000080) <> 0 do              // Wait for clock to be not BUSY
      DelayMicroSeconds(10);

    pCtl^:= pCtl^ or BCM_PASSWORD or (1 shl 4);      // Start Clock  (b4 = 1)
  end;


  // RaspberryPi 5
  // We are missing documentation for RP1 clocks, so this is guessing and trying....
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    Result:= True;

    case ClkNo of
      CLK_GPIO0: pCtl:= SetPtr(PClkMem, RP1_CLK_GP0_CTRL);
      CLK_GPIO1: pCtl:= SetPtr(PClkMem, RP1_CLK_GP1_CTRL);
      CLK_GPIO2: pCtl:= SetPtr(PClkMem, RP1_CLK_GP2_CTRL);
      // We also have CLK_GPIO3,5,6 but don't set it....
      CLK_PWM:   pCtl:= SetPtr(PClkMem, RP1_CLK_PWM_CTRL);
      CLK_UART:  pCtl:= SetPtr(PClkMem, RP1_CLK_UART_CTRL);
      else Exit(False);
    end;

    pDivI:= SetPtr(pCtl, 4);                     // Clock Divisor Int
    pDivF:= SetPtr(pCtl, 8);                     // Clock Divisor Frac
    pSel:=  SetPtr(pCtl, 12);                    // Clock Select ???
    pOE:=   SetPtr(PClkMem, RP1_GPCLK_OE_CTRL);  // Output Enable

    Rp1ClkDef:= RP1_ClockDefs[ClkNo];            // Load Defs

    // If Freq = 0, then stop the Clock
    if Freq = 0 then
    begin
      pCtl^:= pCtl^ and (not RP1_CLK_CTRL_ENABLE);

      // If this is a GPCLK, turn off the output-enable
      if Rp1ClkDef.OEmask <> 0 then pOE^:= pOE^ and (not Rp1ClkDef.OEmask);
      Exit(True);
    end;

    // Calculate Freq.
    ClkSrc:= Rp1ClkDef.SrcLo;
    PiFreq:= Rp1ClkDef.FreqLo;         // Pi 5.   763 Hz to 50 MHz
    MinDiv:= 1;

    // If we are going fast, use High source clock to avoid jitter on the clock
{   >>>>> Fast mode don't work proberly, so don't use it for now <<<<<
    >>>>> Still missing documentation                            <<<<<
    if Freq > (PiFreq div 10) then
    begin
      ClkSrc:= Rp1ClkDef.SrcHi;
      PiFreq:= Rp1ClkDef.FreqHi;       // Pi 5.   3052 Hz to 20 MHz
      MinDiv:= Rp1ClkDef.MinDivI;
    end;
}
    // Calculate Divisor and Fraction
    // Fraction don't semms to work!!!!
    DivI:= PiFreq div LongWord(Freq);
    DivF:= Round(((PiFreq / Freq) - DivI) * (Rp1ClkDef.MaxDivI+1));  // Round up here
    if DivF > Rp1ClkDef.MaxDivI+1 then DivF:= Rp1ClkDef.MaxDivI;

    if DivI < MinDiv then
    begin
      FLastErrorStr:= 'Requested frequency too high, max. freq = ' + IntToStr(PiFreq div MinDiv);
      Exit(False);
    end;

    if (DivI >  Rp1ClkDef.MaxDivI) then
    begin
      FLastErrorStr:= 'Requested frequency too low, lowest freq = ' +
                      IntToStr(QWord(PiFreq div Rp1ClkDef.MaxDivI)+1);
      Exit(False);
    end;

    if (DivF > $FFFF) then
    begin                   // we should never come here!
      DivF:= $FFFF;
      FLastErrorStr:= 'Clock Fraction Overflow';
      Result:= False;
    end;

    pCtl^:= pCtl^ and (not RP1_CLK_CTRL_ENABLE);     // Stop Clock
    while (pCtl^ and RP1_CLK_CTRL_BUSY) <> 0 do      // Wait for clock to be not BUSY
      DelayMicroSeconds(10);

    pDivI^:= DivI;                                   // Set Divisor and Fraction
    pDivF^:= DivF;                                   // Are DivF working?

    pCtl^:= pCtl^ and (not RP1_CLK_CTRL_SRCMASK) or (ClkSrc shl 5); // Set Source

    pCtl^:= pCtl^ or RP1_CLK_CTRL_ENABLE;             // Start Clock
    DelayMicroSeconds(10);

    // If this is a GPCLK, turn on the output-enable
    if Rp1ClkDef.OEmask <> 0 then pOE^:= pOE^ or Rp1ClkDef.OEmask;
  end;
end;

// ------------------------------------------------------------------------

function TPiGpio.GetRawClockData(ClkNo: Integer; var Data: TGpioClk): Boolean;
var
  pCtl,pDivI,pDivF: ^LongWord;

begin
  if not IsCpuOk then Exit(False);
  if UsingGpioMem then Exit(False);
  Result:= False;

  // RaspberryPi 1 to RaspberryPi 4
  if FRPiModel.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
  begin
    case ClkNo of
      CLK_GPIO0: pCtl:= SetPtr(PClkMem, CLK_GP0_CTL);
      CLK_GPIO1: pCtl:= SetPtr(PClkMem, CLK_GP1_CTL);
      CLK_GPIO2: pCtl:= SetPtr(PClkMem, CLK_GP2_CTL);
      CLK_PWM:   pCtl:= SetPtr(PClkMem, CLK_PWM_CTL);
      CLK_UART:  pCtl:= SetPtr(PClkMem, CLK_UART_CTL);
      CLK_PCM:   pCtl:= SetPtr(PClkMem, CLK_PCM_CTL);
      else Exit;
    end;

    pDivI:= SetPtr(pCtl, 4);                     // Clock Divisor Int

    Data.Control:=  pCtl^;
    Data.Divisor:= (pDivI^ shr 12) and $FFF;
    Data.Fract:=   (pDivI^ and $FFF);
    Result:= True;
  end;

  // RaspberryPi 5
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    case ClkNo of
      CLK_GPIO0: pCtl:= SetPtr(PClkMem, RP1_CLK_GP0_CTRL);
      CLK_GPIO1: pCtl:= SetPtr(PClkMem, RP1_CLK_GP1_CTRL);
      CLK_GPIO2: pCtl:= SetPtr(PClkMem, RP1_CLK_GP2_CTRL);
      CLK_GPIO3: pCtl:= SetPtr(PClkMem, RP1_CLK_GP3_CTRL);
      CLK_GPIO4: pCtl:= SetPtr(PClkMem, RP1_CLK_GP4_CTRL);
      CLK_GPIO5: pCtl:= SetPtr(PClkMem, RP1_CLK_GP5_CTRL);
      CLK_PWM:   pCtl:= SetPtr(PClkMem, RP1_CLK_PWM_CTRL);
      CLK_UART:  pCtl:= SetPtr(PClkMem, RP1_CLK_UART_CTRL);
      else Exit;
    end;

    pDivI:= SetPtr(pCtl, 4);                     // Clock Divisor Int
    pDivF:= SetPtr(pCtl, 8);                     // Clock Divisor Frac

    Data.Control:= pCtl^;
    Data.Divisor:= pDivI^;
    Data.Fract:=   pDivF^;
    Result:= True;
  end;
end;

// ------------------------------------------------------------------------

// -----------------------------------------------
// Calculate Frequency of a clock
// Pi1-4: From the manual: Freq:= Source / (DIVI + DIVF / 1024)
// Pi1-4: This is dependent on MESH mode !!! Assumes MESH1 mode
// Pi5: Don't have dokumentation for RP1 chip!!!!
// -----------------------------------------------
function TPiGpio.GetClockFrequency(ClkNo: Integer): LongWord;
var
  ClkData: TGpioClk;
  PiFreq, Src: LongWord;
  Rp1ClkDef: TRp1Clk;

begin
  Result:= 0;
  if not GetRawClockData(ClkNo, ClkData{%H-}) then Exit;

  // RaspberryPi 1 to RaspberryPi 4
  if FRPiModel.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
  begin
    // Check for Clock running (b4=1)
    if ClkData.Control and (1 shl 4) = 0 then Exit;

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

    if ClkData.Divisor > 0 then
      Result:= Trunc((PiFreq / (ClkData.Divisor + (ClkData.Fract / 4096))) + 0.5);  // Round down
  end;

  // RaspberryPi 5
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    // Check for Clock running
    if ClkData.Control and RP1_CLK_CTRL_ENABLE = 0 then Exit;

    Rp1ClkDef:= RP1_ClockDefs[ClkNo];                         // Load Defs
    Src:= (ClkData.Control and RP1_CLK_CTRL_SRCMASK) shr 5;

    if Src = Rp1ClkDef.SrcLo then PiFreq:= Rp1ClkDef.FreqLo
    else
    if Src = Rp1ClkDef.SrcHi then PiFreq:= Rp1ClkDef.FreqHi
    else Exit;

    if ClkData.Divisor > 0 then
      Result:= Trunc((PiFreq / (ClkData.Divisor + (ClkData.Fract / (Rp1ClkDef.MaxDivI+1)))) + 0.5);  // Round down
  end;
end;



// ------------------------------------------------------------------------
//
// PWM STUFF
//
// ------------------------------------------------------------------------

// ----------------------------------------
// Internal function for set pointer to right PWM channel
// ----------------------------------------
function TPiGpio.GetPwmBasePtr(Gpin: Byte; Ofs: Word): Pointer;
var
  ChanTwoOfs: Word;

begin
  Result:= Nil;
  if not IsCpuOk then Exit;
  if UsingGpioMem then Exit;

  // RaspberryPi 1 to RaspberryPi 4
  if FRPiModel.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
  begin
    Case FRPiModel.Cpu of
      PI_CPU_BCM2835,
      PI_CPU_BCM2836,
      PI_CPU_BCM2837: ChanTwoOfs:= PWM0_OFFSET;   // Pi 1-3. Only one set of PWM channels
      PI_CPU_BCM2711: ChanTwoOfs:= PWM1_OFFSET;   // Pi 4. Two sets of PWM channels
    end;

    Case Gpin of
      12,13,18,19,45: Exit(SetPtr(PPwmMem, PWM0_OFFSET, Ofs));
      40,41:          Exit(SetPtr(PPwmMem, ChanTwoOfs, Ofs));
      else
      begin
        FLastErrorStr:= 'PWM only on GPIO 12,13,18,19,40,41,45';
        Exit;
      end;
    end;
  end;

  // RaspberryPi 5
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    Case Gpin of
      12,13,14,15,18,19: Exit(SetPtr(PPwmMem, Ofs));
      else
      begin
        FLastErrorStr:= 'PWM only on GPIO 12,13,14,15,18,19';
        Exit;
      end;
    end;
  end;
end;

// ------------------------------------------------------------------------

function TPiGpio.SetPwmMode(Gpin: Byte; Mode: Byte): Boolean;
var
  pPwm,pPwmChan: ^LongWord;
  PwmReg: Word;
  Mask: LongWord;

begin
  Result:= False;

  // RaspberryPi 1 to RaspberryPi 4
  if FRPiModel.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
  begin
    pPwm:= GetPwmBasePtr(Gpin, PWM_CONTROL);   // Get PWM0_x group / PWM1_x group pointer
    if pPwm = Nil then Exit;

    Case Gpin of
      12,18{,40}:
      begin
        Mask:= PWM0_ENABLE or PWM0_SERIAL or PWM0_REPEATFF or PWM0_SILENCE or
               PWM0_REVPOLAR or PWM0_USEFIFO or PWM0_MS_Mode;

        if (Mode = PWM_MODE_MS)
          then pPwm^:= (pPwm^ and (not Mask)) or PWM0_ENABLE or PWM0_MS_MODE
          else pPwm^:= (pPwm^ and (not Mask)) or PWM0_ENABLE;
      end;

      13,19{,41,45}:
      begin
        Mask:= PWM1_ENABLE or PWM1_SERIAL or PWM1_REPEATFF or PWM1_SILENCE or
               PWM1_REVPOLAR or PWM1_USEFIFO or PWM1_MS_Mode;

        if (Mode = PWM_MODE_MS)
          then pPwm^:= (pPwm^ and (not Mask)) or PWM1_ENABLE or PWM1_MS_MODE
          else pPwm^:= (pPwm^ and (not Mask)) or PWM1_ENABLE;
      end;

      else Exit(False);
    end;

    Result:= True;
  end;

  // RaspberryPi 5
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    if Mode = PWM_MODE_BAL then
    begin
      FLastErrorStr:= 'PWM Balanced Mode not supported';
      Exit;
    end;

    Case Gpin of
      12:    begin PwmReg:= RP1_PWM_CHAN0_CTRL; Mask:= $01; end;
      13:    begin PwmReg:= RP1_PWM_CHAN1_CTRL; Mask:= $02; end;
      14,18: begin PwmReg:= RP1_PWM_CHAN2_CTRL; Mask:= $04; end;
      15,19: begin PwmReg:= RP1_PWM_CHAN3_CTRL; Mask:= $08; end;
      else   begin PwmReg:= 0;                  Mask:= $00; end;
    end;

    pPwm:= GetPwmBasePtr(Gpin, RP1_PWM_GLOBAL_CTRL);
    if pPwm = Nil then Exit;

    pPwmChan:= GetPwmBasePtr(Gpin, PwmReg);
    if pPwmChan = Nil then Exit;

    if Mode = PWM_MODE_MS then
    begin
      pPwmChan^:= RP1_PWM_CHANCTRL_FIFO_POP or RP1_PWM_CHANCTRL_MODE_MS;
      pPwm^:= pPwm^ or Mask;
      pPwm^:= pPwm^ or RP1_PWM_GLOBCTRL_SET_UPDATE;
    end;

    if Mode = PWM_MODE_OFF then
    begin
      pPwmChan^:= RP1_PWM_CHANCTRL_FIFO_POP;
      pPwm^:= pPwm^ and (not Mask);
      pPwm^:= pPwm^ or RP1_PWM_GLOBCTRL_SET_UPDATE;
    end;

    Result:= True;
  end;
end;

// ------------------------------------------------------------------------

function TPiGpio.SetPwmRange(Gpin: Byte; Range: LongWord): Boolean;
var
  pPwm: ^LongWord;
  PwmReg: Word;

begin
  Result:= False;

  // RaspberryPi 1 to RaspberryPi 4
  if FRPiModel.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
  begin
    Case Gpin of
      12,18,40:    PwmReg:= PWM0_RANGE;
      13,19,41,45: PwmReg:= PWM1_RANGE;
      else         PwmReg:= 0;
    end;
  end;

  // RaspberryPi 5
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    Case Gpin of
      12:    PwmReg:= RP1_PWM_CHAN0_RANGE;
      13:    PwmReg:= RP1_PWM_CHAN1_RANGE;
      14,18: PwmReg:= RP1_PWM_CHAN2_RANGE;
      15,19: PwmReg:= RP1_PWM_CHAN3_RANGE;
      else   PwmReg:= 0;
    end;

    // On Pi5 the count starts at 0 and increments on each cycle until it reaches RANGE
    if Range > 1 then Range:= Range - 1;
  end;

  pPwm:= GetPwmBasePtr(Gpin, PwmReg);
  if pPwm = Nil then Exit;

  pPwm^:= Range;
  Result:= True;
end;

// ------------------------------------------------------------------------

function TPiGpio.SetPwmValue(Gpin: Byte; Value: Longword): Boolean;
var
  pPwm: ^LongWord;
  PwmReg: Word;

begin
  Result:= False;

  // RaspberryPi 1 to RaspberryPi 4
  if FRPiModel.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
  begin
    Case Gpin of
      12,18,40:    PwmReg:= PWM0_DATA;
      13,19,41,45: PwmReg:= PWM1_DATA;
      else         PwmReg:= 0;
    end;
  end;

  // RaspberryPi 5
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    Case Gpin of
      12:    PwmReg:= RP1_PWM_CHAN0_DUTY;
      13:    PwmReg:= RP1_PWM_CHAN1_DUTY;
      14,18: PwmReg:= RP1_PWM_CHAN2_DUTY;
      15,19: PwmReg:= RP1_PWM_CHAN3_DUTY;
      else   PwmReg:= 0;
    end;
  end;

  pPwm:= GetPwmBasePtr(Gpin, PwmReg);
  if pPwm = Nil then Exit;

  pPwm^:= Value;
  Result:= True;
end;

// ------------------------------------------------------------------------

function TPiGpio.SetPwmDutyCycle(Gpin: Byte; DutyCycle: Single): Boolean;
var
  pPwm: ^LongWord;
  PwmReg: Word;
  Value: LongWord;

begin
  Result:= False;

  // RaspberryPi 1 to RaspberryPi 4
  if FRPiModel.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
  begin
    Case Gpin of
      12,18,40:    PwmReg:= PWM0_RANGE;
      13,19,41,45: PwmReg:= PWM1_RANGE;
      else         PwmReg:= 0;
    end;
  end;

  // RaspberryPi 5
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    Case Gpin of
      12:    PwmReg:= RP1_PWM_CHAN0_RANGE;
      13:    PwmReg:= RP1_PWM_CHAN1_RANGE;
      14,18: PwmReg:= RP1_PWM_CHAN2_RANGE;
      15,19: PwmReg:= RP1_PWM_CHAN3_RANGE;
      else   PwmReg:= 0;
    end;
  end;

  pPwm:= GetPwmBasePtr(Gpin, PwmReg);
  if pPwm = Nil then Exit;
  if pPwm^ = 0 then
  begin
    FLastErrorStr:= 'Range has to be set first';
    Exit;
  end;

  Value:= Trunc((pPwm^ * DutyCycle) / 100);

  // On Pi5 the count starts at 0 and increments on each cycle until it reaches RANGE
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    if (Value > 0) and (Value < $FFFFFFFF) then
      Value:= Value + 1;
  end;

  Result:= SetPwmValue(Gpin, Value);
end;

// ------------------------------------------------------------------------

function TPiGpio.GetRawPwmData(PwmGroup: Integer; var Data: TPwmData): Boolean;
var
  pPwm: ^LongWord;
  Ofs: LongWord;
  I: Integer;

begin
  Result:= False;

  if not IsCpuOk then Exit;
  if UsingGpioMem then Exit;

  FillChar(Data, SizeOf(Data), 0);
  Data.Cpu:= FRPiModel.Cpu;

  // RaspberryPi 1 to RaspberryPi 4
  if FRPiModel.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
  begin
    if (PwmGroup = PWM_GROUP_1) and             // Only two groups on Pi4
       (FRPiModel.Cpu <> PI_CPU_BCM2711) then
    begin
      FLastErrorStr:= 'PWM Group 1 not supported on this CPU';
      Exit;
    end;

    case PwmGroup of
      PWM_GROUP_0: Ofs:= PWM0_OFFSET;
      PWM_GROUP_1: Ofs:= PWM1_OFFSET;
      else Exit;
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

    Result:= True;
  end;

  // RaspberryPi 5
  if FRPiModel.Cpu = PI_CPU_BCM2712 then
  begin
    case PwmGroup of
      PWM_GROUP_0: Ofs:= RP1_PWM0_OFFSET;
      PWM_GROUP_1: Ofs:= RP1_PWM1_OFFSET;
      else Exit;
    end;

    pPwm:= SetPtr(PPwmMem, Ofs, RP1_PWM_GLOBAL_CTRL);
    Data.Rp1GlobCtrl:= pPwm^;

    pPwm:= SetPtr(PPwmMem, Ofs, RP1_PWM_FIFO_CTRL);
    Data.Rp1FifoCtrl:= pPwm^;

    pPwm:= SetPtr(PPwmMem, Ofs, RP1_PWM_COMMON_RANGE);
    Data.Rp1ComRange:= pPwm^;

    pPwm:= SetPtr(PPwmMem, Ofs, RP1_PWM_COMMON_DUTY);
    Data.Rp1ComDuty:= pPwm^;

    pPwm:= SetPtr(PPwmMem, Ofs, RP1_PWM_DUTY_FIFO);
    Data.Rp1DutyFifo:= pPwm^;

    pPwm:= SetPtr(PPwmMem, Ofs, RP1_PWM_CHAN0_CTRL);
    for I:= Low(Data.Rp1Channels) to High(Data.Rp1Channels) do
    begin
      Data.Rp1Channels[I].Rp1Control:= pPwm^; pPwm:= pPwm + 1;
      Data.Rp1Channels[I].Rp1Range:=   pPwm^; pPwm:= pPwm + 1;
      Data.Rp1Channels[I].Rp1Phase:=   pPwm^; pPwm:= pPwm + 1;
      Data.Rp1Channels[I].Rp1Duty:=    pPwm^; pPwm:= pPwm + 1;
      if (Data.Rp1Channels[I].Rp1Range > 0) and
         (Data.Rp1Channels[I].Rp1Range < $FFFFFFFF) then
        Data.Rp1Channels[I].Rp1Range:= Data.Rp1Channels[I].Rp1Range + 1;
    end;

    Result:= True;
  end;
end;

// ------------------------------------------------------------------------

function TPiGpio.GetGpiosForPwm(PwmChan: Integer): TIntArray;
var
  Gpio: Integer;
  Ok: Boolean;
  Data: TGpioPin;

begin
  Result:= [];
  if not IsCpuOk then Exit;

  for Gpio in [12,13,14,15,18,19,34,35,40,41,45,48] do
  begin
    Ok:= False;
    GetGpioPinData(Gpio, Data{%H-});

    Case FRPiModel.Cpu of
      PI_CPU_BCM2835,
      PI_CPU_BCM2836,
      PI_CPU_BCM2837:     // RaspberryPi 1 to RaspberryPi 3
      begin
        case PwmChan of
          PWM_CHANNEL_0_0:
            Ok:= ((Gpio = 12) and (Data.Mode = PM_ALT0)) or
                 ((Gpio = 18) and (Data.Mode = PM_ALT5)) or
                 ((Gpio = 40) and (Data.Mode = PM_ALT0));

          PWM_CHANNEL_0_1:
            Ok:= ((Gpio = 13) and (Data.Mode = PM_ALT0)) or
                 ((Gpio = 19) and (Data.Mode = PM_ALT5)) or
                 ((Gpio = 45) and (Data.Mode = PM_ALT0)) or
                 ((Gpio = 41) and (Data.Mode = PM_ALT0));
        end;
      end;

      PI_CPU_BCM2711:     // RaspberryPi 4
      begin
        case PwmChan of
          PWM_CHANNEL_0_0:
            Ok:= ((Gpio = 12) and (Data.Mode = PM_ALT0)) or
                 ((Gpio = 18) and (Data.Mode = PM_ALT5));

          PWM_CHANNEL_0_1:
            Ok:= ((Gpio = 13) and (Data.Mode = PM_ALT0)) or
                 ((Gpio = 19) and (Data.Mode = PM_ALT5)) or
                 ((Gpio = 45) and (Data.Mode = PM_ALT0));

          PWM_CHANNEL_1_0:
            Ok:= ((Gpio = 40) and (Data.Mode = PM_ALT0));

          PWM_CHANNEL_1_1:
            Ok:= ((Gpio = 41) and (Data.Mode = PM_ALT0));
        end;
      end;

      PI_CPU_BCM2712:     // RaspberryPi 5
      begin
        case PwmChan of
          PWM_CHANNEL_0_0:
            Ok:= ((Gpio = 12) and (Data.Mode = PM_ALT0));

          PWM_CHANNEL_0_1:
            Ok:= ((Gpio = 13) and (Data.Mode = PM_ALT0));

          PWM_CHANNEL_0_2:
            Ok:= ((Gpio = 14) and (Data.Mode = PM_ALT0)) or
                 ((Gpio = 18) and (Data.Mode = PM_ALT3));

          PWM_CHANNEL_0_3:
            Ok:= ((Gpio = 15) and (Data.Mode = PM_ALT0)) or
                 ((Gpio = 19) and (Data.Mode = PM_ALT3));

          PWM_CHANNEL_1_0:
            Ok:= ((Gpio = 35) and (Data.Mode = PM_ALT1)) or
                 ((Gpio = 44) and (Data.Mode = PM_ALT2)) or
                 ((Gpio = 48) and (Data.Mode = PM_ALT0));

          PWM_CHANNEL_1_1:
            Ok:= ((Gpio = 40) and (Data.Mode = PM_ALT0));

          PWM_CHANNEL_1_2:
            Ok:= ((Gpio = 34) and (Data.Mode = PM_ALT0)) or
                 ((Gpio = 41) and (Data.Mode = PM_ALT0));

          PWM_CHANNEL_1_3:
            Ok:= ((Gpio = 45) and (Data.Mode = PM_ALT0));
        end;
      end;
    end;

    If Ok then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result)-1]:= Gpio;
    end;
  end;
end;



end.
