unit GpioDefs;

// ------------------------------------------------------------------------
//
// GPIO for Raspberry Pi 1 to 4
// ----------------------------
//
// All the definitions for Pi's memory and
// definitions used in Gpio4Pi calls
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// ------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;


// -----------------------------------------------------
//
// Definitions used in Gpio4Pi calls
//
// -----------------------------------------------------
const
  // SetPinMode() and GetGpioPinData(): Pin Modes
  PM_INPUT      =  0;    // ------------------------------------
  PM_OUTPUT     =  1;    // These constants must not be changed as they have
  PM_ALT5       =  2;    // the same value as the FSEL_xx constants.
  PM_ALT4       =  3;    // And Yes, PM_ALT0...PM_ALT5 IS meant to be defined like that.
  PM_ALT0       =  4;    //
  PM_ALT1       =  5;    //
  PM_ALT2       =  6;    // SetPinMode() and GetGpioPinData()
  PM_ALT3       =  7;    // ------------------------------------
  PM_PWMOUT_MS  =  8;    // Only SetPinMode(). Not returned by GetGpioPinData()
  PM_PWMOUT_BAL =  9;    // Only SetPinMode(). Not returned by GetGpioPinData()
  PM_GPIO_CLOCK = 10;    // Only SetPinMode(). Not returned by GetGpioPinData()

  // GpioRead() and GpioWrite(): Pin Levels
  PIN_LOW  = 0;
  PIN_HIGH = 1;

  // SetPullMode(): Pull Up/Down/None
  PUD_OFF  = 0;
  PUD_DOWN = 1;
  PUD_UP   = 2;

  // SetPwmMode(): PWM Modes
  PWM_MODE_BAL = 0;     // BCM Default, Balanced
  PWM_MODE_MS  = 1;     // Use Mark/Space for PWM

  // GetGpiosForGpioClock(): Find all the GPIOs that are assigned to a GpioClock
  // GetRawClockData():      Returns the Raw Control and Divisor for a clock
  // GetClockFrequency():    Calculate Frequency of a clock
  CLK_GPIO0 = 0;
  CLK_GPIO1 = 1;
  CLK_GPIO2 = 2;
  CLK_PCM   = 3;
  CLK_PWM   = 4;

  // GetGpiosForPwm(): Find all the GPIOs that are assigned to a PWM channel
  PWM_CHANNEL_0_0 = 0;
  PWM_CHANNEL_0_1 = 1;
  PWM_CHANNEL_1_0 = 2;    // Only Pi4 (BCM2711)
  PWM_CHANNEL_1_1 = 3;    // Only Pi4 (BCM2711)

  // GetRawPwmData(): Returns the Raw data for a PWM group with 2 channels
  PWM_GROUP_0 = 0;
  PWM_GROUP_1 = 1;

  // PI model types and version numbers
  PI_MODEL_A       = 0;
  PI_MODEL_B       = 1;
  PI_MODEL_AP      = 2;
  PI_MODEL_BP      = 3;
  PI_MODEL_2       = 4;
  PI_ALPHA         = 5;
  PI_MODEL_CM1     = 6;
  PI_MODEL_07      = 7;
  PI_MODEL_3       = 8;
  PI_MODEL_ZERO    = 9;
  PI_MODEL_CM3     = 10;
  PI_MODEL_ZERO_W  = 12;
  PI_MODEL_3BP     = 13;
  PI_MODEL_3AP     = 14;
  PI_MODEL_CM3P    = 16;
  PI_MODEL_4B      = 17;
  PI_MODEL_ZERO_2W = 18;
  PI_MODEL_400     = 19;
  PI_MODEL_CM4     = 20;
  PI_MODEL_CM4S    = 21;
  PI_MODEL_5       = 23;

  // PI model for Model A + B + AP + BP + CM
  PI_VERSION_1   = 0;
  PI_VERSION_1_1 = 1;
  PI_VERSION_1_2 = 2;
  PI_VERSION_2   = 3;

  // CPU's
  PI_CPU_BCM2835 = 0;
  PI_CPU_BCM2836 = 1;
  PI_CPU_BCM2837 = 2;
  PI_CPU_BCM2711 = 3;
  PI_CPU_BCM2712 = 4;

  // Manufacturer
  PI_MAKER_SONY    = 0;
  PI_MAKER_EGOMAN  = 1;
  PI_MAKER_EMBEST  = 2;
  PI_MAKER_SONYJAP = 3;
  PI_MAKER_EMBEST2 = 4;
  PI_MAKER_STADIUM = 5;

  // Memory Size
  PI_MEM_256  = 0;
  PI_MEM_512  = 1;
  PI_MEM_1024 = 2;
  PI_MEM_2048 = 3;
  PI_MEM_4096 = 4;
  PI_MEM_8192 = 5;


  PiModelNames: Array[0..23] of String =
  ( 'Model A',    //  0
    'Model B',    //  1
    'Model A+',   //  2
    'Model B+',   //  3
    'Pi 2',       //  4
    'Alpha',      //  5
    'CM',         //  6
    'Unknown07',  //  7
    'Pi 3',       //  8
    'Pi Zero',    //  9
    'CM3',        // 10
    'Unknown11',  // 11
    'Pi Zero-W',  // 12
    'Pi 3B+',     // 13
    'Pi 3A+',     // 14
    'Unknown15',  // 15
    'CM3+',       // 16
    'Pi 4B',      // 17
    'Pi Zero2-W', // 18
    'Pi 400',     // 19
    'CM4',        // 20
    'CM4S',       // 21
    'Unknown22',  // 22
    'Pi 5');      // 23

  PiProcessor: Array[0..4] of String =
  ( 'BCM2835',
    'BCM2836',
    'BCM2837',
    'BCM2711',
    'BCM2712');

  PiRevisionNames: Array[0..15] of String =
  ( '00','01','02','03','04','05','06','07',
    '08','09','10','11','12','13','14','15');

  PiMakerNames: Array[0..15] of String =
  ( 'Sony',       //  0
    'Egoman',     //  1
    'Embest',     //  2
    'Sony Japan', //  3
    'Embest',     //  4
    'Stadium',    //  5
    'Unknown06',  //  6
    'Unknown07',  //  7
    'Unknown08',  //  8
    'Unknown09',  //  9
    'Unknown10',  // 10
    'Unknown11',  // 11
    'Unknown12',  // 12
    'Unknown13',  // 13
    'Unknown14',  // 14
    'Unknown15'); // 15

  PiMemorySize: Array[0..7] of integer =
  (  256,  // 0
     512,  // 1
    1024,  // 2
    2048,  // 3
    4096,  // 4
    8192,  // 5
       0,  // 6
       0); // 7



// -----------------------------------------------------
//
// Definitions for Raspberry Pi's CPU memory map
//
// -----------------------------------------------------
const
  // fpMap uses page offset, one page is 4096 Bytes = hex $1000
  // so simply calculate $20.000.000 / $1000 = $20.000
  PAGE_SIZE  = 4096;
  BLOCK_SIZE = 4096;

  // Memory addresses Pi 1 to 4
  GPIO_PERI_BASE_2835 = $20000;    // Pi 1
  GPIO_PERI_BASE_2836 = $3F000;    // Pi 2,3
  GPIO_PERI_BASE_2711 = $FE000;    // Pi 4
  GPIO_PERI_BASE_2712 = $00;   // Pi 5: Unknown. 32-bit mapped global mem access not supported for now

  // Offsets into the memory interface, also div $1000
  CLOCK_BASE = $101;              // $101000
  GPIO_BASE  = $200;              // $200000
  GPIO_PWM   = $20C;              // $20C000


  // -----------------------------------------------
  // GPIO section
  // -----------------------------------------------

  // GPIO function select bits
  FSEL_INPUT  = %000;
  FSEL_OUTPUT = %001;
  FSEL_ALT0   = %100;
  FSEL_ALT1   = %101;
  FSEL_ALT2   = %110;
  FSEL_ALT3   = %111;
  FSEL_ALT4   = %011;
  FSEL_ALT5   = %010;

  // GPIO Registers
  GPFSEL0 = $00;    // GPIO Function Select
  GPFSEL1 = $04;
  GPFSEL2 = $08;
  GPFSEL3 = $0C;
  GPFSEL4 = $10;
  GPFSEL5 = $14;
  GPSET0  = $1C;    // GPIO Pin Output Set
  GPSET1  = $20;
  GPCLR0  = $28;    // GPIO Pin Output Clear
  GPCLR1  = $2C;
  GPLEV0  = $34;    // GPIO Pin Level
  GPLEV1  = $38;
  GPEDS0  = $40;    // GPIO Pin Event Detect Status
  GPEDS1  = $44;
  GPREN0  = $4C;    // GPIO Pin Rising Edge Detect Enable
  GPREN1  = $50;
  GPFEN0  = $58;    // GPIO Pin Falling Edge Detect Enable
  GPFEN1  = $5C;
  GPHEN0  = $64;    // GPIO Pin High Detect Enable
  GPHEN1  = $68;
  GPLEN0  = $70;    // GPIO Pin Low Detect Enable
  GPLEN1  = $74;
  GPAREN0 = $7C;    // GPIO Pin Async. Rising Edge Detect 0
  GPAREN1 = $80;
  GPAFEN0 = $88;    // GPIO Pin Async. Falling Edge Detect 0
  GPAFEN1 = $8C;

  // Pull Up/Down BCM2835 (Pi 1-3)
  GPPUD     = $94;
  GPPUDCLK0 = $98;
  GPPUDCLK1 = $9C;

  //  Pull Up/Down BCM2711 (Pi 4)
  GPPUPPDN0 = $E4;
  GPPUPPDN1 = $E8;
  GPPUPPDN2 = $EC;
  GPPUPPDN3 = $F0;

  // -----------------------------------------------
  // Clock section
  // -----------------------------------------------

  // Clock Registers
  CLK_GP0_CTL = $70;    // GPIO clocks
  CLK_GP0_DIV = $74;
  CLK_GP1_CTL = $78;
  CLK_GP1_DIV = $7C;
  CLK_GP2_CTL = $80;
  CLK_GP2_DIV = $84;
  CLK_PCM_CTL = $98;    // PCM clock
  CLK_PCM_DIV = $9C;
  CLK_PWM_CTL = $A0;    // PWM clock
  CLK_PWM_DIV = $A4;

  // MASH modes
  CLK_CTL_MASH1 = 1 shl 9;
  CLK_CTL_MASH2 = 2 shl 9;
  CLK_CTL_MASH3 = 3 shl 9;

  // Clock sources
  CLK_SRC_OSC  = 1;
  CLK_SRC_PLLD = 6;

  CLK_OSC_FREQ       =  19200000;   // 19,2 Mhz  (Pi 1-3)
  CLK_OSC_FREQ_2711  =  54000000;   // 54 Mhz    (Pi 4)
  CLK_PLLD_FREQ      = 500000000;   // 500 MHz
  CLK_PLLD_FREQ_2711 = 750000000;   // 750 MHz

  BCM_PASSWORD = $5A000000;

  // -----------------------------------------------
  // PWM section
  // -----------------------------------------------

  // PWM Registers
  PWM0_OFFSET = $0000;  // The PWM0 register base address is 0x7e20c000 and
  PWM1_OFFSET = $0800;  // the PWM1 register base address is 0x7e20c800

  PWM_CONTROL = $00;
  PWM_STATUS  = $04;
  PWM_DMACTL  = $08;
  PWM0_RANGE  = $10;
  PWM0_DATA   = $14;
  PWM_FIFO    = $18;
  PWM1_RANGE  = $20;
  PWM1_DATA   = $24;

  // PWM control bits
  PWM0_MS_MODE  = $0080;  // Run in MS Mode
  PWM_CLRFIFO   = $0040;  // Clear FIFO
  PWM0_USEFIFO  = $0020;  // Data from FIFO
  PWM0_REVPOLAR = $0010;  // Reverse polarity
  PWM0_SILENCE  = $0008;  // The state of the output when no transmission takes place
  PWM0_REPEATFF = $0004;  // Repeat last Value if FIFO empty
  PWM0_SERIAL   = $0002;  // Run in serial Mode
  PWM0_ENABLE   = $0001;  // Channel Enable

  PWM1_MS_MODE  = $8000;  // Run in MS Mode
  PWM1_USEFIFO  = $2000;  // Data from FIFO
  PWM1_REVPOLAR = $1000;  // Reverse polarity
  PWM1_SILENCE  = $0800;  // The state of the output when no transmission takes place
  PWM1_REPEATFF = $0400;  // Repeat last Value if FIFO empty
  PWM1_SERIAL   = $0200;  // Run in serial Mode
  PWM1_ENABLE   = $0100;  // Channel Enable



// -----------------------------------------------
// Convert pin number from Phys to GPIO pin number
// FrType,ToType: RPI_PIN_GPIO, RPI_PIN_PHYS
// -----------------------------------------------
const
  RPI_PIN_GPIO = 1;  // pin are GPIO Pins
  RPI_PIN_PHYS = 2;  // pin Physic Pins

function ConvertPinFromTo(FrType,ToType: Byte; Pin: Integer): Integer;



implementation


const
// physToGpio:
// Take a physical pin (1 through 40) and re-map it to the BCM_GPIO pin
PhysToGpio: Array[0..63] Of Integer =
(
  -1,		// 0
  -1, -1,	// 1, 2
   2, -1,
   3, -1,
   4, 14,
  -1, 15,
  17, 18,
  27, -1,
  22, 23,
  -1, 24,
  10, -1,
   9, 25,
  11,  8,
  -1,  7,	// 25, 26

// B+

   0,  1,       // 27, 28
   5, -1,
   6, 12,
  13, -1,
  19, 16,
  26, 20,
  -1, 21,       // 39, 40

// the P5 connector on the Rev 2 boards: NO, Not here

  -1, -1,
  -1, -1,
  -1, -1,
  -1, -1,
  -1, -1,
  -1, -1,
  -1, -1,
  -1, -1,
  -1, -1,
  -1, -1,
  -1, -1,
  -1
);

// -------------------------------------------------------------------------------



// -----------------------------------------------
// Convert pin number from Phys to GPIO pin number
// FrType,ToType: RPI_PIN_GPIO, RPI_PIN_PHYS
// -----------------------------------------------
function ConvertPinFromTo(FrType,ToType: Byte; Pin: Integer): Integer;
var
  GP,I: Integer;

begin
  // Are From and To the same?
  if FrType = ToType then Exit(Pin);

  // First convert From FrType to GPIO pin
  case FrType of
    RPI_PIN_GPIO:     // From are GPIO Pins
    begin
      GP:= Pin;
    end;

    RPI_PIN_PHYS:     // From are Physic Pins
    begin
      if not (Pin in [Low(PhysToGpio)..High(PhysToGpio)]) then Exit(-1);
      GP:= PhysToGpio[Pin];
    end;

    else Exit(-1);
  end;

  // Then convert From GPIO pin to ToType
  case ToType of
    RPI_PIN_GPIO:     // ToPin should GPIO Pins
    begin
      Exit(GP);
    end;

    RPI_PIN_PHYS:    // ToPin should Physic Pins
    begin
      // Find GPIO pin and convert to Physic Pins
      if (GP < 0) or (GP >= High(PhysToGpio)) then Exit(-1);
      for I:= Low(PhysToGpio) to High(PhysToGpio) do
        if PhysToGpio[I] = GP then Exit(I);

      Exit(-1);       // Not found
    end;

    else Exit(-1);
  end;
end;



end.

