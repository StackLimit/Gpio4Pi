unit GpioDefs;

// ------------------------------------------------------------------------
//
// GPIO for Raspberry Pi 1 to 5
// ----------------------------
//
// All the definitions for Pi's memory and
// definitions used in Gpio4Pi calls
//
// Still under development and therefore not quite finished
// Copyright (c) 2024-2025 Jan Andersen
// ------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TIntArray = Array of Integer;

// -----------------------------------------------------
//
// Definitions used in Gpio4Pi calls
//
// -----------------------------------------------------
const
  // SetPinMode() and GetGpioPinData(): Pin Modes
  PM_INPUT      =  $00;   // ------------------------------------
  PM_OUTPUT     =  $01;   // These constants must not be changed as they have
  PM_ALT5       =  $02;   // the same value as the FSEL_xx constants.
  PM_ALT4       =  $03;   // And Yes, PM_ALT0...PM_ALT5 IS meant to be defined like that.
  PM_ALT0       =  $04;   //
  PM_ALT1       =  $05;   //
  PM_ALT2       =  $06;   //
  PM_ALT3       =  $07;   //
  PM_ALT6       =  $08;   // ALT6 to ALT 8 are for Pi5 (RP1)
  PM_ALT7       =  $09;   //
  PM_ALT8       =  $0A;   // SetPinMode() and GetGpioPinData()
  PM_GPIO_OFF   =  $1F;   // ------------------------------------
  PM_PWMOUT_MS  =  $20;   // Only SetPinMode(). Not returned by GetGpioPinData()
  PM_PWMOUT_BAL =  $21;   // Only SetPinMode(). Not returned by GetGpioPinData()
  PM_GPIO_CLOCK =  $22;   // Only SetPinMode(). Not returned by GetGpioPinData()

  // GpioRead() and GpioWrite(): Pin Levels
  PIN_LOW  = 0;
  PIN_HIGH = 1;

  // SetPullMode(): Pull Up/Down/None
  PUD_OFF  = 0;
  PUD_DOWN = 1;
  PUD_UP   = 2;

  // SetPwmMode(): PWM Modes
  PWM_MODE_OFF = 0;     // Turn PWM Off
  PWM_MODE_BAL = 1;     // BCM Default, Balanced
  PWM_MODE_MS  = 2;     // Use Mark/Space for PWM

  // GetGpiosForGpioClock(): Find all the GPIOs that are assigned to a GpioClock
  // GetRawClockData():      Returns the Raw Control and Divisor for a clock
  // GetClockFrequency():    Calculate Frequency of a clock
  CLK_GPIO0 = 0;
  CLK_GPIO1 = 1;
  CLK_GPIO2 = 2;
  CLK_GPIO3 = 3;      // Pi5
  CLK_GPIO4 = 4;      // Pi5
  CLK_GPIO5 = 5;      // Pi5
  CLK_PWM   = 6;
  CLK_UART  = 7;
  CLK_PCM   = 8;

  // GetGpiosForPwm(): Find all the GPIOs that are assigned to a PWM channel
  PWM_CHANNEL_0_0 = 0;    // Group 0 - Channel 0    Pi 1,2,3,4,5
  PWM_CHANNEL_0_1 = 1;    // Group 0 - Channel 1    Pi 1,2,3,4,5
  PWM_CHANNEL_0_2 = 2;    // Group 0 - Channel 2    Pi 5
  PWM_CHANNEL_0_3 = 3;    // Group 0 - Channel 3    Pi 5
  PWM_CHANNEL_1_0 = 4;    // Group 1 - Channel 0    Pi 4,5
  PWM_CHANNEL_1_1 = 5;    // Group 1 - Channel 1    Pi 4,5
  PWM_CHANNEL_1_2 = 6;    // Group 1 - Channel 2    Pi 5
  PWM_CHANNEL_1_3 = 7;    // Group 1 - Channel 3    Pi 5

  // GetRawPwmData(): Returns the Raw data for a PWM group with 2/4 channels
  PWM_GROUP_0 = 0;        // Group 0    Pi 1,2,3,4,5
  PWM_GROUP_1 = 1;        // Group 1    Pi 4,5


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
  PI_MODEL_CM5     = 24;
  PI_MODEL_500     = 25;
  PI_MODEL_CM5L    = 26;

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


  PiModelNames: Array[0..26] of String =
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
    'Pi 5',       // 23
    'Pi CM5',     // 24
    'Pi 500',     // 25
    'Pi CM5L');   // 26

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
// 32 and 64 bit address space
//
// -----------------------------------------------------
const
  PAGE_SIZE          = 4096;      // Pi1 to Pi4
  PAGE_SIZE_RP1_MEM  = $400000;   // Pi5 /dev/mem
  PAGE_SIZE_RP1_GPIO = $30000;    // Pi5 /dev/gpiomem0

{$ifdef CPU32}
  // 32 BIT address space
  // Memory addresses Pi 1 to 5
  GPIO_PERI_BASE_2835 = $20000;    // Pi 1
  GPIO_PERI_BASE_2836 = $3F000;    // Pi 2,3
  GPIO_PERI_BASE_2711 = $FE000;    // Pi 4
  GPIO_PERI_BASE_2712 = $40000;    // Pi 5 - Not Yet

  // Pi1-Pi4: Offsets into the memory interface, div $1000
  SYST_BASE  = $003;     // System Timer peripheral
  CLOCK_BASE = $101;     // General Purpose clocks
  GPIO_BASE  = $200;     // General Purpose Input/Output (GPIO)
  UART_BASE  = $201;     // UART (Universal Asynchronous Receiver/Transmitter). UART0, UART2, UART3, UART4 & UART5
  PCM_BASE   = $203;     // PCM (Pulse Code Modulation) audio interface
  SPI_BASE   = $204;     // Serial Peripheral Interface (SPI) or Synchronous Serial Protocol (SSP)
  PWM_BASE   = $20C;     // Pulse Width Modulator (PWM) peripherals
  AUX_BASE   = $215;     // Auxiliary peripherals: One mini UART (UART1) and two SPI masters (SPI1 & SPI2)
{$endif}

{$ifdef CPU64}
  // 64 BIT address space
  // Memory addresses Pi 1 to 5
  GPIO_PERI_BASE_2835 = $0020000000;    // Pi 1
  GPIO_PERI_BASE_2836 = $003F000000;    // Pi 2,3
  GPIO_PERI_BASE_2711 = $00FE000000;    // Pi 4
  GPIO_PERI_BASE_2712 = $1F00000000;    // Pi 5

  // Pi1-Pi4: Offsets into the memory interface
  SYST_BASE  = $003000;     // System Timer peripheral
  CLOCK_BASE = $101000;     // General Purpose clocks
  GPIO_BASE  = $200000;     // General Purpose Input/Output (GPIO)
  UART_BASE  = $201000;     // UART (Universal Asynchronous Receiver/Transmitter). UART0, UART2, UART3, UART4 & UART5
  PCM_BASE   = $203000;     // PCM (Pulse Code Modulation) audio interface
  SPI_BASE   = $204000;     // Serial Peripheral Interface (SPI) or Synchronous Serial Protocol (SSP)
  PWM_BASE   = $20C000;     // Pulse Width Modulator (PWM) peripherals
  AUX_BASE   = $215000;     // Auxiliary peripherals: One mini UART (UART1) and two SPI masters (SPI1 & SPI2)

  // Pi5: RP1 chip address and Offsets
  RP1_CLOCK_BASE = $018000;
  RP1_UART_BASE  = $030000;
  RP1_PWM_BASE   = $098000;   // PWM 0 Block
  RP1_GPIO_BASE  = $0D0000;   // GPIO 0 to 27   On Pin Header
  RP1_GPIO1_OFFS = $004000;   // GPIO 28 to 33  (RP1_GPIO_BASE + RP1_GPIO1_OFFS)
  RP1_GPIO2_OFFS = $008000;   // GPIO 34 to 53  (RP1_GPIO_BASE + RP1_GPIO2_OFFS)
  RP1_RIO_BASE   = $0E0000;   // RIO 1/2 have same offsets as GPIO
  RP1_PADS_BASE  = $0F0000;   // PADS 1/2 have same offsets as GPIO
{$endif}


  // -----------------------------------------------
  // GPIO section
  // -----------------------------------------------

  // Pi1-Pi4: GPIO function select bits
  FSEL_INPUT  = $00;
  FSEL_OUTPUT = $01;
  FSEL_ALT5   = $02;
  FSEL_ALT4   = $03;
  FSEL_ALT0   = $04;
  FSEL_ALT1   = $05;
  FSEL_ALT2   = $06;
  FSEL_ALT3   = $07;

  // Pi1-Pi4: GPIO Registers
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

  // Pi1-Pi3: Pull Up/Down BCM2835
  GPPUD     = $94;
  GPPUDCLK0 = $98;
  GPPUDCLK1 = $9C;

  // Pi4: Pull Up/Down BCM2711
  GPPUPPDN0 = $E4;
  GPPUPPDN1 = $E8;
  GPPUPPDN2 = $EC;
  GPPUPPDN3 = $F0;

  // Pi5: RP1 PAD Offsets and registers
  RP1_PAD_VOLTAGE_SELECT = $0000;  // First register are VOLTAGE_SELECT which sets
                                   // the voltage to all of the GPIO lines in bank 0.
                                   // A zero sets 3.3V and a one sets 1.8V.
                                   // The default is 3.3V
  RP1_PAD_PADS_OFFS      = $0004;  // PAD registers for each GPIO line

  // Pi5: RP1 PAD register
  RP1_PAD_OD        = %10000000;  // Output disable
  RP1_PAD_IE        = %01000000;  // Input Enable
  RP1_PAD_DRV_MASK  = %00110000;  // Drive strength mask
  RP1_PAD_DRV_2MA   = %00000000;  // Drive 2 mA
  RP1_PAD_DRV_4MA   = %00010000;  // Drive 4 mA
  RP1_PAD_DRV_8MA   = %00100000;  // Drive 8 mA
  RP1_PAD_DRV_12MA  = %00110000;  // Drive 12 mA
  RP1_PAD_PULL_MASK = %00001100;  // Pull-up/down mask
  RP1_PAD_PUE       = %00001000;  // Pull-up enable
  RP1_PAD_PDE       = %00000100;  // Pull-down enable
  RP1_PAD_SCHMITT   = %00000010;  // Enable Schmitt trigger
  RP1_PAD_SLEWFAST  = %00000001;  // Slew rate control. 1 = Fast, 0 = Slow

  // Pi5: RP1 GPIO Offsets and Registers
  RP1_GPIO_STATUS = $0000;
  RP1_GPIO_CTRL   = $0004;

  RP1_GPIO_FILTER    = $04 shl 5;  // Default Filter/debounce time constant
  RP1_GPIO_FSEL_GPIO = 5;          // Alt5 = SYS_RIO. Drive GPIO via RIO.
  RP1_GPIO_FSEL_NONE = $1F;        // Default / mask

  // Pi5: RP1 RIO Offsets and Registers
  RP1_RIO_NORMAL_OFFS = $0000;  // Normal read/write access
  RP1_RIO_XOR_OFFS    = $1000;  // Atomic XOR on write and reads have no side effects
  RP1_RIO_SET_OFFS    = $2000;  // Atomic SET on write and normal read access
  RP1_RIO_CLR_OFFS    = $3000;  // Atomic CLR on write and normal read access

  RP1_RIO_OUT    = $00;    // Sets GPIO lines to high or low
  RP1_RIO_OE     = $04;    // Sets GPIO line to output driver or high impedance
  RP1_RIO_IN     = $08;    // Reads GPIO lines
  RP1_RIO_INSYNC = $0C;    // Reads GPIO lines synchronized to clk_sys


  // -----------------------------------------------
  // Clock section
  // -----------------------------------------------

  // Pi1-Pi4: Clock Registers
  CLK_GP0_CTL  = $70;    // GPIO clocks
  CLK_GP0_DIV  = $74;
  CLK_GP1_CTL  = $78;
  CLK_GP1_DIV  = $7C;
  CLK_GP2_CTL  = $80;
  CLK_GP2_DIV  = $84;
  CLK_PCM_CTL  = $98;    // PCM clock
  CLK_PCM_DIV  = $9C;
  CLK_PWM_CTL  = $A0;    // PWM clock
  CLK_PWM_DIV  = $A4;
  CLK_UART_CTL = $F0;    // UART clock
  CLK_UART_DIV = $F4;

  // Pi1-Pi4: MASH modes
  CLK_CTL_MASH1 = 1 shl 9;
  CLK_CTL_MASH2 = 2 shl 9;
  CLK_CTL_MASH3 = 3 shl 9;

  // Pi1-Pi4: Password for clocks
  BCM_PASSWORD = $5A000000;

  // Pi1-Pi4: Clock sources and frequncy
  CLK_SRC_OSC  = 1;
  CLK_SRC_PLLD = 6;

  CLK_OSC_FREQ       =  19200000;   // 19,2 Mhz  (Pi 1-3)
  CLK_OSC_FREQ_2711  =  54000000;   // 54 Mhz    (Pi 4)
  CLK_PLLD_FREQ      = 500000000;   // 500 MHz
  CLK_PLLD_FREQ_2711 = 750000000;   // 750 MHz

  // Pi5: Clock Registers
  RP1_GPCLK_OE_CTRL     = $000;   // Output Enable register

  RP1_CLK_GP0_CTRL      = $174;   // GPIO clocks
  RP1_CLK_GP0_DIV_INT   = $178;
  RP1_CLK_GP0_DIV_FRAC  = $17C;
  RP1_CLK_GP0_SEL       = $180;
  RP1_CLK_GP1_CTRL      = $184;
  RP1_CLK_GP1_DIV_INT   = $188;
  RP1_CLK_GP1_DIV_FRAC  = $18C;
  RP1_CLK_GP1_SEL       = $190;
  RP1_CLK_GP2_CTRL      = $194;
  RP1_CLK_GP2_DIV_INT   = $198;
  RP1_CLK_GP2_DIV_FRAC  = $19C;
  RP1_CLK_GP2_SEL       = $1A0;
  RP1_CLK_GP3_CTRL      = $1A4;
  RP1_CLK_GP3_DIV_INT   = $1A8;
  RP1_CLK_GP3_DIV_FRAC  = $1AC;
  RP1_CLK_GP3_SEL       = $1B0;
  RP1_CLK_GP4_CTRL      = $1B4;
  RP1_CLK_GP4_DIV_INT   = $1B8;
  RP1_CLK_GP4_DIV_FRAC  = $1BC;
  RP1_CLK_GP4_SEL       = $1C0;
  RP1_CLK_GP5_CTRL      = $1C4;
  RP1_CLK_GP5_DIV_INT   = $1C8;
  RP1_CLK_GP5_DIV_FRAC  = $1CC;
  RP1_CLK_GP5_SEL       = $1D0;
  RP1_CLK_PWM_CTRL      = $074;   // PWM clock
  RP1_CLK_PWM_DIV_INT   = $078;
  RP1_CLK_PWM_DIV_FRAC  = $07C;
  RP1_CLK_PWM_SEL       = $080;
  RP1_CLK_UART_CTRL     = $054;   // UART clock
  RP1_CLK_UART_DIV_INT  = $058;
  RP1_CLK_UART_DIV_FRAC = $05C;   // ????
  RP1_CLK_UART_SEL      = $060;

  // Pi5: Clock fields for all clocks
  RP1_CLK_CTRL_ENABLE  = 1 shl 11;      // 0 = Stop Clock, 1 = Start Clock
  RP1_CLK_CTRL_SRCMASK = $000001E0;     // Mask for Source (4 bits)
  RP1_CLK_CTRL_BUSY    = $10000000;     // Clock busy bit?

  // Pi5: Clock sources and frequncy
type
  TRp1Clk = record
    SrcLo:   Integer;   // Source for Low freq.
    FreqLo:  LongWord;  // Low freq. in Hz
    SrcHi:   Integer;   // Source for High freq.
    FreqHi:  LongWord;  // High freq. in Hz
    MaxDivI: Integer;   // Max. I Divisor
    MinDivI: Integer;   // Min. I Divisor in high freq.
    OEmask:  LongInt;   // Mask for RP1_GPCLK_OE_CTRL
  end;
  TRp1Clks = Array[CLK_GPIO0..CLK_UART] of TRp1Clk;

const
  RP1_ClockDefs: TRp1Clks =
  (
   (SrcLo: $08; FreqLo:  50000000;      // CLK_GPIO0
    SrcHi: $06; FreqHi: 200000000;
    MaxDivI: $FFFF; MinDivI: 10; OEmask: $01),

   (SrcLo: $08; FreqLo:  50000000;      // CLK_GPIO1
    SrcHi: $06; FreqHi: 100000000;
    MaxDivI: $FFFF; MinDivI: 4; OEmask: $02),

   (SrcLo: $0C; FreqLo:  50000000;      // CLK_GPIO2
    SrcHi: $0F; FreqHi: 200000000;
    MaxDivI: $FFFF; MinDivI: 10; OEmask: $04),

   (SrcLo: $00; FreqLo:  50000000;      // CLK_GPIO3. We don't set this clock
    SrcHi: $00; FreqHi: 200000000;
    MaxDivI: $FFFF; MinDivI: 10; OEmask: $08),

   (SrcLo: $00; FreqLo:  50000000;      // CLK_GPIO4. We don't set this clock
    SrcHi: $00; FreqHi: 200000000;
    MaxDivI: $FFFF; MinDivI: 10; OEmask: $10),

   (SrcLo: $00; FreqLo:  50000000;      // CLK_GPIO5. We don't set this clock
    SrcHi: $00; FreqHi: 200000000;
    MaxDivI: $FFFF; MinDivI: 10; OEmask: $20),

   (SrcLo: $02; FreqLo: 50000000;       // CLK_PWM   Only 25MHz?
    SrcHi: $02; FreqHi: 50000000;       // And no High freq.?
    MaxDivI: $FFFF; MinDivI: 1; OEmask: $00),

   (SrcLo: $00; FreqLo:  50000000;      // CLK_UART
    SrcHi: $00; FreqHi: 200000000;
    MaxDivI: $FF; MinDivI: 10; OEmask: $00)
  );



  // -----------------------------------------------
  // PWM section
  // -----------------------------------------------

  // Pi1-Pi4: PWM Registers
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

  // Pi1-Pi4: PWM control bits
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

  // Pi5: PWM Registers
  RP1_PWM0_OFFSET = $0000;  // The PWM0 register base address is 0x40098000 and
  RP1_PWM1_OFFSET = $4000;  // the PWM1 register base address is 0x4009C000

  RP1_PWM_GLOBAL_CTRL  = $00;
  RP1_PWM_FIFO_CTRL    = $04;
  RP1_PWM_COMMON_RANGE = $08;
  RP1_PWM_COMMON_DUTY  = $0C;
  RP1_PWM_DUTY_FIFO    = $10;
  RP1_PWM_CHAN0_CTRL   = $14;
  RP1_PWM_CHAN0_RANGE  = $18;
  RP1_PWM_CHAN0_PHASE  = $1C;
  RP1_PWM_CHAN0_DUTY   = $20;
  RP1_PWM_CHAN1_CTRL   = $24;
  RP1_PWM_CHAN1_RANGE  = $28;
  RP1_PWM_CHAN1_PHASE  = $2C;
  RP1_PWM_CHAN1_DUTY   = $30;
  RP1_PWM_CHAN2_CTRL   = $34;
  RP1_PWM_CHAN2_RANGE  = $38;
  RP1_PWM_CHAN2_PHASE  = $3C;
  RP1_PWM_CHAN2_DUTY   = $40;
  RP1_PWM_CHAN3_CTRL   = $44;
  RP1_PWM_CHAN3_RANGE  = $48;
  RP1_PWM_CHAN3_PHASE  = $4C;
  RP1_PWM_CHAN3_DUTY   = $50;

  // Pi5: PWM Global Control bits
  RP1_PWM_GLOBCTRL_SET_UPDATE = $80000000;    // Bit 31
  RP1_PWM_GLOBCTRL_CHAN3_EN   = $08;
  RP1_PWM_GLOBCTRL_CHAN2_EN   = $04;
  RP1_PWM_GLOBCTRL_CHAN1_EN   = $02;
  RP1_PWM_GLOBCTRL_CHAN0_EN   = $01;

  // Pi5: PWM Channel 0-3 Control bits
  RP1_PWM_CHANCTRL_SM_BIAS      = $FFFF0000;
  RP1_PWM_CHANCTRL_SDM_BITWIDTH = $0000F000;
  RP1_PWM_CHANCTRL_FIFO_POP     = $00000100;
  RP1_PWM_CHANCTRL_DITHER       = $00000080;
  RP1_PWM_CHANCTRL_SDM          = $00000040;
  RP1_PWM_CHANCTRL_USEFIFO      = $00000020;
  RP1_PWM_CHANCTRL_BIND         = $00000010;
  RP1_PWM_CHANCTRL_INVERT       = $00000008;
  RP1_PWM_CHANCTRL_MODE_MASK    = $00000007;   // Mask for Mode
  RP1_PWM_CHANCTRL_MODE_MS      = $00000001;   // Trailing-edge mark-space PWM modulation


  // -----------------------------------------------
  // Uart section
  // -----------------------------------------------

  // Pi1-Pi4: Uart Registers
  UART0_OFFSET = $0000;
  UART2_OFFSET = $0400;
  UART3_OFFSET = $0600;
  UART4_OFFSET = $0800;
  UART5_OFFSET = $0A00;

  UART_DR     = $00;
  UART_RSRECR = $04;
  UART_FR     = $18;
  UART_IBRD   = $24;
  UART_FBRD   = $28;
  UART_LCRH   = $2C;
  UART_CR     = $30;
  UART_IFLS   = $34;
  UART_IMSC   = $38;
  UART_RIS    = $3C;
  UART_MIS    = $40;
  UART_ICR    = $44;
  UART_DMACR  = $48;
  UART_ITCR   = $80;
  UART_ITIP   = $84;
  UART_ITOP   = $88;
  UART_TDR    = $8C;

  // Pi1-Pi4: Uart control bits for UART_CR register
  UART_CR_CTSENA   = $8000;  // CTS hardware flow control enable
  UART_CR_RTSENA   = $4000;  // RTS hardware flow control enable
  UART_CR_RTS      = $0800;  // Request to send
  UART_CR_RXENABLE = $0200;  // Receive enable
  UART_CR_TXENABLE = $0100;  // Transmit enable
  UART_CR_LOOPBACK = $0080;  // Loopback enable
  UART_CR_ENABLE   = $0001;  // Uart Enable

  // Pi1-Pi4: Uart Line control bits for UART_LCRH register
  UART_LCRH_STICK  = $0080;  // Stick parity select
  UART_LCRH_XBITS  = $0060;  // Word length bits mask
  UART_LCRH_8BITS  = $0060;  // Word length 8 bits
  UART_LCRH_7BITS  = $0040;  // Word length 7 bits
  UART_LCRH_6BITS  = $0020;  // Word length 6 bits
  UART_LCRH_5BITS  = $0000;  // Word length 5 bits (bit 5 and 6)
  UART_LCRH_FIFO   = $0010;  // Enable FIFOs
  UART_LCRH_2STOP  = $0008;  // Two stop bits select
  UART_LCRH_EVEN   = $0004;  // Even parity select
  UART_LCRH_PARITY = $0002;  // Parity enable
  UART_LCRH_BREAK  = $0001;  // Send break

  // Pi1-Pi4: Uart Flag bits for UART_FR register
  UART_FR_TXFE = $0080;    // Transmit FIFO empty
  UART_FR_RXFF = $0040;    // Receive FIFO full
  UART_FR_TXFF = $0020;    // Transmit FIFO full
  UART_FR_RXFE = $0010;    // Receive FIFO empty
  UART_FR_BUSY = $0008;    // UART is busy transmitting data
  UART_FR_CTS  = $0001;    // Clear to send

  // Pi1-Pi4: Uart Error bits for Data Register UART-DR
  UART_DR_OE   = $0800;    // Overrun error
  UART_DR_BE   = $0400;    // Break error
  UART_DR_PE   = $0200;    // Parity error
  UART_DR_FE   = $0100;    // Framing error. Not a valid stop bit
  UART_DR_DATA = $00FF;    // RX/TX Data Mask



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

