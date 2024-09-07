unit GpioExtraStuff;

// -----------------------------------------------------------------
//
// Extra stuff for converting Mode, Alt, etc. to text
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// -----------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;


function GpioModeToShortStr(Mode: Integer): String;
function GpioModeToLongStr(Cpu, Gpio, Mode: Integer): String;
function GpioPullToShortStr(Pull: Integer): String;
function GpioPullToLongStr(Pull: Integer): String;
function GpioAltModeToStr(Cpu, Gpio, AltMode: Integer): String;


implementation

uses
  GpioDefs;


function GpioModeToShortStr(Mode: Integer): String;
begin
  case Mode of
    FSEL_INPUT:  Result:= 'In';
    FSEL_OUTPUT: Result:= 'Out';
    FSEL_ALT0:   Result:= 'Alt0';
    FSEL_ALT1:   Result:= 'Alt1';
    FSEL_ALT2:   Result:= 'Alt2';
    FSEL_ALT3:   Result:= 'Alt3';
    FSEL_ALT4:   Result:= 'Alt4';
    FSEL_ALT5:   Result:= 'Alt5';
    else         Result:= 'Undef';
  end;
end;

// ---------------------------------------------------------

function GpioModeToLongStr(Cpu, Gpio, Mode: Integer): String;
begin
  case Mode of
    FSEL_INPUT:  Result:= 'Input';
    FSEL_OUTPUT: Result:= 'Output';
    FSEL_ALT0:   Result:= 'Alt 0 = ' + GpioAltModeToStr(Cpu, Gpio, Mode);
    FSEL_ALT1:   Result:= 'Alt 1 = ' + GpioAltModeToStr(Cpu, Gpio, Mode);
    FSEL_ALT2:   Result:= 'Alt 2 = ' + GpioAltModeToStr(Cpu, Gpio, Mode);
    FSEL_ALT3:   Result:= 'Alt 3 = ' + GpioAltModeToStr(Cpu, Gpio, Mode);
    FSEL_ALT4:   Result:= 'Alt 4 = ' + GpioAltModeToStr(Cpu, Gpio, Mode);
    FSEL_ALT5:   Result:= 'Alt 5 = ' + GpioAltModeToStr(Cpu, Gpio, Mode);
    else         Result:= 'Undefined';
  end;
end;

// ---------------------------------------------------------

function GpioPullToShortStr(Pull: Integer): String;
begin
  case Pull of
    PUD_OFF:  Result:= ',NoPul';
    PUD_UP:   Result:= ',PulU';
    PUD_DOWN: Result:= ',PulD';
    else      Result:= ',Pull?';
  end;
end;

// ---------------------------------------------------------

function GpioPullToLongStr(Pull: Integer): String;
begin
  case Pull of
    PUD_OFF:  Result:= 'No PullUp/Down';
    PUD_UP:   Result:= 'PullUp';
    PUD_DOWN: Result:= 'PullDown';
    else      Result:= 'PullUp/Down Undefined';
  end;
end;

// ---------------------------------------------------------

const
  // ALT modes for Pi1-3 (BCM2835)
  GpioAltModePi1to3Def: Array[0..53,0..5] of String[20] =
{GPIO  ALT 0         ALT 1          ALT 2         ALT 3             ALT 4               ALT 5}
{0} (('SDA0',       'SA5',         '<reserved>', '',               '',                 ''),
     ('SCL0',       'SA4',         '<reserved>', '',               '',                 ''),
     ('SDA1',       'SA3',         '<reserved>', '',               '',                 ''),
     ('SCL1',       'SA2',         '<reserved>', '',               '',                 ''),
     ('GPCLK0',     'SA1',         '<reserved>', '',               '',                 'ARM_TDI'),
     ('GPCLK1',     'SA0',         '<reserved>', '',               '',                 'ARM_TDO'),
     ('GPCLK2',     'SOE_N/SE',    '<reserved>', '',               '',                 'ARM_RTCK'),
     ('SPI0_CE1_N', 'SWE_N/SRW_N', '<reserved>', '',               '',                 ''),
     ('SPI0_CE0_N', 'SD0',         '<reserved>', '',               '',                 ''),
     ('SPI0_MISO',  'SD1',         '<reserved>', '',               '',                 ''),
{10} ('SPI0_MOSI',  'SD2',         '<reserved>', '',               '',                 ''),
     ('SPI0_SCLK',  'SD3',         '<reserved>', '',               '',                 ''),
     ('PWM0',       'SD4',         '<reserved>', '',               '',                 'ARM_TMS'),
     ('PWM1',       'SD5',         '<reserved>', '',               '',                 'ARM_TCK'),
     ('TXD0',       'SD6',         '<reserved>', '',               '',                 'TXD1'),
     ('RXD0',       'SD7',         '<reserved>', '',               '',                 'RXD1'),
     ('<reserved>', 'SD8',         '<reserved>', 'CTS0',           'SPI1_CE2_N',       'CTS1'),
     ('<reserved>', 'SD9',         '<reserved>', 'RTS0',           'SPI1_CE1_N',       'RTS1'),
     ('PCM_CLK',    'SD10',        '<reserved>', 'BSCSL SDA/MOSI', 'SPI1_CE0_N',       'PWM0'),
     ('PCM_FS',     'SD11',        '<reserved>', 'BSCSL SCL/SCLK', 'SPI1_MISO',        'PWM1'),
{20} ('PCM_DIN',    'SD12',        '<reserved>', 'BSCSL/MISO',     'SPI1_MOSI',        'GPCLK0'),
     ('PCM_DOUT',   'SD13',        '<reserved>', 'BSCSL/CE_N',     'SPI1_SCLK',        'GPCLK1'),
     ('<reserved>', 'SD14',        '<reserved>', 'SD1_CLK',        'ARM_TRST',         ''),
     ('<reserved>', 'SD15',        '<reserved>', 'SD1_CMD',        'ARM_RTCK',         ''),
     ('<reserved>', 'SD16',        '<reserved>', 'SD1_DAT0',       'ARM_TDO',          ''),
     ('<reserved>', 'SD17',        '<reserved>', 'SD1_DAT1',       'ARM_TCK',          ''),
     ('<reserved>', '<reserved>',  '<reserved>', 'SD1_DAT2',       'ARM_TDI',          ''),
     ('<reserved>', '<reserved>',  '<reserved>', 'SD1_DAT3',       'ARM_TMS',          ''),
     ('SDA0',       'SA5',         'PCM_CLK',    '<reserved>',     '',                 ''),
     ('SCL0',       'SA4',         'PCM_FS',     '<reserved>',     '',                 ''),
{30} ('<reserved>', 'SA3',         'PCM_DIN',    'CTS0',           '',                 'CTS1'),
     ('<reserved>', 'SA2',         'PCM_DOUT',   'RTS0',           '',                 'RTS1'),
     ('GPCLK0',     'SA1',         '<reserved>', 'TXD0',           '',                 'TXD1'),
     ('<reserved>', 'SA0',         '<reserved>', 'RXD0',           '',                 'RXD1'),
     ('GPCLK0',     'SOE_N/SE',    '<reserved>', '<reserved>',     '',                 ''),
     ('SPI0_CE1_N', 'SWE_N/SRW_N', '',           '<reserved>',     '',                 ''),
     ('SPI0_CE0_N', 'SD0',         'TXD0',       '<reserved>',     '',                 ''),
     ('SPI0_MISO',  'SD1',         'RXD0',       '<reserved>',     '',                 ''),
     ('SPI0_MOSI',  'SD2',         'RTS0',       '<reserved>',     '',                 ''),
     ('SPI0_SCLK',  'SD3',         'CTS0',       '<reserved>',     '',                 ''),
{40} ('PWM0',       'SD4',         '',           '<reserved>',     'SPI2_MISO',        'TXD1'),
     ('PWM1',       'SD5',         '<reserved>', '<reserved>',     'SPI2_MOSI',        'RXD1'),
     ('GPCLK1',     'SD6',         '<reserved>', '<reserved>',     'SPI2_SCLK',        'RTS1'),
     ('GPCLK2',     'SD7',         '<reserved>', '<reserved>',     'SPI2_CE0_N',       'CTS1'),
     ('GPCLK1',     'SDA0',         'SDA1',      '<reserved>',     'SPI2_CE1_N',       ''),
     ('PWM1'  ,     'SCL0',         'SCL1',      '<reserved>',     'SPI2_CE2_N',       ''),
     ('<Internal>', '',             '',          '',               '',                 ''),
     ('<Internal>', '',             '',          '',               '',                 ''),
     ('<Internal>', '',             '',          '',               '',                 ''),
     ('<Internal>', '',             '',          '',               '',                 ''),
{50} ('<Internal>', '',             '',          '',               '',                 ''),
     ('<Internal>', '',             '',          '',               '',                 ''),
     ('<Internal>', '',             '',          '',               '',                 ''),
{53} ('<Internal>', '',             '',          '',               '',                 ''));


  GpioAltModePi4Def: Array[0..57,0..5] of String[20] =
{GPIO  ALT 0         ALT 1          ALT 2         ALT 3             ALT 4               ALT 5}
{0} (('SDA0',       'SA5',         'PCLK',       'SPI3_CE0_N',     'TXD2',             'SDA6'),
     ('SCL0',       'SA4',         'DE',         'SPI3_MISO',      'RXD2',             'SCL6'),
     ('SDA1',       'SA3',         'LCD_VSYNC',  'SPI3_MOSI',      'CTS2',             'SDA3'),
     ('SCL1',       'SA2',         'LCD_HSYNC',  'SPI3_SCLK',      'RTS2',             'SCL3'),
     ('GPCLK0',     'SA1',         'DPI_D0',     'SPI4_CE0_N',     'TXD3',             'SDA3'),
     ('GPCLK1',     'SA0',         'DPI_D1',     'SPI4_MISO',      'RXD3',             'SCL3'),
     ('GPCLK2',     'SOE_N/SE',    'DPI_D2',     'SPI4_MOSI',      'CTS3',             'SDA4'),
     ('SPI0_CE1_N', 'SWE_N/SRW_N', 'DPI_D3',     'SPI4_SCLK',      'RTS3',             'SCL4'),
     ('SPI0_CE0_N', 'SD0',         'DPI_D4',     'BSCSL/CE_N',     'TXD4',             'SDA4'),
     ('SPI0_MISO',  'SD1',         'DPI_D5',     'BSCSL/MISO',     'RXD4',             'SCL4'),
{10} ('SPI0_MOSI',  'SD2',         'DPI_D6',     'BSCSL SDA/MOSI', 'CTS4',             'SDA5'),
     ('SPI0_SCLK',  'SD3',         'DPI_D7',     'BSCSL SCL/SCLK', 'RTS4',             'SCL5'),
     ('PWM0_0',     'SD4',         'DPI_D8',     'SPI5_CE0_N',     'TXD5',             'SDA5'),
     ('PWM0_1',     'SD5',         'DPI_D9',     'SPI5_MISO',      'RXD5',             'SCL5'),
     ('TXD0',       'SD6',         'DPI_D10',    'SPI5_MOSI',      'CTS5',             'TXD1'),
     ('RXD0',       'SD7',         'DPI_D11',    'SPI5_SCLK',      'RTS5',             'RXD1'),
     ('<reserved>', 'SD8',         'DPI_D12',    'CTS0',           'SPI1_CE2_N',       'CTS1'),
     ('<reserved>', 'SD9',         'DPI_D13',    'RTS0',           'SPI1_CE1_N',       'RTS1'),
     ('PCM_CLK',    'SD10',        'DPI_D14',    'SPI6_CE0_N',     'SPI1_CE0_N',       'PWM0_0'),
     ('PCM_FS',     'SD11',        'DPI_D15',    'SPI6_MISO',      'SPI1_MISO',        'PWM0_1'),
{20} ('PCM_DIN',    'SD12',        'DPI_D16',    'SPI6_MOSI',      'SPI1_MOSI',        'GPCLK0'),
     ('PCM_DOUT',   'SD13',        'DPI_D17',    'SPI6_SCLK',      'SPI1_SCLK',        'GPCLK1'),
     ('SD0_CLK',    'SD14',        'DPI_D18',    'SD1_CLK',        'ARM_TRST',         'SDA6'),
     ('SD0_CMD',    'SD15',        'DPI_D19',    'SD1_CMD',        'ARM_RTCK',         'SCL6'),
     ('SD0_DAT0',   'SD16',        'DPI_D20',    'SD1_DAT0',       'ARM_TDO',          'SPI3_CE1_N'),
     ('SD0_DAT1',   'SD17',        'DPI_D21',    'SD1_DAT1',       'ARM_TCK',          'SPI4_CE1_N'),
     ('SD0_DAT2',   '<reserved>',  'DPI_D22',    'SD1_DAT2',       'ARM_TDI',          'SPI5_CE1_N'),
     ('SD0_DAT3',   '<reserved>',  'DPI_D23',    'SD1_DAT3',       'ARM_TMS',          'SPI6_CE1_N'),
     ('SDA0',       'SA5',         'PCM_CLK',    '<reserved>',     'MII_A_RX_ERR',     'RGMII_MDIO'),
     ('SCL0',       'SA4',         'PCM_FS',     '<reserved>',     'MII_A_TX_ERR',     'RGMII_MDC'),
{30} ('<reserved>', 'SA3',         'PCM_DIN',    'CTS0',           'MII_A_CRS',        'CTS1'),
     ('<reserved>', 'SA2',         'PCM_DOUT',   'RTS0',           'MII_A_COL',        'RTS1'),
     ('GPCLK0',     'SA1',         '<reserved>', 'TXD0',           'SD_CARD_PRES',     'TXD1'),
     ('<reserved>', 'SA0',         '<reserved>', 'RXD0',           'SD_CARD_WR',       'PROT RXD1'),
     ('GPCLK0',     'SOE_N/SE',    '<reserved>', 'SD1_CLK',        'SD_CARD_LED',      'RGMII_IRQ'),
     ('SPI0_CE1_N', 'SWE_N/SRW_N', '',           'SD1_CMD',        'RGMII_START_STOP', ''),
     ('SPI0_CE0_N', 'SD0',         'TXD0',       'SD1_DAT0',       'RGMII_RX_OK',      'MII_A_RX_ERR'),
     ('SPI0_MISO',  'SD1',         'RXD0',       'SD1_DAT1',       'RGMII_MDIO',       'MII_A_TX_ERR'),
     ('SPI0_MOSI',  'SD2',         'RTS0',       'SD1_DAT2',       'RGMII_MDC',        'MII_A_CRS'),
     ('SPI0_SCLK',  'SD3',         'CTS0',       'SD1_DAT3',       'RGMII_IRQ',        'MII_A_COL'),
{40} ('PWM1_0',     'SD4',         '',           'SD1_DAT4',       'SPI0_MISO',        'TXD1'),
     ('PWM1_1',     'SD5',         '<reserved>', 'SD1_DAT5',       'SPI0_MOSI',        'RXD1'),
     ('GPCLK1',     'SD6',         '<reserved>', 'SD1_DAT6',       'SPI0_SCLK',        'RTS1'),
     ('GPCLK2',     'SD7',         '<reserved>', 'SD1_DAT7',       'SPI0_CE0_N',       'CTS1'),
     ('GPCLK1',     'SDA0',         'SDA1',      '<reserved>',     'SPI0_CE1_N',       'SD_CARD_VOLT'),
     ('PWM0_1',     'SCL0',         'SCL1',      '<reserved>',     'SPI0_CE2_N',       'SD_CARD_PW R0'),
     ('<Internal>', '',             '',          '',               '',                 ''),
     ('<Internal>', '',             '',          '',               '',                 ''),
     ('<Internal>', '',             '',          '',               '',                 ''),
     ('<Internal>', '',             '',          '',               '',                 ''),
{50} ('<Internal>', '',             '',          '',               '',                 ''),
     ('<Internal>', '',             '',          '',               '',                 ''),
     ('<Internal>', '',             '',          '',               '',                 ''),
     ('<Internal>', '',             '',          '',               '',                 ''),
     ('<Internal>', '',             '',          '',               '',                 ''),
     ('<Internal>', '',             '',          '',               '',                 ''),
     ('<Internal>', '',             '',          '',               '',                 ''),
{57} ('<Internal>', '',             '',          '',               '',                 ''));


function GpioAltModeToStr(Cpu, AltMode: Integer): String;
begin
  case AltMode of
    FSEL_ALT0: AltMode:= 0;
    FSEL_ALT1: AltMode:= 1;
    FSEL_ALT2: AltMode:= 2;
    FSEL_ALT3: AltMode:= 3;
    FSEL_ALT4: AltMode:= 4;
    FSEL_ALT5: AltMode:= 5;
    else       AltMode:= -1;
  end;

  if Cpu = PI_CPU_BCM2711 then
  begin
    if (Gpio in [0..57]) and (AltMode in [0..5])
      then Result:= GpioAltModePi4Def[Gpio, AltMode]
      else Result:= 'Unknown Alt Mode';
  end
  else
  begin
    if (Gpio in [0..53]) and (AltMode in [0..5])
      then Result:= GpioAltModePi1to3Def[Gpio, AltMode]
      else Result:= 'Unknown Alt Mode';
  end;
end;


end.

