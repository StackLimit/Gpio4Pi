unit GpioPiTxtCmd;

// -------------------------------------------------------------------
//
// Text commands between Gpio4PiTest and Gpio4PiClient
//
// Used by the following modules:
// - Gpio4PiClient/GpioPiCli.pas
// - Gpio4PiTest/unix.pas
//
// Some commands have arguments. Eg. PinMode sets pin xx to In/Out/etc.
// Eg: PinMode,30,1 means that GPIO pin 30 is set to Output
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// -------------------------------------------------------------------

{$mode ObjFPC}{$H+}

{$hints off}
{$notes off}

interface


const
  // -----------------------------------------------
  // Commands for initialization, etc.
  // -----------------------------------------------

  TxtGpioPiReqFullMap   = 'GpioPiReqFullGpioMap';  // Client wants a full GPIO map
  TxtGpioPiFullGpioMap  = 'GpioPiFullGpioMap';     // Full GPIO map return to Client
  TxtGpioPiFullClockMap = 'GpioPiFullClockMap';    // Full Clock map return to Client
  TxtGpioPiFullPwmMap   = 'GpioPiFullPwmMap';      // Full PWM map return to Client


  // -----------------------------------------------
  // Commands
  // -----------------------------------------------

  // For GPIO
  TxtGpioSetGpioPin = 'GpioSetGpioPin';      // Client sets a GPIO pin to On/Off
  TxtGpioSetPinMode = 'GpioSetPinMode';
  TxtGpioSetPull    = 'GpioSetPull';
  TxtGpioPinLevel   = 'GpioPinLevel';

  // For Clock
  TxtGpioSetClock = 'GpioSetClock';
  TxtPcmSetClock  = 'PcmSetClock';
  TxtPwmSetClock  = 'PwmSetClock';

  // For PWM
  TxtPwmWholeGroup = 'PwmWholeGroup';        // Just send the whole PWM block in HEX




implementation

end.
