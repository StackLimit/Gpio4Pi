# Gpio4Pi
 Lazarus Pascal GPIO for Raspberry Pi

Preface:
For many years I have used WiringPi for basic Input/Output with a translated Pascal header.
Now I had to use PWM on Gpio pins and found out that it doesn't really support Rpi4 (BCM2711) to the full.

So I decided to make a simple GPIO interface for Lazarus Pascal.
Why another GPIO library when there are so many out there?
Because of my own interest in how the CPU is built and how I use it,
and I wanted a simple interface to the GPIO, which is written in Pascal.

All the functions in this library are most likely the same functions as in all the other GPIO libraries out there.
But everything in this library is written in pure Pascal (Lazarus).
Some of the declarations and functions are converted from WiringPi and rewritten.

To use the library you just copy the two files: Gpio4Pi.pas and GpioDefs.pas to your source directory.
Gpio4Pi.pas is the actual GPIO object with all the functions and
GpioDefs.pas are all the constant definitions used for calling the functions.
There is a brief description of all the functions in Gpio4Pi.pas.

A liitle example of using the library/object, set GPIO pin 4 high:

program GpioTest;</BR>
uses Gpio4Pi, GpioDefs;</BR>
var</BR>
  PiGpio: TPiGpio;</BR>
  
begin</BR>
  PiGpio:= TPiGpio.Create;</BR>
  PiGpio.SetPinMode(4, PM_OUTPUT);</BR>
  PiGpio.GpioWrite(4, PIN_HIGH);</BR>
end;</BR>


To have some examples to look at, there are four directories to look at:

Gpio4PiTest:</BR>
------------</BR>


