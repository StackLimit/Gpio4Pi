# Gpio4Pi
 Lazarus Pascal GPIO for Raspberry Pi

Preface:</BR>
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

```pas
program GpioTest;
uses Gpio4Pi, GpioDefs;
var
  PiGpio: TPiGpio;
  
begin
  PiGpio:= TPiGpio.Create;
  if PiGpio <> nil then
  begin
    PiGpio.SetPinMode(4, PM_OUTPUT);
    PiGpio.GpioWrite(4, PIN_HIGH);
  end;
end;
```

Thats it.....

To have some examples to look at, there are four directories to look at:

Gpio4PiTest:</BR>
============</BR>
This is a GUI app where you can set any GPIO pin for Input, Output, Alt0..5, PWM and Clock mode.
Pull Up/Down can also be tested and can be read/written from/to the GPIO pin. 
Furthermore, the GPIO clock frequency can be set and the PWM frequency and Range/Value can be set.
I used this when developing the Gpio4Pi object.

Note that two files, baseunix.pas and unix.pas, are included in this directory. 
It is to be able to compile and test the application under Windows. 
These two files MUST NOT be copied to the Raspberry Pi when the application is to be built there.
Sometimes I write the software on my Windows PC with multiple monitors and then transfer
the source files to Rasbian on the PI and compile the project there.

Furthermore, the Windows version has built-in memory simulation so the app works under Windows.
It is also built in so that changes in memory are sent via UDP to a Client,
which is in the directory Gpio4PiClient.

Gpio4PiClient:</BR>
==============</BR>
This is a GUI app where the Raspberry Pi 40-pin header is presented graphically.
The status of all GPIO pins can be read here.
The application is used together with Gpio4PiTest.
To use this app, both this app and Gpio4PiTest must be running.
Clocks and PWM can also be read in this app.

Gpio4PiCmd:</BR>
===========</BR>
This is a command-line application with the following options:</BR>
- info ..................: Show info about the Paspberry PI</BR>
- show ..................: Show all GPIO, Clock and PWM information</BR>
- mode <mode gpio> ......: Set GPIO to Mode. Mode=in,out,alt0..alt5</BR>
- pull <pull gpio> ......: Set GPIO Pull-Up/Down. Pull=none,up,down</BR>
- write <val gpio> ......: Write a Value to GPIO. Value=0,1</BR>
- read <gpio> ...........: Read a Value from GPIO</BR>
- clock <freq gpio> .....: Set GPIO Clock in Hz</BR>
- pwmclock <freq> .......: Set PWM Master Clock in Hz</BR>
- pwmmode <mode gpio> ...: Set PWM Mode for GPIO x. Mode=bal,ms</BR>
- pwmrange <range gpio> .: Set PWM Range for GPIO x</BR>
- pwmduty <duty gpio> ...: Set PWM Duty Cycle for GPIO x</BR>

Note that two files, baseunix.pas and unix.pas, also are included in this directory. 
It is to be able to compile and test the application under Windows. 
These two files MUST NOT be copied to the Raspberry Pi when the application is to be built there.

Gpio4PiMonitor:</BR>
===============</BR>
This is a GUI application where the Pi's memory is scanned for changes all the time
and changes in the memory are presented graphically.
All GPIOs are shown with Input, Output, Alt0..5 and Pull Up/Down.
All Clock's and PWM's are displayed with all their values.

Note that two files, baseunix.pas and unix.pas, also are included in this directory. 
It is to be able to compile and test the application under Windows. 
These two files MUST NOT be copied to the Raspberry Pi when the application is to be built there.

