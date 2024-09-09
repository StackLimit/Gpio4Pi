# Gpio4Pi
 Lazarus Pascal GPIO for Raspberry Pi

**Preface**</BR>
For many years I have used WiringPi for basic Input/Output with a translated Pascal header.
Now I had to use PWM on Gpio pins and found out that it doesn't really support Rpi4 (BCM2711) to the full.
So I decided to make a simple GPIO interface for Lazarus Pascal.

Why another GPIO library when there are so many out there?
Because of my own interest in how the CPU is built and how I use it,
and I wanted a simple interface to the GPIO, which is written in Pascal.

All the functions in this library are most likely the same functions as in all the other GPIO libraries out there.
But everything in this library is written in pure Pascal (Lazarus).
Some of the declarations and functions are converted from WiringPi and rewritten.

To use the library you just copy the two files: **Gpio4Pi.pas** and **GpioDefs.pas** to your source directory.
- **Gpio4Pi.pas** is the actual GPIO object with all the functions and
- **GpioDefs.pas** are all the constant definitions used for calling the functions.

There is a brief description of all the functions in Gpio4Pi.pas.

**Support:** Supports Raspberry Pi 1 to Raspberry Pi 4B. NO support for Rpi 5 yet.

Like other GPIO libraries, Rpi user must be in the same group as /dev/gpiomem to perform basic GPIO Input/Output operations.
To perform Clock's and PWM operations, sudo must be used when running the application.

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

## Gpio4PiCmd
This is a command-line application with the following options:
```
- info ..................: Show info about the Paspberry PI
- show ..................: Show all GPIO, Clock and PWM information
- mode <mode gpio> ......: Set GPIO to Mode. Mode=in,out,alt0..alt5
- pull <pull gpio> ......: Set GPIO Pull-Up/Down. Pull=none,up,down
- write <val gpio> ......: Write a Value to GPIO. Value=0,1
- read <gpio> ...........: Read a Value from GPIO
- clock <freq gpio> .....: Set GPIO Clock in Hz
- pwmclock <freq> .......: Set PWM Master Clock in Hz
- pwmmode <mode gpio> ...: Set PWM Mode for GPIO x. Mode=bal,ms
- pwmrange <range gpio> .: Set PWM Range for GPIO x
- pwmduty <duty gpio> ...: Set PWM Duty Cycle for GPIO x
```

Note that two files, baseunix.pas and unix.pas, also are included in this directory. 
It is to be able to compile and test the application under Windows. 
These two files MUST NOT be copied to the Raspberry Pi when the application is to be built there.

An example of running Gpio4PiCmd with option show on a Rpi4 with PicorePlayer:
```
tc@pCP9:~$ sudo Gpio4PiCmd show
GPIO Initialized OK, Using /dev/mem

---------- All GPIOs ----------
GPIO 0: Mode=Output, Level=Low, Pull=PullUp
GPIO 1: Mode=Input, Level=High, Pull=PullUp
GPIO 2: Mode=Alt 0 ~ SDA1, Level=High, Pull=PullUp
GPIO 3: Mode=Alt 0 ~ SCL1, Level=High, Pull=PullUp
GPIO 4: Mode=Input, Level=High, Pull=PullUp
GPIO 5: Mode=Input, Level=High, Pull=PullUp
GPIO 6: Mode=Input, Level=High, Pull=PullUp
GPIO 7: Mode=Output, Level=High, Pull=PullUp
GPIO 8: Mode=Output, Level=High, Pull=PullUp
GPIO 9: Mode=Alt 0 ~ SPI0_MISO, Level=Low, Pull=PullDown
GPIO 10: Mode=Alt 0 ~ SPI0_MOSI, Level=Low, Pull=PullDown
GPIO 11: Mode=Alt 0 ~ SPI0_SCLK, Level=Low, Pull=PullDown
GPIO 12: Mode=Output, Level=High, Pull=PullDown
GPIO 13: Mode=Alt 0 ~ PWM0_1, Level=Low, Pull=PullDown
GPIO 14: Mode=Input, Level=High, Pull=PullUp
GPIO 15: Mode=Input, Level=High, Pull=PullUp
GPIO 16: Mode=Input, Level=Low, Pull=PullDown
GPIO 17: Mode=Input, Level=High, Pull=PullDown
GPIO 18: Mode=Alt 0 ~ PCM_CLK, Level=Low, Pull=PullDown
GPIO 19: Mode=Alt 0 ~ PCM_FS, Level=Low, Pull=PullDown
GPIO 20: Mode=Alt 0 ~ PCM_DIN, Level=Low, Pull=PullDown
GPIO 21: Mode=Alt 0 ~ PCM_DOUT, Level=Low, Pull=PullDown
GPIO 22: Mode=Output, Level=High, Pull=PullDown
GPIO 23: Mode=Input, Level=High, Pull=PullUp
GPIO 24: Mode=Input, Level=High, Pull=PullUp
GPIO 25: Mode=Output, Level=High, Pull=PullDown
GPIO 26: Mode=Input, Level=High, Pull=PullUp
GPIO 27: Mode=Output, Level=High, Pull=PullDown
GPIO 28: Mode=Alt 5 ~ RGMII_MDIO, Level=High, Pull=PullUp
GPIO 29: Mode=Alt 5 ~ RGMII_MDC, Level=Low, Pull=PullDown
GPIO 30: Mode=Input, Level=High, Pull=PullUp
GPIO 31: Mode=Input, Level=High, Pull=No PullUp/Down
GPIO 32: Mode=Input, Level=High, Pull=PullDown
GPIO 33: Mode=Input, Level=High, Pull=PullDown
GPIO 34: Mode=Alt 3 ~ SD1_CLK, Level=High, Pull=No PullUp/Down
GPIO 35: Mode=Alt 3 ~ SD1_CMD, Level=High, Pull=PullUp
GPIO 36: Mode=Alt 3 ~ SD1_DAT0, Level=High, Pull=PullUp
GPIO 37: Mode=Alt 3 ~ SD1_DAT1, Level=High, Pull=PullUp
GPIO 38: Mode=Alt 3 ~ SD1_DAT2, Level=High, Pull=PullUp
GPIO 39: Mode=Alt 3 ~ SD1_DAT3, Level=High, Pull=PullUp
GPIO 40: Mode=Alt 0 ~ PWM1_0, Level=Low, Pull=No PullUp/Down
GPIO 41: Mode=Alt 0 ~ PWM1_1, Level=Low, Pull=No PullUp/Down
GPIO 42: Mode=Output, Level=Low, Pull=PullUp
GPIO 43: Mode=Input, Level=High, Pull=PullUp
GPIO 44: Mode=Input, Level=High, Pull=PullUp
GPIO 45: Mode=Input, Level=High, Pull=PullUp
GPIO 46: Mode=Input, Level=Low, Pull=PullUp
GPIO 47: Mode=Input, Level=Low, Pull=PullUp
GPIO 48: Mode=Input, Level=Low, Pull=PullDown
GPIO 49: Mode=Input, Level=Low, Pull=PullDown
GPIO 50: Mode=Input, Level=Low, Pull=PullDown
GPIO 51: Mode=Input, Level=Low, Pull=PullDown
GPIO 52: Mode=Input, Level=Low, Pull=PullDown
GPIO 53: Mode=Input, Level=Low, Pull=PullDown
GPIO 54: Mode=Input, Level=Low, Pull=PullDown
GPIO 55: Mode=Input, Level=Low, Pull=PullDown
GPIO 56: Mode=Input, Level=Low, Pull=PullDown
GPIO 57: Mode=Input, Level=Low, Pull=PullDown

---------- All CLOCKs ----------
GPIO CLOCK 0: Enable=No, Source=GND, Kill=No, Flip=No, MASH=1-stage MASH, Div=0, Frac=0, Freq=0Hz
  Connected GPIOs: None

GPIO CLOCK 1: Enable=No, Source=GND, Kill=No, Flip=No, MASH=1-stage MASH, Div=0, Frac=0, Freq=0Hz
  Connected GPIOs: None

GPIO CLOCK 2: Enable=No, Source=GND, Kill=No, Flip=No, MASH=1-stage MASH, Div=0, Frac=0, Freq=0Hz
  Connected GPIOs: None

PCM CLOCK: Enable=No, Source=GND, Kill=No, Flip=No, MASH=1-stage MASH, Div=0, Frac=0, Freq=0Hz

PWM CLOCK: Enable=Yes, Source=PLLD per, Kill=No, Flip=No, MASH=Integer Division, Div=750, Frac=0, Freq=1000000Hz

---------- All PWMs ----------
PWM 0_0: Enable=Yes, Serial=Yes, Repeat=No, Silence=No, Polarity=No, UseFifo=Yes, M/S mode=No, Range=10, Data=0
  Connected GPIOs: None

PWM 0_1: Enable=Yes, Serial=No, Repeat=No, Silence=No, Polarity=No, UseFifo=No, M/S mode=Yes, Range=5000, Data=150
  Connected GPIOs: 13

PWM 1_0: Enable=No, Serial=No, Repeat=No, Silence=No, Polarity=No, UseFifo=No, M/S mode=No, Range=32, Data=0
  Connected GPIOs: 40

PWM 1_1: Enable=No, Serial=No, Repeat=No, Silence=No, Polarity=No, UseFifo=No, M/S mode=No, Range=32, Data=0
  Connected GPIOs: 41
```

## Gpio4PiMonitor
This is a GUI application where the Pi's memory is scanned for changes all the time
and changes in the memory are presented graphically.
All GPIOs are shown with Input, Output, Alt0..5 and Pull Up/Down.
All Clock's and PWM's are displayed with all their values.

At the moment the app are written for Raspberry Pi 4 (BCM2711) and therefore are showing all 57 GPIO pins and four PWM channels.

Note that two files, baseunix.pas and unix.pas, also are included in this directory. 
It is to be able to compile and test the application under Windows. 
These two files MUST NOT be copied to the Raspberry Pi when the application is to be built there.

## Gpio4PiTest
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

## Gpio4PiClient
This is a GUI app where the Raspberry Pi 40-pin header is presented graphically.
The status of all GPIO pins can be read here.
Clocks and PWM can also be read.
The application is used together with Gpio4PiTest which communicate together via UDP.
To use this app, both this app and Gpio4PiTest must be running.
