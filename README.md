# Gpio4Pi
 Lazarus Pascal GPIO for Raspberry Pi

Preface:
For many years I have used WiringPi for basic Input/Output with a translated Pascal header.
Now I had to use PWM on Gpio pins and found out that it doesn't really support Rpi4 (BCM2711) to the full.

So I decided to make a simple GPIO interface for Lazarus Pascal.
Why another GPIO library when there are so many out there?
Because of my own interest in how the CPU is built and how I use it, and I wanted a simple interface to the GPIO.

All the functions in this library are most likely the same functions as in all the other GPIO libraries out there.
But everything in this library is written in pure Pascal (Lazarus).
Some of the declarations and functions are converted from WiringPi and rewritten.



