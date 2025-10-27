unit RasPiMem;

// -------------------------------------------------------------------
//
// Memory scanner
// Scans PI's memory for changes and updates Forms when changes occur
//
// Still under development and therefore not quite finished
// Copyright (c) 2024-2025 Jan Andersen
// -------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Gpio4Pi;


var
  PiGpio: TPiGpio;


implementation

Uses
  GpioDefs, MainForm, ClockForm, PwmForm, OverviewForm;



// ------------------------------------------------------------------------
//
// MEM scanner in Thread
//
// ------------------------------------------------------------------------

type
  TMemThread = class(TThread)
  private
    Step: Integer;
    OldGpioPin: Array[0..57] of TGpioPin;
    OldGpioClk: Array[0..8]  of TGpioClk;
    OldPwmData: Array[0..1]  of TPwmData;
    procedure ScanGPIOClockMem;
    procedure ScanPWMMem;
    procedure ScanGPIOMem;

  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
  end;


var
  MemThread: TMemThread;


// -------------------------------------------------------------
// Scan GPIO Clock 0-2/5, PCM and PWM clocks for changes
// -------------------------------------------------------------
procedure TMemThread.ScanGPIOClockMem;
var
  I: Integer;
  ClkNo: TClockNumber;
  Data: TGpioClk;

begin
  if PiGpio.UsingGpioMem then exit;

  for I:= Low(OldGpioClk) to High(OldGpioClk) do
  begin
    if PiGpio.RPiModelInfo.Cpu = PI_CPU_BCM2712 then
    begin
      // RaspberryPi 5
      case I of
        0: ClkNo:= CLK_GPIO0;
        1: ClkNo:= CLK_GPIO1;
        2: ClkNo:= CLK_GPIO2;
        3: ClkNo:= CLK_GPIO3;
        4: ClkNo:= CLK_GPIO4;
        5: ClkNo:= CLK_GPIO5;
        6: ClkNo:= CLK_PWM;
        7: ClkNo:= CLK_UART;
        8: ClkNo:= CLK_PCM;
        else exit;
      end;
    end
    else
    begin
      // RaspberryPi 1 - RaspberryPi 4
      case I of
        0: ClkNo:= CLK_GPIO0;
        1: ClkNo:= CLK_GPIO1;
        2: ClkNo:= CLK_GPIO2;
        3: ClkNo:= CLK_PWM;
        4: ClkNo:= CLK_UART;
        5: ClkNo:= CLK_PCM;
        else exit;
      end;
    end;

    if PiGpio.GetRawClockData(ClkNo, Data{%H-}) then
    begin
      if (Data.Control <> OldGpioClk[Ord(ClkNo)].Control) or
         (Data.Divisor <> OldGpioClk[Ord(ClkNo)].Divisor) or
         (Data.Fract   <> OldGpioClk[Ord(ClkNo)].Fract) then
      begin
        // Clock Changed, update Clock Form and Overview
        FormClocks.UpdateClock(ClkNo);
        FormOverview.UpdateView;

        OldGpioClk[Ord(ClkNo)]:= Data;
      end;
    end;
  end;
end;



// -------------------------------------------------------------
// Scan PWM blocks for changes
// -------------------------------------------------------------
procedure TMemThread.ScanPWMMem;
var
  Grp,Chan: Integer;
  Data: TPwmData;
  Changed: Boolean;

begin
  if PiGpio.UsingGpioMem then exit;

  for Grp:= Low(OldPwmData) to High(OldPwmData) do
  begin
    if PiGpio.GetRawPwmData(TPwmGroupNumber(Grp), Data{%H-}) then
    begin
      if Data.Cpu = PI_CPU_BCM2712 then
      begin
        // RaspberryPi 5
        Changed:= ((Data.Rp1GlobCtrl <> OldPwmData[Grp].Rp1GlobCtrl) or
                   (Data.Rp1FifoCtrl <> OldPwmData[Grp].Rp1FifoCtrl) or
                   (Data.Rp1ComRange <> OldPwmData[Grp].Rp1ComRange) or
                   (Data.Rp1ComDuty  <> OldPwmData[Grp].Rp1ComDuty) or
                   (Data.Rp1DutyFifo <> OldPwmData[Grp].Rp1DutyFifo));

        for Chan:= Low(Data.Rp1Channels) to High(Data.Rp1Channels) do
        begin
          if (Data.Rp1Channels[Chan].Rp1Control <> OldPwmData[Grp].Rp1Channels[Chan].Rp1Control) or
             (Data.Rp1Channels[Chan].Rp1Range <> OldPwmData[Grp].Rp1Channels[Chan].Rp1Range) or
             (Data.Rp1Channels[Chan].Rp1Phase <> OldPwmData[Grp].Rp1Channels[Chan].Rp1Phase) or
             (Data.Rp1Channels[Chan].Rp1Duty <> OldPwmData[Grp].Rp1Channels[Chan].Rp1Duty) or
             Changed then
          begin
            // PWM channel changed, update PWM Form and Overview
            FormPwm.UpdatePwmBlock(TPwmGroupNumber(Grp));
            FormOverview.UpdateView;

            OldPwmData[Grp]:= Data;
          end;
        end;
      end
      else
      begin
        // RaspberryPi 1 - RaspberryPi 4
        Changed:= ((Data.Control <> OldPwmData[Grp].Control) or
                   (Data.Status  <> OldPwmData[Grp].Status) or
                   (Data.DMA     <> OldPwmData[Grp].DMA) or
                   (Data.FIFO    <> OldPwmData[Grp].FIFO));

        for Chan:= Low(Data.Channels) to High(Data.Channels) do
        begin
          if (Data.Channels[Chan].Range <> OldPwmData[Grp].Channels[Chan].Range) or
             (Data.Channels[Chan].Data  <> OldPwmData[Grp].Channels[Chan].Data) or
             Changed then
          begin
            // PWM channel changed, update PWM Form and Overview
            FormPwm.UpdatePwmBlock(TPwmGroupNumber(Grp));
            FormOverview.UpdateView;

            OldPwmData[Grp]:= Data;
          end;
        end;
      end;
    end;
  end;
end;



// -------------------------------------------------------------
// Scan all GPIO's for changes
// -------------------------------------------------------------
procedure TMemThread.ScanGPIOMem;
var
  Gpin: Integer;
  Data: TGpioPin;

begin
  Gpin:= 0;

  while PiGpio.GetGpioPinData(Gpin, Data{%H-}) do
  begin
    if (Data.Mode <> OldGpioPin[Gpin].Mode) or
       (Data.Level <> OldGpioPin[Gpin].Level) or
       (Data.Pull <> OldGpioPin[Gpin].Pull) then
    begin
      // Pin Changed, Update Combo and Overview
      FormMain.UpdatePinCombo(Gpin);
      FormOverview.UpdateView;

      OldGpioPin[Gpin]:= Data;
    end;
    Gpin:= Gpin + 1;
  end;
end;



// -------------------------------------------------------------
// Thread Execute loop
// -------------------------------------------------------------
procedure TMemThread.Execute;
begin
  Sleep(1000);  // Wait until FormMain are ready
  Step:= 0;

  while (not Terminated) do
  begin
    if PiGpio <> Nil then
    begin
      Step:= Step + 1;
      if Step > 3 then Step:= 1;

      case Step of
        1: Synchronize(@ScanGPIOMem);       // GPIO Memory
        2: Synchronize(@ScanGPIOClockMem);  // GPIO Clock Memory
        3: Synchronize(@ScanPWMMem);        // PWM Memory
        else Step:= 0;
      end;
    end;

    Sleep(10);
//    DelayMicroseconds(10000);  // 10mS <- This uses a LOT of CPU time
  end;
end;


// -------------------------------------------------------------
// Thread Create
// -------------------------------------------------------------
constructor TMemThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate:= True;
end;




Initialization
  PiGpio:= Nil;

  // Start Memory scanning Thread
  MemThread:= TMemThread.Create(False);


Finalization
  if MemThread <> nil then MemThread.Terminate;


end.
