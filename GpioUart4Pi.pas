unit GpioUart4Pi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Gpio4Pi, GpioDefs;


// -----------------------------------------------
// Callback when Data is received
// -----------------------------------------------
type
  TRxDataEvent = procedure(UartNo: Integer; Const Data; Count: Integer) of object;
  TUartStatusEvent = procedure(UartNo: Integer; Status: LongWord) of object;

const
  // UartStatusEvent -> Status
  UART_STAT_RX_OVERRUN = $00000800;    // Overrun error
  UART_STAT_RX_BREAK   = $00000400;    // Break error
  UART_STAT_RX_PARITY  = $00000200;    // Parity error
  UART_STAT_RX_FRAMING = $00000100;    // Framing error. Not a valid stop bit

  UART_STAT_TX_DONE    = $00010000;    // TX done sending data


  // If Uart Master Clock are not running, we set it to 48MHz
  UartMasterClockFreq = 48000000;


type
  TTxRxThread = class(TThread)
    protected
      procedure Execute; override;
    private
      UartNo: Integer;
      pDR: ^LongWord;             // Pointer to Data Register
      pFR: ^LongWord;             // Pointer to Flag Register
      SleepMs: Integer;

      RxActive: Boolean;
      RxTimeOut: Int64;
      RxTimer: Int64;
      RxBuffer: TBytes;           // Array Of Byte;
      RxDataEvent: TRxDataEvent;

      TxActive: Boolean;
      TxIndex: Integer;
      TxBuffer: TBytes;           // Array Of Byte;

      UartStatus: LongWord;
      UartStatusEvent: TUartStatusEvent;
      procedure DataReceived;
      procedure UartStatusProc;
      procedure RunRx;
      procedure RunTx;
    public
    published
  end;


  // Uart Setup Data used by SetupUart()
  TSetupUartData = Record
    UartNo: Integer;
    Baud:   Integer;
    Mode:   LongWord;
    OnDataReceive: TRxDataEvent;
    OnUartStatus:  TUartStatusEvent;
  end;


const
  // TSetupUartData.Mode: Mask to be Or'ed together
  UARTMODE_8BITS   = $00000003;
  UARTMODE_7BITS   = $00000002;
  UARTMODE_6BITS   = $00000001;
  UARTMODE_5BITS   = $00000000;
  UARTMODE_2STOP   = $00000010;
  UARTMODE_PARENA  = $00000020;
  UARTMODE_EVENPAR = $00000040;
  UARTMODE_FLOWENA = $00000080;


type
  // Uart Data returned by GetRawUartData()
  TUartData = Record
    DataReg:     LongWord;  // Data Register
    FlagReg:     LongWord;  // Flag register
    BaudDivisor: LongWord;  // Integer Baud rate divisor
    BaudFract:   LongWord;  // Fractional Baud rate divisor
    LineCtlReg:  LongWord;  // Line Control register
    Control:     LongWord;  // Control register
  end;


  // GPIO Uart Class
  TPiGpioUart = class(TPiGpio)
  private
    TxRxThread: Array[0..5] Of TTxRxThread;

    // ----------------------------------------
    // Internal function for set pointer to right Uart
    // UartNo: Uart number 0,2,3,4,5
    // Return: Pointer to Uart(x) space. Nil = Error
    // ----------------------------------------
    function GetUartBasePtr(UartNo: Integer): Pointer;

    // ----------------------------------------
    // Internal function SetUartBaudRate:
    // Set the Baud Rate for Uart number X
    // UartNo: Uart number 0,2,3,4,5
    // Baud:   The Baud Rate. eg. 9600, 19200, 115200.
    //         If 0 the Uart is stopped.
    // Return: True on success, False on error
    // ----------------------------------------
    function SetUartBaudRate(UartNo: Integer; Baud: Integer): Boolean;

  protected
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
    // SetupUart:
    // Setup Uart number X
    // SetupData:
    //   .UartNo: Uart number 0,2,3,4,5
    //   .Baud:   Baud Rate. eg. 9600, 19200, 115200. If 0 the Uart is stopped
    //   .Mode:   Modes for the Uart
    //   .OnDataReceive: Callback for Received Data
    //   .OnUartStatus:  Callback for Uart Status
    // Return: True on success, False on error
    // ----------------------------------------
    function SetupUart(SetupData: TSetupUartData): Boolean;

    // ----------------------------------------
    // StopUart:
    // Stops Uart number X
    // UartNo: Uart number 0,2,3,4,5
    // Return: True on success, False on error
    // ----------------------------------------
    function StopUart(UartNo: Integer): Boolean;

    // ----------------------------------------
    // GetUartBaudRate:
    // Get the Baud Rate for Uart number X
    // UartNo: Uart number 0,2,3,4,5
    // Return: The Baud Rate. eg. 9600, 19200, 115200.
    //         0 if the Uart is stopped. -1 if error.
    // ----------------------------------------
    function GetUartBaudRate(UartNo: Integer): Integer;

    // ----------------------------------------
    // TransmitUartData:
    // Transmit data to Uart number X
    // UartNo: Uart number 0,2,3,4,5
    //
    // Return: True on success, False on error
    // ----------------------------------------
    function TransmitUartData(UartNo: Integer; Const Data; Count: Integer): Boolean;

    // ----------------------------------------
    // GetRawUartData:
    // Returns the Raw data for a Uart
    // UartNo: Uart number 0,2,3,4,5
    // Data: Data returned to the caller:
    //
    // Consult the BCM manual for description of a UART block
    // Return: True on success, False on error
    // ----------------------------------------
    function GetRawUartData(UartNo: Integer; var Data: TUartData): Boolean;

    // -------------------------------------------------------------
    // GetGpiosForUart:
    // Find all the GPIOs that are assigned to a Uart
    // UartNo: 0 for PI_CPU_BCM2835, PI_CPU_BCM2836 and PI_CPU_BCM2837 (Pi1-3)
    //         0,2,3,4,5 for BCM2711 (Pi4)
    // Return: Array of GPIO's. Empty = No GPIOs assigned
    // -------------------------------------------------------------
    function GetGpiosForUart(UartNo: Integer): TIntArray;
  end;


implementation

Uses
  Unix;


// ------------------------------------------------------------------------

function NowMicroSec: Int64; inline;
var
  tv: TimeVal;

begin
  FpGetTimeOfDay(@tv, nil);
  Result:= Int64(tv.tv_sec) * 1000000 + Int64(tv.tv_usec);
end;

// ------------------------------------------------------------------------

constructor TPiGpioUart.Create;
begin
  inherited Create;
end;

// ------------------------------------------------------------------------

destructor TPiGpioUart.Destroy;
begin
  inherited Destroy;
end;

// ------------------------------------------------------------------------

// ----------------------------------------
// Internal function for set pointer to right Uart
// ----------------------------------------
function TPiGpioUart.GetUartBasePtr(UartNo: Integer): Pointer;
begin
  if not IsCpuOk then Exit(Nil);
  if UsingGpioMem then Exit(Nil);

  if (UartNo = 1) then
  begin
    FLastErrorStr:= 'Uart 1 is a Mini Uart and are not supported';
    Exit(Nil);
  end;

  Case FRPiModel.Cpu of
    PI_CPU_BCM2835,
    PI_CPU_BCM2836,
    PI_CPU_BCM2837:
    begin
      if UartNo = 0 then Exit(SetPtr(PUartMem, UART0_OFFSET));
    end;

    PI_CPU_BCM2711:
    begin
      case UartNo of
        0: Exit(SetPtr(PUartMem, UART0_OFFSET));
        2: Exit(SetPtr(PUartMem, UART2_OFFSET));
        3: Exit(SetPtr(PUartMem, UART3_OFFSET));
        4: Exit(SetPtr(PUartMem, UART4_OFFSET));
        5: Exit(SetPtr(PUartMem, UART5_OFFSET));
      end;
    end;
  end;

  FLastErrorStr:= 'Uart number ' + IntToStr(UartNo) + ' not supported';
  Exit(Nil);
end;

// ------------------------------------------------------------------------

function TPiGpioUart.SetupUart(SetupData: TSetupUartData): Boolean;
var
  pUartBase,pUart: ^LongWord;
  LineCtl, Ctl: LongWord;
  GpioPins: TIntArray;

begin
  pUartBase:= GetUartBasePtr(SetupData.UartNo);
  if pUartBase = nil then Exit(False);

  Result:= True;

  // Disable Uart in Control Register
  // 1. Disable the UART.
  // 2. Wait for the end of transmission or reception of the current character.
  // 3. Flush the transmit FIFO by setting the FEN bit to 0 in the Line Control Register, UART_LCRH.
  // 4. Reprogram the Control Register, UART_CR.
  // 5. Enable the UART.
  StopUart(SetupData.UartNo);

  if SetupData.Baud = 0 then Exit;

  // Setup GPIO's for Uart
  if SetupData.UartNo = 0 then
  begin
    // Uart 0 are different ALT modes
    // We use the GPIO's on the Pin Header
    SetPinMode(14, PM_ALT0);      // TXD2
    SetPinMode(15, PM_ALT0);      // RXD2
    if (SetupData.Mode and UARTMODE_FLOWENA) <> 0 then
    begin
      SetPinMode(16, PM_ALT3);    // CTS2
      SetPinMode(17, PM_ALT3);    // RTS2
    end;
  end
  else
  begin   // Uart 2 to 5 are all ALT4 mode
    case SetupData.UartNo of
      2: GpioPins:= [ 0, 1, 2, 3];    // TX,RX,CTS,RTS
      3: GpioPins:= [ 4, 5, 6, 7];
      4: GpioPins:= [ 8, 9,10,11];
      5: GpioPins:= [12,13,14,15];
    end;

    SetPinMode(GpioPins[0], PM_ALT4);      // TXD2
    SetPinMode(GpioPins[1], PM_ALT4);      // RXD2
    if (SetupData.Mode and UARTMODE_FLOWENA) <> 0 then
    begin
      SetPinMode(GpioPins[2], PM_ALT4);    // CTS2
      SetPinMode(GpioPins[3], PM_ALT4);    // RTS2
    end;
  end;

  // Setup Baud rate
  if not SetUartBaudRate(SetupData.UartNo, SetupData.Baud) then Exit(False);

  // Setup Control and Line Control Register
  Ctl:= UART_CR_RXENABLE or UART_CR_TXENABLE;         // Enable Rx and Tx
  LineCtl:= ((SetupData.Mode and $3) shl 5) or        // Bit length. 5,6,7 or 8 bits
            UART_LCRH_FIFO;                           // Enable Fifo

  if (SetupData.Mode and UARTMODE_2STOP) <> 0 then
    LineCtl:= LineCtl or UART_LCRH_2STOP;             // 2 Stop Bits

  if (SetupData.Mode and UARTMODE_PARENA) <> 0 then
    LineCtl:= LineCtl or UART_LCRH_PARITY;            // Enable Parity

  if (SetupData.Mode and UARTMODE_EVENPAR) <> 0 then
    LineCtl:= LineCtl or UART_LCRH_EVEN;              // Even Parity

  if (SetupData.Mode and UARTMODE_FLOWENA) <> 0 then
    Ctl:= Ctl or UART_CR_CTSENA or UART_CR_RTSENA;    // Enable Flow Control

  // Load Uart Line Control and Control
  pUart:= SetPtr(pUartBase, UART_LCRH);
  pUart^:= LineCtl;

  pUart:= SetPtr(pUartBase, UART_CR);
  pUart^:= Ctl;

  // Start Uart
  pUart:= SetPtr(pUartBase, UART_CR);
  pUart^:= pUart^ or UART_CR_ENABLE;

  // Create and start new TX/RX Thread
  TxRxThread[SetupData.UartNo]:= TTxRxThread.Create(True);
  TxRxThread[SetupData.UartNo].UartNo:= SetupData.UartNo;
  TxRxThread[SetupData.UartNo].pDR:= SetPtr(pUartBase, UART_DR);
  TxRxThread[SetupData.UartNo].pFR:= SetPtr(pUartBase, UART_FR);
  TxRxThread[SetupData.UartNo].RxDataEvent:= SetupData.OnDataReceive;
  TxRxThread[SetupData.UartNo].UartStatusEvent:= SetupData.OnUartStatus;
  TxRxThread[SetupData.UartNo].RxTimeOut:= Trunc((1 / SetupData.Baud) * 20000000);
//  TxRxThread[SetupData.UartNo].TxTimeOut:= Trunc((1 / SetupData.Baud) * 400000000);

  case SetupData.Baud of
    0..50000:      TxRxThread[SetupData.UartNo].SleepMs:= 3;
    50001..100000: TxRxThread[SetupData.UartNo].SleepMs:= 2;
    else           TxRxThread[SetupData.UartNo].SleepMs:= 1;
  end;

  TxRxThread[SetupData.UartNo].FreeOnTerminate:= True;
  TxRxThread[SetupData.UartNo].Start;
end;

// ------------------------------------------------------------------------

function TPiGpioUart.StopUart(UartNo: Integer): Boolean;
var
  pUartBase,pUart: ^LongWord;
  Wait: LongWord;

begin
  // First Stop RX and TX Threads
  if TxRxThread[UartNo] <> nil then
  begin
    TxRxThread[UartNo].Terminate;
    TxRxThread[UartNo].WaitFor;
    TxRxThread[UartNo]:= nil;
  end;

  pUartBase:= GetUartBasePtr(UartNo);
  if pUartBase = nil then Exit(False);

  Result:= True;

  pUart:= SetPtr(pUartBase, UART_CR);       // Disable Uart
  pUart^:= pUart^ and (not UART_CR_ENABLE);

  pUart:= SetPtr(pUartBase, UART_FR);       // Wait for Not Busy
  Wait:= 0;
  while (pUart^ and UART_FR_BUSY) <> 0 do
  begin
    Sleep(1);
    Wait:= Wait + 1;
    if Wait > 1000 then
    begin
      FLastErrorStr:= 'Stop Wait Timeout';    // Debug Test
      Result:= False;
      Break;
    end;
  end;

  pUart:= SetPtr(pUartBase, UART_LCRH);     // Flush/Disable Fifo
  pUart^:= pUart^ and (not UART_LCRH_FIFO);

  pUart:= SetPtr(pUartBase, UART_RSRECR);   // Clear Receive Status Register
  pUart^:= 0;
end;

// ------------------------------------------------------------------------

function TPiGpioUart.SetUartBaudRate(UartNo: Integer; Baud: Integer): Boolean;
var
  pUartBase,pUart: ^LongWord;
  UartClk: LongWord;
  rDivisor: Real;
  iDiv,iFrac: LongWord;

begin
  pUartBase:= GetUartBasePtr(UartNo);
  if pUartBase = nil then Exit(False);

  if Baud = 0 then Exit(StopUart(UartNo));

  Result:= True;

  // Get the Uart Clock Frequency
  UartClk:= GetClockFrequency(CLK_UART);

  // Check for Uart Clock is running
  if UartClk = 0 then
  begin
    // Not running, start Uart Clock.
    UartClk:= UartMasterClockFreq;
    if not SetUartMasterClock(UartClk) then Exit(False);
  end;

  // Baud rate divisor BAUDDIV = (FUARTCLK/(16 * Baud rate))
  rDivisor:= UartClk / (16 * Baud);
  iDiv:= Trunc(rDivisor);
  if iDiv = 0 then
  begin
    iDiv:= 1;
    iFrac:= 0;
  end
  else
  if iDiv > $FFFF then
  begin
    iDiv:= $FFFF;
    iFrac:= 0;
  end
  else iFrac:= Trunc((Frac(rDivisor) * 64) + 0.5);   // 0.5= Round up
  if iFrac > 63 then iFrac:= 63;

  pUart:= SetPtr(pUartBase, UART_IBRD);         // Integer Baud rate divisor
  pUart^:= iDiv;

  pUart:= SetPtr(pUartBase, UART_FBRD);         // Fractional Baud rate divisor
  pUart^:= iFrac;
end;

// ------------------------------------------------------------------------

function TPiGpioUart.GetUartBaudRate(UartNo: Integer): Integer;
var
  pUartBase,pUart: ^LongWord;
  UartClk: LongWord;
  iDiv,iFrac: LongWord;

begin
  pUartBase:= GetUartBasePtr(UartNo);
  if pUartBase = nil then Exit(-1);

  UartClk:= GetClockFrequency(CLK_UART);
  if UartClk = 0 then Exit(0);

  pUart:= SetPtr(pUartBase, UART_IBRD);         // Integer Baud rate divisor
  iDiv:= pUart^ and $0000FFFF;                  // It's only bit 0 to 15

  pUart:= SetPtr(pUartBase, UART_FBRD);         // Fractional Baud rate divisor
  iFrac:= pUart^ and $0000003F;                 // It's only bit 0 to 5

  // Baud rate divisor BAUDDIV = (FUARTCLK/(16 * Baud rate))
  if iDiv > 0
    then Result:= Trunc(UartClk / (16 * (iDiv + (iFrac / 64))))
    else Result:= 0;
end;

// ------------------------------------------------------------------------

function TPiGpioUart.GetRawUartData(UartNo: Integer; var Data: TUartData): Boolean;
var
  pUartBase,pUart: ^LongWord;

begin
  pUartBase:= GetUartBasePtr(UartNo);
  if pUartBase = nil then Exit(False);

  Result:= True;

  pUart:= SetPtr(pUartBase, UART_DR);
  Data.DataReg:= pUart^;

  pUart:= SetPtr(pUartBase, UART_FR);
  Data.FlagReg:= pUart^;

  pUart:= SetPtr(pUartBase, UART_IBRD);
  Data.BaudDivisor:= pUart^ and $0000FFFF;     // It's only bit 0 to 15

  pUart:= SetPtr(pUartBase, UART_FBRD);
  Data.BaudFract:= pUart^ and $0000003F;       // It's only bit 0 to 5

  pUart:= SetPtr(pUartBase, UART_LCRH);
  Data.LineCtlReg:= pUart^;

  pUart:= SetPtr(pUartBase, UART_CR);
  Data.Control:= pUart^;
end;

// ------------------------------------------------------------------------

function TPiGpioUart.GetGpiosForUart(UartNo: Integer): TIntArray;
var
  Gpio: Integer;
  Ok:   Boolean;
  Data: TGpioPin;

begin
  Result:= [];
  if not IsCpuOk then Exit;

  for Gpio in [0..17,30..33,36..39] do
  begin
    Ok:= False;
    GetGpioPinData(Gpio, Data{%H-});

    case UartNo of
      0: Ok:= ((Gpio = 14) and (Data.Mode = FSEL_ALT0)) or
              ((Gpio = 15) and (Data.Mode = FSEL_ALT0)) or
              ((Gpio = 16) and (Data.Mode = FSEL_ALT3)) or
              ((Gpio = 17) and (Data.Mode = FSEL_ALT3)) or
              ((Gpio = 30) and (Data.Mode = FSEL_ALT3)) or
              ((Gpio = 31) and (Data.Mode = FSEL_ALT3)) or
              ((Gpio = 32) and (Data.Mode = FSEL_ALT3)) or
              ((Gpio = 33) and (Data.Mode = FSEL_ALT3)) or
              ((Gpio = 36) and (Data.Mode = FSEL_ALT2)) or
              ((Gpio = 37) and (Data.Mode = FSEL_ALT2)) or
              ((Gpio = 38) and (Data.Mode = FSEL_ALT2)) or
              ((Gpio = 39) and (Data.Mode = FSEL_ALT2));

      2: Ok:= ((Gpio = 0) and (Data.Mode = FSEL_ALT4)) or
              ((Gpio = 1) and (Data.Mode = FSEL_ALT4)) or
              ((Gpio = 2) and (Data.Mode = FSEL_ALT4)) or
              ((Gpio = 3) and (Data.Mode = FSEL_ALT4));

      3: Ok:= ((Gpio = 4) and (Data.Mode = FSEL_ALT4)) or
              ((Gpio = 5) and (Data.Mode = FSEL_ALT4)) or
              ((Gpio = 6) and (Data.Mode = FSEL_ALT4)) or
              ((Gpio = 7) and (Data.Mode = FSEL_ALT4));

      4: Ok:= ((Gpio = 8)  and (Data.Mode = FSEL_ALT4)) or
              ((Gpio = 9)  and (Data.Mode = FSEL_ALT4)) or
              ((Gpio = 10) and (Data.Mode = FSEL_ALT4)) or
              ((Gpio = 11) and (Data.Mode = FSEL_ALT4));

      5: Ok:= ((Gpio = 12) and (Data.Mode = FSEL_ALT4)) or
              ((Gpio = 13) and (Data.Mode = FSEL_ALT4)) or
              ((Gpio = 14) and (Data.Mode = FSEL_ALT4)) or
              ((Gpio = 15) and (Data.Mode = FSEL_ALT4));
    end;

    If Ok then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result)-1]:= Gpio;
    end;
  end;
end;

// ------------------------------------------------------------------------

function TPiGpioUart.TransmitUartData(UartNo: Integer; Const Data; Count: Integer): Boolean;
begin
  if TxRxThread[UartNo] = nil then
  begin
    FLastErrorStr:= 'Uart not initialized';
    Exit(False);
 end;

  if TxRxThread[UartNo].TxActive then
  begin
    FLastErrorStr:= 'Uart TX are busy';
    Exit(False);
  end;

  Result:= True;

  SetLength(TxRxThread[UartNo].TxBuffer, Count);
  Move(Data, TxRxThread[UartNo].TxBuffer[0], Count);
end;

// ------------------------------------------------------------------------


// ------------------------------------------------------------------------
//
// Transmit / Receive Thread
//
// ------------------------------------------------------------------------

procedure TTxRxThread.Execute;
begin
  while not Terminated do
  begin
    RunRx;
    RunTx;

    if (RxActive or TxActive)
      then Sleep(0)
      else Sleep(SleepMs);
  end;
end;

// ------------------------------------------------------------------------

procedure TTxRxThread.UartStatusProc;
begin
  // Callback to Main
  if Assigned(UartStatusEvent) then
    UartStatusEvent(UartNo, UartStatus);
end;

// ------------------------------------------------------------------------

procedure TTxRxThread.DataReceived;
begin
  // Callback to Main
  if Assigned(RxDataEvent) then
    RxDataEvent(UartNo, RxBuffer[0], Length(RxBuffer));
end;

// ------------------------------------------------------------------------

procedure TTxRxThread.RunRx;
var
  Data: LongWord;

begin
  // Check for RX data
  if (pFR^ and UART_FR_RXFE) = 0 then
  begin
    while (pFR^ and UART_FR_RXFE) = 0 do
    begin
      Data:= pDR^;                // Read Data and Status

      // Check for errors
      if Data and $0F00 <> 0 then
      begin
        UartStatus:= Data and $0F00;
        Synchronize(@UartStatusProc);
      end
      else
      begin
        SetLength(RxBuffer, Length(RxBuffer)+1);
        RxBuffer[Length(RxBuffer)-1]:= Data and UART_DR_DATA;
      end;
    end;

    // Reset Timeout counter
    RxActive:= True;
    RxTimer:= NowMicroSec;
  end;

  // When RxTimeout -> deliver Rx Data
  if RxActive then
  begin
    if NowMicroSec > RxTimer + RxTimeOut then
    begin
      Synchronize(@DataReceived);
      SetLength(RxBuffer, 0);
      RxActive:= False;
    end;
  end;
end;

// ------------------------------------------------------------------------

procedure TTxRxThread.RunTx;
begin
  // Check for TX data
  if TxActive then
  begin
    // TX Active, Check/Wait for finish
    if (TxIndex >= Length(TxBuffer)) then
    begin
      // End of TxBuffer, wait for FIFO empty
      if (pFR^ and UART_FR_TXFE) <> 0 then
      begin
        // TX done and FIFO empty
        TxActive:= False;
        SetLength(TxBuffer, 0);

        // Callback ....
        UartStatus:= UART_STAT_TX_DONE;
        Synchronize(@UartStatusProc);
      end;
    end
    else
    begin
      // Still data in TxBuffer, fill FIFO buffer
      if ((pFR^ and UART_FR_TXFF) = 0) and
         (TxIndex < Length(TxBuffer)) then
      begin
        pDR^:= TxBuffer[TxIndex];
        Inc(TxIndex);
      end;
    end;
  end

  else  // if TxActive then

  begin
    // Not Active. Check for new data to send
    if Length(TxBuffer) > 0 then
    begin
      TxActive:= True;
      TxIndex:= 0;

      while ((pFR^ and UART_FR_TXFF) = 0) and    // Fill Fifo
            (TxIndex < Length(TxBuffer)) do
      begin
        pDR^:= TxBuffer[TxIndex];
        Inc(TxIndex);
      end;
    end;
  end;
end;

// ------------------------------------------------------------------------



end.

