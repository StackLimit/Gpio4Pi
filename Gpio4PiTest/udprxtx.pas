unit UdpRxTx;

// ----------------------------------------------------------------
//
// UDP Rx / Tx unit for Gpio4Pi under Windows
//
// Takes care of UDP communication between Gpio and GpioPiClient.
//
// Receiver runs in a thread since fprecvfrom() is blocking.
// When data is received, the routine is called in RxDataEvent
// if it is Assigned.
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// ----------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Sockets
{$IFDEF Unix}
  , UnixType
{$ENDIF}
  ;



// -----------------------------------------------
// Callback when Data is received
// -----------------------------------------------
Type
//  TRxDataEvent = procedure(Data: String) of object;
  TRxDataEvent = procedure(Data: String);



Var
  RxDataEvent: TRxDataEvent;


procedure UdpRxTxInit;
procedure UdpRxTxSend(Data: String);



implementation

Var
  // Variable for where data is sent and received.
  // Can be overwritten from config file
  // ToAddr are overwritten when receiving data from another IP address
  ToAddr: String = '127.0.0.1';    // Normal Localhost
  TxPortNr: Word = 51000;          // Data is sent on this port
  RxPortNr: Word = 51001;          // Data is received on this port


Type
  // UDP Rx/Tx buffer. There must be room for Full GPIO Map
  TUdpRxTxBuffer = record
    Count:  Integer;
    Buffer: Array[1..1000] Of Char;
  end;

  TSocketData = record
    Socket: Longint;
    PortNo: Word;
    Addr:   TInetSockAddr;
  end;

  // Receiving thread as fprecvfrom() is blocking
  TRxTraad = class(TThread)
   protected
     procedure Execute; override;
   private
     procedure DataReceived;
   public
   published
 end;



Var
  TxSocket:  TSocketData;
  RxSocket:  TSocketData;
  RxTraad:   TRxTraad;
  ConfigFil: TIniFile;
  DebugLog:  Boolean;

  // Receiver buffer from thread is located locally here, and NOT in the thread object
  // The reason is that Synchronize does not work in a DLL, so we call directly
  // into the main module - and hope it goes well... Hmmmmm...
  RxBuffer: TUdpRxTxBuffer;


// -----------------------------------------------
// Write messages to a Log file
// -----------------------------------------------
procedure WrtLog(S: String);
const LogFil = 'PiGpioLog.txt';
var F: TextFile;
begin
  if not DebugLog then exit;

  Assign(F, LogFil);
  if FileExists(LogFil)
    then Append(F)
    else Rewrite(F);
  WriteLn(F, S);
  Close(F);
end;


// -----------------------------------------------
// UDP Rx/Tx must be initialized before it can be used
// -----------------------------------------------
procedure UdpRxTxInit;
begin
  if TxSocket.Socket <> 0 then exit;

  WrtLog('-------------------------------------------' + #13#10 +
        'UDP RxTx Initializing');

  TxSocket.Addr.sin_family:= AF_INET;
  TxSocket.Addr.sin_port:= htons(TxPortNr);
  TxSocket.Addr.sin_addr:= StrToNetAddr(ToAddr);

  RxSocket.Addr.sin_family:= AF_INET;
  RxSocket.Addr.sin_port:= htons(RxPortNr);
  RxSocket.Addr.sin_addr.s_addr:= INADDR_ANY;  // Any Address

  // Create Datagram Socket UDP
  TxSocket.Socket:= fpSocket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
  if TxSocket.Socket = -1 then
  begin
    WrtLog('TX Socket Error ' + IntToStr(SocketError));
    TxSocket.Socket:= 0;
    Exit;
  end;
  WrtLog('TX Socket Open Port ' + IntToStr(TxPortNr) +
        ', Socket Number:  ' + IntToStr(TxSocket.Socket));
  WrtLog('TX Sending Data To ' + ToAddr);

  RxSocket.Socket:= fpSocket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
  if RxSocket.Socket = -1 then
  begin
    WrtLog('RX Socket Error ' + IntToStr(SocketError));
    RxSocket.Socket:= 0;
    Exit;
  end;

  // Bind RX Socket
  if fpBind(RxSocket.Socket, @RxSocket.Addr, sizeof(RxSocket.Addr)) = -1 then
   begin
     WrtLog('RX Socket Bind Error');
     RxSocket.Socket:= CloseSocket(RxSocket.Socket);
     Exit;
   end;

  WrtLog('RX Socket Open Port ' + IntToStr(RxPortNr) +
        ', Socket Number:  ' + IntToStr(RxSocket.Socket));
  WrtLog('RX Receiving Data From Everywhere');

  // Start RX thread
  RxTraad:= TRxTraad.Create(true);
  RxTraad.FreeOnTerminate:= true;
  RxTraad.Start;
end;



// -----------------------------------------------
// UDP Rx/Tx Send data to receiver
// -----------------------------------------------
procedure UdpRxTxSend(Data: String);
var
  Tx: TUdpRxTxBuffer;
  CntTx: ssize_t;

begin
  if TxSocket.Socket = 0 then exit;
  if Length(Data) > SizeOf(Tx.Buffer) then exit;

  // Load Data
  FillChar(Tx, SizeOf(Tx), 0);
  Move(Data[1], Tx.Buffer, Length(Data));
  Tx.Count:= Length(Data);

  // Load ToAddress
  TxSocket.Addr.sin_addr:= StrToNetAddr(ToAddr);

  CntTx:= fpsendto(TxSocket.Socket, @Tx.Buffer, Tx.Count,
                   0, @TxSocket.Addr, sizeof(TxSocket.Addr));

  WrtLog('Data Send To ' + ToAddr + ', Count: ' + IntToStr(CntTx) + ', Data: ' + Data);
end;



// -----------------------------------------------------------------------
// RX Thread
// -----------------------------------------------------------------------

procedure TRxTraad.DataReceived;
var
  Rx: String;

begin
  // Callback til Main
  if not Assigned(RxDataEvent) then exit;

  Rx:= '';
  SetLength(Rx, RxBuffer.Count);
  Move(RxBuffer.Buffer, Rx[1], RxBuffer.Count);

  WrtLog('Data Received From ' + ToAddr + ', Data: ' + Rx);

  RxDataEvent(Rx);
end;


procedure TRxTraad.Execute;
var
  From:    TInetSockAddr;
  FromLen: TSockLen;
  TmpAdr: String;

begin
  while not Terminated do
  begin
    fillchar(From, sizeof(From), 0);
    FromLen:= sizeof(from);

    // Wait for received data (blocking)
    RxBuffer.Count:= fprecvfrom(RxSocket.Socket,
                                @RxBuffer.Buffer, SizeOf(RxBuffer.Buffer),
                                0, @From, @FromLen);
    if (RxBuffer.Count > 0) then
    begin
      // Save the IP address in ToAddr so that data can be sent back to the sender
      if FromLen = SizeOf(From) then
      begin
        TmpAdr:= NetAddrToStr(From.sin_addr);
        if TmpAdr <> ToAddr then
        begin
          WrtLog('SendTo Address changed from ' + ToAddr + ' to ' + TmpAdr);
          ToAddr:= TmpAdr;
        end;
      end;

      Synchronize(@DataReceived);
    end;
  end;
end;



Initialization
  // Clear Workspace
  fillchar(TxSocket, sizeof(TxSocket), 0);
  fillchar(RxSocket, sizeof(RxSocket), 0);
  RxDataEvent:= nil;

  // Load config file
  ConfigFil:= TIniFile.Create('PiGpio.cfg');
  ToAddr:= ConfigFil.ReadString('UDP', 'ToAddr', ToAddr);
  TxPortNr:= ConfigFil.ReadInteger('UDP', 'TxPortNr', TxPortNr);
  RxPortNr:= ConfigFil.ReadInteger('UDP', 'RxPortNr', RxPortNr);
  DebugLog:= ConfigFil.ReadBool('LOG', 'LogEnable', False);
  ConfigFil.Destroy;


Finalization

  RxTraad.Terminate;


end.

