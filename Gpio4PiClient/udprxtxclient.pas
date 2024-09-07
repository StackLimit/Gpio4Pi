unit UdpRxTxClient;

// ----------------------------------------------------------------
//
// UDP Rx / Tx unit for Gpio4Pi Client
//
// Takes care of UDP communication between Gpio4Pi and Gpio4PiClient.
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
  TRxDataEvent = procedure(From, Data: String) of object;

Var
  RxDataEvent: TRxDataEvent;


  // Variable for where data is sent and received.
  // Can be overwritten from Setup/ConfigFile
  ToAddr: String = '127.0.0.1';   // Normal Localhost

  // NB: These port numbers must correspond to those of the Gpio4Pi
  TxPortNr: Word = 51001;         // Data is sent on this port
  RxPortNr: Word = 51000;         // Data is received on this port



procedure UdpRxTxInit;
procedure UdpRxTxClose;
procedure UdpRxTxSend(TxData: String);


implementation

Uses MainForm, Common;


Type
  // UDP Rx/Tx buffer. NB: Must be the same size as in ..\UdpRxTx.pas
  TUdpRxTxBuffer = record
    Antal: Integer;
    Buffer: Array[1..500] Of Char;
  end;

  TSocketData = record
    Socket: Longint;
    PortNr: Word;
    Addr:   TInetSockAddr;
  end;

  // Receiving thread as fprecvfrom() is blocking
  TRxTraad = class(TThread)
   protected
     procedure Execute; override;
   private
     DataFrom: TInetSockAddr;
     RxBuffer: TUdpRxTxBuffer;
     procedure DataReceived;
   public
   published
 end;


Var
  TxSocket: TSocketData;
  RxSocket: TSocketData;
  RxTraad:  TRxTraad;


// -----------------------------------------------
// Print messages in MainForm
// -----------------------------------------------
procedure WrtLog(S: String);
begin
  FormMain.WrtLog(S);
end;


// -----------------------------------------------
// UDP Rx/Tx must be initialized before it can be used
// -----------------------------------------------
procedure UdpRxTxInit;
var
  CfgFil: TIniFile;

begin
  if TxSocket.Socket <> 0 then exit;

  // Load setup from Config File
  CfgFil:= TIniFile.Create(GetConfigFilNavn);
  ToAddr:=   CfgFil.ReadString(SectionNet, IdentToAddr, ToAddr);
  TxPortNr:= CfgFil.ReadInteger(SectionNet, IdentTxPortNr, TxPortNr);
  RxPortNr:= CfgFil.ReadInteger(SectionNet, IdentRxPortNr, RxPortNr);
  CfgFil.Destroy;

  // Create TX and RX Sockets
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
         ', Socket number:  ' + IntToStr(TxSocket.Socket));
  WrtLog('TX sending data to ' + ToAddr);

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
         ', Socket number:  ' + IntToStr(RxSocket.Socket));
  WrtLog('RX receiving data from everywhere');

  // Start RX thread
  RxTraad:= TRxTraad.Create(true);
  RxTraad.FreeOnTerminate:= false;  // NOT TRUE, then WaitFor hangs forever
  RxTraad.Start;
end;




// -----------------------------------------------
// STOP UDP Rx/Tx network
// -----------------------------------------------
procedure UdpRxTxClose;
begin
  if TxSocket.Socket = 0 then exit;

  // Stop RX thread
  RxTraad.Terminate;

  fpshutdown(TxSocket.Socket, 2);
  fpshutdown(RxSocket.Socket, 2);

  CloseSocket(TxSocket.Socket);
  CloseSocket(RxSocket.Socket);

  RxTraad.WaitFor;
  RxTraad.Free;      // FreeOnTerminate is false, otherwise WaitFor hangs forever

  fillchar(TxSocket, sizeof(TxSocket), 0);
  fillchar(RxSocket, sizeof(RxSocket), 0);
end;




// -----------------------------------------------
// UDP Rx/Tx Send data to receiver
// -----------------------------------------------
procedure UdpRxTxSend(TxData: String);
var
  Tx: TUdpRxTxBuffer;
  AntTx: ssize_t;

begin
  if TxSocket.Socket = 0 then exit;
  if Length(TxData) > SizeOf(Tx.Buffer) then exit;

  FillChar(Tx{%H-}, SizeOf(Tx), 0);
  Move(TxData[1], Tx.Buffer, Length(TxData));
  Tx.Antal:= Length(TxData);
  AntTx:= fpsendto(TxSocket.Socket, @Tx.Buffer, Tx.Antal,
                   0, @TxSocket.Addr, sizeof(TxSocket.Addr));

  WrtLog('TxData, Count: ' + IntToStr(AntTx) + ', Data: ' + TxData);
end;



// -----------------------------------------------------------------------
// RX Thread
// -----------------------------------------------------------------------

procedure TRxTraad.DataReceived;
var
  Rx: String;

begin
  // Callback to Main
  if not Assigned(RxDataEvent) then exit;

  SetLength(Rx{%H-}, RxBuffer.Antal);
  Move(RxBuffer.Buffer, Rx[1], RxBuffer.Antal);

  RxDataEvent(NetAddrToStr(DataFrom.sin_addr), Rx);
end;


procedure TRxTraad.Execute;
var
  FromLen: TSockLen;

begin
  while not Terminated do
  begin
    fillchar(DataFrom, sizeof(DataFrom), 0);
    FromLen:= sizeof(DataFrom);

    // Wait for received data (blocking)
    RxBuffer.Antal:= fprecvfrom(RxSocket.Socket,
                                @RxBuffer.Buffer, SizeOf(RxBuffer.Buffer),
                                0, @DataFrom, @Fromlen);
    if (RxBuffer.Antal > 0) then
    begin
      Synchronize(@DataReceived);
    end;
  end;
end;



Initialization
  fillchar(TxSocket, sizeof(TxSocket), 0);
  fillchar(RxSocket, sizeof(RxSocket), 0);
  RxDataEvent:= nil;


Finalization
  UdpRxTxClose;


end.

