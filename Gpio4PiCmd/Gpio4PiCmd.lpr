program Gpio4PiCmd;

// -----------------------------------------------------------------
//
// Gpio4Pi Cmd-line is a test / debug App accessing Gpio4Pi object
//
// Still under development and therefore not quite finished
// Copyright (c) 2024 Jan Andersen
// -----------------------------------------------------------------

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  GpioDefs, Gpio4Pi, GpioExtraStuff;


type
  // TGpio4PiCmd
  TGpio4PiCmd = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    PiGpio: TPiGpio;
    S: String;
    function ConnectedClocksToStr(Gpios: IntArray): String;
    procedure ShowAllGpio;
    procedure ShowAllClock;
    procedure ShowAllPwm;
    procedure ShowAll;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;







//----------------------------------------------------------------

procedure TGpio4PiCmd.ShowAllGpio;
var
  I:    Integer;
  Data: TGpioPin;

begin
  WriteLn;
  WriteLn('---------- All GPIOs ----------');
  for I:= 0 to PiGpio.GpioHighestPin do
  begin
    if PiGpio.GetGpioPinData(I, Data{%H-}) then
    begin
      S:= 'GPIO ' + IntToStr(I) +
          ': Mode=' + GpioModeToLongStr(PiGpio.RPiModelInfo.Cpu, I, Data.Mode) +
          ', Level=' + GpioLevelToLongStr(Data.Level);

      if PiGpio.RPiModelInfo.Cpu = PI_CPU_BCM2711 then
        S:= S + ', Pull=' + GpioPullToLongStr(Data.Pull);

      WriteLn(S);
    end;
  end;
end;

//----------------------------------------------------------------

function TGpio4PiCmd.ConnectedClocksToStr(Gpios: IntArray): String;
var
  I: Integer;
  Ret: String;

begin
  Ret:= '  Connected GPIOs: ';
  if Length(Gpios) = 0 then
  begin
    Ret:= Ret + 'None';
    Exit(Ret);
  end;

  for I:= 0 to Length(Gpios)-1 do
  begin
    if I > 0 then Ret:= Ret + ',';
    Ret:= Ret + IntToStr(Gpios[I]);
  end;

  Exit(Ret);
end;

//----------------------------------------------------------------

procedure TGpio4PiCmd.ShowAllClock;
var
  ClkNo: Integer;
  Clock: TClock;
  Di,Fr,Freq: LongWord;
  Gpios: IntArray;

begin
  WriteLn;
  WriteLn('---------- All CLOCKs ----------');
  for ClkNo:= 0 to 4 do
  begin
    if PiGpio.GetRawClockData(ClkNo, Clock{%H-}) then
    begin
      case ClkNo of
        CLK_GPIO0: S:= 'GPIO CLOCK 0:';
        CLK_GPIO1: S:= 'GPIO CLOCK 1:';
        CLK_GPIO2: S:= 'GPIO CLOCK 2:';
        CLK_PCM:   S:= 'PCM CLOCK:';
        CLK_PWM:   S:= 'PWM CLOCK:';
        else exit;
      end;

      // Enable bit (B4)
      S:= S + ' Enable=' + LongToYesNo((Clock.Control shr 4) and 1);

      // Source (B0-B3)
      S:= S + ', Source=';
      case Clock.Control and $0F of
        1:   S:= S + 'Oscillator';
        2:   S:= S + 'Testdebug0';
        3:   S:= S + 'Testdebug1';
        4:   S:= S + 'PLLA per';
        5:   S:= S + 'PLLC per';
        6:   S:= S + 'PLLD per';
        7:   S:= S + 'HDMI aux';
        else S:= S + 'GND';
      end;

      // Kill bit (B5)
      S:= S + ', Kill=' + LongToYesNo((Clock.Control shr 5) and 1);

      // Flip bit (B8)
      S:= S + ', Flip=' + LongToYesNo((Clock.Control shr 8) and 1);

      // MASH (B9-B10)
      S:= S + ', MASH=';
      case (Clock.Control shr 9) and 3 of
        0: S:= S + 'Integer Division';
        1: S:= S + '1-stage MASH';
        2: S:= S + '2-stage MASH';
        3: S:= S + '3-stage MASH';
      end;

      // Divisor (B12-B23)
      Di:= (Clock.Divisor shr 12) and $FFF;
      S:= S + ', Div=' + IntToStr(Di);

      // Fraction (B0-B11)
      Fr:= Clock.Divisor and $FFF;
      S:= S + ', Frac=' + IntToStr(Fr);

      // Calculate Frequency.
      Freq:= PiGpio.GetClockFrequency(ClkNo);
      S:= S + ', Freq=' + IntToStr(Freq) + 'Hz';
      WriteLn(S);

      // Print connected GPIOs
      if ClkNo in [0..2] then
      begin
        Gpios:= PiGpio.GetGpiosForGpioClock(ClkNo);
        S:= ConnectedClocksToStr(Gpios);
        WriteLn(S);
      end;

      WriteLn;
    end;
  end;
end;

//----------------------------------------------------------------

procedure TGpio4PiCmd.ShowAllPwm;
var
  Pwm: TPwmData;
  Gpios: IntArray;

procedure ShowOnePwm(Group: Integer);
begin
  if not PiGpio.GetRawPwmData(Group, Pwm) then exit;

  // Channel 1 Control
  if PiGpio.RPiModelInfo.Cpu = PI_CPU_BCM2711
    then S:= 'PWM ' + IntToStr(Group) + '_0: '
    else S:= 'PWM 0: ';

  S:= S +
    'Enable=' + LongToYesNo(Pwm.Control and PWM0_ENABLE) +
    ', Serial=' + LongToYesNo(Pwm.Control and PWM0_SERIAL) +
    ', Repeat=' + LongToYesNo(Pwm.Control and PWM0_REPEATFF) +
    ', Silence=' + LongToYesNo(Pwm.Control and PWM0_SILENCE) +
    ', Polarity=' + LongToYesNo(Pwm.Control and PWM0_REVPOLAR) +
    ', UseFifo=' + LongToYesNo(Pwm.Control and PWM0_USEFIFO) +
    ', M/S mode=' + LongToYesNo(Pwm.Control and PWM0_MS_MODE) +

  // Channel 1 Data
    ', Range=' + IntToStr(Pwm.Channels[0].Range) +
     ', Data=' + IntToStr(Pwm.Channels[0].Data);
  WriteLn(S);

  // Print connected GPIOs
  Gpios:= PiGpio.GetGpiosForPwm((Group * 2) + 0);
  S:= ConnectedClocksToStr(Gpios);
  WriteLn(S);
  WriteLn;

  // Channel 2 Control
  if PiGpio.RPiModelInfo.Cpu = PI_CPU_BCM2711
    then S:= 'PWM ' + IntToStr(Group) + '_1: '
    else S:= 'PWM 1: ';

  S:= S +
    'Enable=' + LongToYesNo(Pwm.Control and PWM1_ENABLE) +
    ', Serial=' + LongToYesNo(Pwm.Control and PWM1_SERIAL) +
    ', Repeat=' + LongToYesNo(Pwm.Control and PWM1_REPEATFF) +
    ', Silence=' + LongToYesNo(Pwm.Control and PWM1_SILENCE) +
    ', Polarity=' + LongToYesNo(Pwm.Control and PWM1_REVPOLAR) +
    ', UseFifo=' + LongToYesNo(Pwm.Control and PWM1_USEFIFO) +
    ', M/S mode=' + LongToYesNo(Pwm.Control and PWM1_MS_MODE) +

  // Channel 2 Data
    ', Range=' + IntToStr(Pwm.Channels[1].Range) +
    ', Data=' + IntToStr(Pwm.Channels[1].Data);
  WriteLn(S);

  // Print connected GPIOs
  Gpios:= PiGpio.GetGpiosForPwm((Group * 2) + 1);
  S:= ConnectedClocksToStr(Gpios);
  WriteLn(S);
  WriteLn;
end;

begin
  WriteLn;
  WriteLn('---------- All PWMs ----------');
  ShowOnePwm(0);
  if PiGpio.RPiModelInfo.Cpu = PI_CPU_BCM2711 then ShowOnePwm(1);
end;

//----------------------------------------------------------------

procedure TGpio4PiCmd.ShowAll;
begin
  ShowAllGpio;
  ShowAllClock;
  ShowAllPwm;
end;

//----------------------------------------------------------------

procedure TGpio4PiCmd.DoRun;
var
  Ok: Boolean;
  Val: Byte;
  PiInfo: TRPiModelInfo;

begin
  PiGpio:= TPiGpio.Create;
  if PiGpio = Nil then
  begin
    WriteLn('GPIO Initialized FAIL');
    Terminate;
    Exit;
  end;

  S:= 'GPIO Initialized OK, Using ';
  if PiGpio.UsingGpioMem
    then S:= S + '/dev/gpiomem'
    else S:= S + '/dev/mem';
  WriteLn(S);

  if (ParamCount = 1) and (Params[1] = 'info') then
  begin
    PiInfo:= PiGpio.RPiModelInfo;
    WriteLn('Raspberry Pi Info:');
    WriteLn('------------------');
    WriteLn(' Revision: ' + PiRevisionNames[PiInfo.Rev]);
    WriteLn(' Model...: ' + PiModelNames[PiInfo.Model]);
    WriteLn(' CPU.....: ' + PiProcessor[PiInfo.Cpu]);
    WriteLn(' Maker...: ' + PiMakerNames[PiInfo.Maker]);
    WriteLn(' Memory..: ' + IntToStr(PiMemorySize[PiInfo.Mem]));
    WriteLn('------------------');
  end

  else

  if (ParamCount = 1) and (Params[1] = 'show') then
  begin
    ShowAll;
  end

  else

  if (ParamCount = 3) and (Params[1] = 'mode') and
     (Pos(','+Params[2]+',', ',in,out,alt0,alt1,alt2,alt3,alt4,alt5,') > 0) then
  begin
    Ok:= False;
    case Params[2] of
      'in':   Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_INPUT);
      'out':  Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_OUTPUT);
      'alt0': Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_ALT0);
      'alt1': Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_ALT1);
      'alt2': Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_ALT2);
      'alt3': Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_ALT3);
      'alt4': Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_ALT4);
      'alt5': Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_ALT5);
    end;
    if Ok
      then WriteLn('Set Mode OK')
      else WriteLn('Set Mode Error: ' + PiGpio.LastErrorStr);
  end

  else

  if (ParamCount = 3) and (Params[1] = 'pull') and
     (Pos(','+Params[2]+',', ',none,up,down,') > 0) then
  begin
    Ok:= False;
    case Params[2] of
      'none': Ok:= PiGpio.SetPullMode(StrToIntDef(Params[3], -1), PUD_OFF);
      'up':   Ok:= PiGpio.SetPullMode(StrToIntDef(Params[3], -1), PUD_UP);
      'down': Ok:= PiGpio.SetPullMode(StrToIntDef(Params[3], -1), PUD_DOWN);
    end;
    if Ok
      then WriteLn('Set Pull-Up/Down OK')
      else WriteLn('Set Pull-Up/Down Error: ' + PiGpio.LastErrorStr);
  end

  else

  if (ParamCount = 3) and (Params[1] = 'write') and
     (Pos(','+Params[2]+',', ',0,1,') > 0) then
  begin
    Ok:= False;
    case Params[2] of
      '0': Ok:= PiGpio.GpioWrite(StrToIntDef(Params[3], -1), PIN_LOW);
      '1': Ok:= PiGpio.GpioWrite(StrToIntDef(Params[3], -1), PIN_HIGH);
    end;
    if Ok
      then WriteLn('GPIO Write OK')
      else WriteLn('GPIO Write Error: ' + PiGpio.LastErrorStr);
  end

  else

  if (ParamCount = 2) and (Params[1] = 'read') then
  begin
    Val:= PiGpio.GpioRead(StrToIntDef(Params[2], -1));
    WriteLn('GPIO Read, Value = ' + IntToStr(Val));
  end

  else

  if (ParamCount = 3) and (Params[1] = 'clock') then
  begin
    if PiGpio.SetGpioClock(StrToIntDef(Params[3], -1), StrToIntDef(Params[2], 0))
      then WriteLn('Set GPIO Clock OK')
      else WriteLn('Set GPIO Clock Error: ' + PiGpio.LastErrorStr);
  end

  else

  if (ParamCount = 2) and (Params[1] = 'pwmclock') then
  begin
    if PiGpio.SetPwmMasterClock(StrToIntDef(Params[2], 0))
      then WriteLn('Set PWM Clock OK')
      else WriteLn('Set PWM Clock Error: ' + PiGpio.LastErrorStr);
  end

  else

  if (ParamCount = 3) and (Params[1] = 'pwmmode') and
     (Pos(','+Params[2]+',', ',bal,ms,') > 0) then
  begin
    Ok:= False;
    case Params[2] of
      'bal': Ok:= PiGpio.SetPwmMode(StrToIntDef(Params[3], -1), PWM_MODE_BAL);
      'ms':  Ok:= PiGpio.SetPwmMode(StrToIntDef(Params[3], -1), PWM_MODE_MS);
    end;
    if Ok
      then WriteLn('Set PWM Mode OK')
      else WriteLn('Set PWM Mode Error: ' + PiGpio.LastErrorStr);
  end

  else

  if (ParamCount = 3) and (Params[1] = 'pwmrange') then
  begin
    if PiGpio.SetPwmRange(StrToIntDef(Params[3], 0), StrToIntDef(Params[2], -1))
      then WriteLn('Set PWM Range OK')
      else WriteLn('Set PWM Range Error: ' + PiGpio.LastErrorStr);
  end

  else

  if (ParamCount = 3) and (Params[1] = 'pwmduty') then
  begin
    if PiGpio.SetPwmDutyCycle(StrToIntDef(Params[3], 0), StrToIntDef(Params[2], -1))
      then WriteLn('Set PWM Duty Cycle OK')
      else WriteLn('Set PWM Duty Cycle Error: ' + PiGpio.LastErrorStr);
  end

  else

  begin
    WriteHelp;
  end;

  // stop program loop
  Terminate;
end;

//----------------------------------------------------------------

constructor TGpio4PiCmd.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

//----------------------------------------------------------------

destructor TGpio4PiCmd.Destroy;
begin
  inherited Destroy;
end;

//----------------------------------------------------------------

procedure TGpio4PiCmd.WriteHelp;
begin
  { add your help code here }
  WriteLn('Usage: ', ExtractFileName(ExeName));
  WriteLn('  info ..................: Show info about the Paspberry PI');
  WriteLn('  show ..................: Show all');
  WriteLn('  mode <mode gpio> ......: Set GPIO to Mode. Mode=in,out,alt0..alt5');
  WriteLn('  pull <pull gpio> ......: Set GPIO Pull-Up/Down. Pull=none,up,down');
  WriteLn('  write <val gpio> ......: Write a Value to GPIO. Value=0,1');
  WriteLn('  read <gpio> ...........: Read a Value from GPIO');
  WriteLn('  clock <freq gpio> .....: Set GPIO Clock in Hz');
  WriteLn('  pwmclock <freq> .......: Set PWM Master Clock in Hz');
  WriteLn('  pwmmode <mode gpio> ...: Set PWM Mode for GPIO x. Mode=bal,ms');
  WriteLn('  pwmrange <range gpio> .: Set PWM Range for GPIO x');
  WriteLn('  pwmduty <duty gpio> ...: Set PWM Duty Cycle for GPIO x');
end;

//----------------------------------------------------------------

var
  Application: TGpio4PiCmd;
begin
  Application:=TGpio4PiCmd.Create(nil);
  Application.Title:='A command line GPIO tool';
  Application.Run;
  Application.Free;
end.

