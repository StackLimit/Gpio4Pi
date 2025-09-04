program Gpio4PiCmd;

// -----------------------------------------------------------------
//
// Gpio4Pi Cmd-line is a test / debug App accessing Gpio4Pi object
//
// Still under development and therefore not quite finished
// Copyright (c) 2024-2025 Jan Andersen
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
    function ConnectedClocksToStr(Gpios: TIntArray): String;
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
  Gpin: Integer;
  Data: TGpioPin;

begin
  WriteLn;
  WriteLn('---------- All GPIOs ----------');

  Gpin:= 0;
  while PiGpio.GetGpioPinData(Gpin, Data{%H-}) do
  begin
    S:= 'GPIO ' + IntToStr(Gpin) +
        ': Mode=' + GpioModeToLongStr(PiGpio.RPiModelInfo.Cpu, Gpin, Data.Mode) +
        ', Level=' + GpioLevelToLongStr(Data.Level);

    if PiGpio.RPiModelInfo.Cpu in [PI_CPU_BCM2711,PI_CPU_BCM2712] then
      S:= S + ', Pull=' + GpioPullToLongStr(Data.Pull);

    WriteLn(S);
    Gpin:= Gpin + 1;
  end;
end;

//----------------------------------------------------------------

function TGpio4PiCmd.ConnectedClocksToStr(Gpios: TIntArray): String;
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
  Clock: TGpioClk;
  Freq: LongWord;
  Gpios: TIntArray;

begin
  WriteLn;
  WriteLn('---------- All CLOCKs ----------');
  for ClkNo:= 0 to 8 do
  begin
    if PiGpio.GetRawClockData(ClkNo, Clock{%H-}) then
    begin
      case ClkNo of
        CLK_GPIO0: S:= 'GPIO CLOCK 0:';
        CLK_GPIO1: S:= 'GPIO CLOCK 1:';
        CLK_GPIO2: S:= 'GPIO CLOCK 2:';
        CLK_GPIO3: S:= 'GPIO CLOCK 3:';
        CLK_GPIO4: S:= 'GPIO CLOCK 4:';
        CLK_GPIO5: S:= 'GPIO CLOCK 5:';
        CLK_PWM:   S:= 'PWM CLOCK:';
        CLK_UART:  S:= 'UART CLOCK:';
        CLK_PCM:   S:= 'PCM CLOCK:';
        else exit;
      end;

      // RaspberryPi 1 to RaspberryPi 4
      if  PiGpio.RPiModelInfo.Cpu in [PI_CPU_BCM2835,PI_CPU_BCM2836,PI_CPU_BCM2837,PI_CPU_BCM2711] then
      begin
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
      end;

      // RaspberryPi 5
      if  PiGpio.RPiModelInfo.Cpu = PI_CPU_BCM2712 then
      begin
        // Enable bit (B11)
        S:= S + ' Enable=' + LongToYesNo((Clock.Control shr 11) and 1);

        // Source (B5-B8)
        S:= S + ', Source=0x' + IntToHex((Clock.Control and RP1_CLK_CTRL_SRCMASK) shr 5, 2);

        // Control register
        S:= S + ', Control=0x' + IntToHex(Clock.Control, 8);
      end;

      // The rest are the same for all Raspberry Pi's
      // Divisor
      S:= S + ', Div=' + IntToStr(Clock.Divisor);

      // Fraction
      S:= S + ', Frac=' + IntToStr(Clock.Fract);

      // Calculate Frequency.
      Freq:= PiGpio.GetClockFrequency(ClkNo);
      S:= S + ', Freq=' + IntToStr(Freq) + ' Hz';
      WriteLn(S);

      // Print connected GPIOs
      if ClkNo in [CLK_GPIO0..CLK_GPIO5] then
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
  Gpios: TIntArray;

  // Raspberry Pi 1-4
procedure ShowOnePwmPi1_4(Group: Integer);
begin
  if not PiGpio.GetRawPwmData(Group, Pwm) then exit;

  // Channel 1 Control
  if Pwm.Cpu = PI_CPU_BCM2711
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
  Gpios:= PiGpio.GetGpiosForPwm((Group * 4) + 0);
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
  Gpios:= PiGpio.GetGpiosForPwm((Group * 4) + 1);
  S:= ConnectedClocksToStr(Gpios);
  WriteLn(S);
  WriteLn;
end;

// Raspberry Pi 5
procedure ShowOnePwmPi5(Group: Integer);
var
  Chan: Integer;

begin
  if not PiGpio.GetRawPwmData(Group, Pwm) then exit;

  S:= 'PWM ' + IntToStr(Group) + ' Common Data: ' +
      'GlobalCtrl=0x' + IntToHex(Pwm.Rp1GlobCtrl, 8) +
      ', FifoCtrl=0x' + IntToHex(Pwm.Rp1FifoCtrl, 8) +
      ', CommonRange=0x' + IntToHex(Pwm.Rp1ComRange, 8);
  WriteLn(S);

  S:= '                   ' +
      'CommonDuty=0x' + IntToHex(Pwm.Rp1ComDuty, 8) +
      ', DutyFifo=0x' + IntToHex(Pwm.Rp1DutyFifo, 8);
  WriteLn(S);
  WriteLn;

  for Chan:= 0 to 3 do
  begin
    S:= 'PWM ' + IntToStr(Group) + '_' + IntToStr(Chan) + ': Mode=';
    case Pwm.Rp1Channels[Chan].Rp1Control and $07 of
      $00: S:= S + 'Off';
      $01: S:= S + 'Trailing-edge mark-space';
      $02: S:= S + 'Phase-correct mark-space';
      $03: S:= S + 'Pulse-density encoded';
      $04: S:= S + 'MSB Serialiser';
      $05: S:= S + 'Pulse position modulated';
      $06: S:= S + 'Leading-edge mark-space';
      $07: S:= S + 'LSB Serialiser';
    end;

    S:= S + ', Control=0x' + IntToHex(Pwm.Rp1Channels[Chan].Rp1Control, 8) +
            ', Div=' + IntToStr(Pwm.Rp1Channels[Chan].Rp1Range) +
            ', Duty=' + IntToStr(Pwm.Rp1Channels[Chan].Rp1Duty) +
            ', Phase=' + IntToStr(Pwm.Rp1Channels[Chan].Rp1Phase);

    WriteLn(S);

    // Print connected GPIOs
    Gpios:= PiGpio.GetGpiosForPwm((Group * 4) + Chan);
    S:= ConnectedClocksToStr(Gpios);
    WriteLn(S);
    WriteLn;
  end;
end;

// ShowAllPwm entry..........
begin
  WriteLn;
  WriteLn('---------- All PWMs ----------');
  if PiGpio.RPiModelInfo.Cpu = PI_CPU_BCM2712 then
  begin
    // Raspberry Pi 5
    ShowOnePwmPi5(PWM_GROUP_0);
    ShowOnePwmPi5(PWM_GROUP_1);
  end
  else
  begin
    // Raspberry Pi 1-4
    ShowOnePwmPi1_4(PWM_GROUP_0);
    if PiGpio.RPiModelInfo.Cpu = PI_CPU_BCM2711 then ShowOnePwmPi1_4(PWM_GROUP_1);
  end;
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
     (Pos(','+Params[2]+',', ',off,in,out,alt0,alt1,alt2,alt3,alt4,alt5,alt6,alt7,alt8,clk,pwm,') > 0) then
  begin
    Ok:= False;
    case Params[2] of
      'off':  Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_GPIO_OFF);
      'in':   Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_INPUT);
      'out':  Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_OUTPUT);
      'alt0': Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_ALT0);
      'alt1': Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_ALT1);
      'alt2': Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_ALT2);
      'alt3': Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_ALT3);
      'alt4': Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_ALT4);
      'alt5': Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_ALT5);
      'alt6': Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_ALT6);
      'alt7': Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_ALT7);
      'alt8': Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_ALT8);
      'clk':  Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_GPIO_CLOCK);
      'pwm':  Ok:= PiGpio.SetPinMode(StrToIntDef(Params[3], -1), PM_PWMOUT_MS);
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
  StopOnException:= True;
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
  WriteLn('  show ..................: Show all GPIO, Clock and PWM information');
  WriteLn('  mode <mode gpio> ......: Set GPIO to Mode. Mode=off,in,out,alt0..alt8,clk,pwm');
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
  Application:= TGpio4PiCmd.Create(nil);
  Application.Title:= 'Gpio4PiCmd';
  Application.Run;
  Application.Free;
end.

