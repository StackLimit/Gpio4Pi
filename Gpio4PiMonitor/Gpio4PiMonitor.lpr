program Gpio4PiMonitor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, RasPiMem, Common, GPIOcheckbox, ClockForm, PwmForm,
  OverviewForm, GpioDefs, Gpio4Pi, GpioExtraForm, GpioExtraStuff
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormClocks, FormClocks);
  Application.CreateForm(TFormPwm, FormPwm);
  Application.CreateForm(TFormOverview, FormOverview);
  Application.CreateForm(TFormGpioExtra, FormGpioExtra);
  Application.Run;
end.

