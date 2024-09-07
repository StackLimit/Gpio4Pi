program GpioPiClient;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  cmem,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, UdpRxTxClient, GpioPiCli, Common, GPIOcheckbox, SetupForm,
  GpioPiTxtCmd, ClockForm, PwmForm, OverviewForm;


{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormSetup, FormSetup);
  Application.CreateForm(TFormClocks, FormClocks);
  Application.CreateForm(TFormPwm, FormPwm);
  Application.CreateForm(TFormOverview, FormOverview);
  Application.Run;
end.

