unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonCalc: TButton;
    EditDesired: TEdit;
    EditFrac: TEdit;
    EditDiv: TEdit;
    EditPiFreq: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabResult2: TLabel;
    LabResult1: TLabel;
    procedure ButtonCalcClick(Sender: TObject);
  private

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}



procedure TFormMain.ButtonCalcClick(Sender: TObject);
var
  PiFreq,DivI,DivF,Desired: Integer;
  Freq: double;

begin
  PiFreq:= StrToIntDef(EditPiFreq.Text, -1);
  if PiFreq = -1 then exit;

  DivI:= StrToIntDef(EditDiv.Text, -1);
  if DivI = -1 then exit;

  DivF:= StrToIntDef(EditFrac.Text, -1);
  if DivF = -1 then exit;

  // Calculate - Freq:= Source / ( DIVI + DIVF / 1024 )
  Freq:= PiFreq / ( DivI + (DivF / 1024) );

  LabResult1.Caption:= 'Result: ' + IntToStr(Trunc(Freq));

  // Try calculate desired freq
  Desired:= StrToIntDef(EditDesired.Text, -1);
  if Desired = -1 then exit;

  DivI:= PiFreq div Desired;
  DivF:= Trunc(((PiFreq / Desired) - DivI) * 1024);

  LabResult2.Caption:= 'DivI: ' + IntToStr(DivI) + ', DivF: ' + IntToStr(DivF);
end;



end.

