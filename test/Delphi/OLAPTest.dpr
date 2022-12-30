program OLAPTest;

uses
  Vcl.Forms,
  frmDelphiOLAP_u in 'frmDelphiOLAP_u.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
