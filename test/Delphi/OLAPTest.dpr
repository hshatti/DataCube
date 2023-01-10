program OLAPTest;

uses
  Vcl.Forms,
  frmDelphiOLAP_u in 'frmDelphiOLAP_u.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Onyx Blue');
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
