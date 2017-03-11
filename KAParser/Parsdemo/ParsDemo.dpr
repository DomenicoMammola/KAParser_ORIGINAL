program ParsDemo;

uses
  Forms,
  main in 'main.pas' {Form1},
  CTParser in 'CTParser.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
