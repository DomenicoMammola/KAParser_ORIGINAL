unit main;

interface
{$M 16384,1048576}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  CTParser, ToolWin, ComCtrls, Menus, Spin;

type
  TForm1 = class(TForm)
    btnEval: TButton;
    edtExp: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtResult: TEdit;
    Label3: TLabel;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    Label5: TLabel;
    lbVars: TListBox;
    lbStrVars: TListBox;
    lbForm: TListBox;
    Label6: TLabel;
    lbDep: TListBox;
    Label4: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    OpenDialog1: TOpenDialog;
    Exit1: TMenuItem;
    lbFormDep: TListBox;
    Label8: TLabel;
    lbStrDep: TListBox;
    Label7: TLabel;
    seEvalCount: TSpinEdit;
    Label9: TLabel;
    btnStrEval: TButton;
    procedure btnEvalClick(Sender: TObject);
    procedure btnStrEvalClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbVarsClick(Sender: TObject);
    procedure lbStrVarsClick(Sender: TObject);
    procedure lbFormClick(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private
    Calc : TKAParser;
    T: TextFile;
    procedure UpdateVarList;
    procedure UpdateStrVarList;
    procedure UpdateFormList;
    procedure VarDep;
    procedure FormDep;
    procedure VarFormDep;
    procedure StrVarFormDep;
    procedure NoIdentEventHandler(Sender: TObject; ctype: TCalcType;
      const S: String; var Value: Double; var Handled: Boolean);
    procedure NoStrIdentEventHandler(Sender: TObject; ctype: TCalcType;
      const S: String; var Value: String; var Handled: Boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


{$R *.DFM}

procedure TForm1.btnEvalClick(Sender: TObject);
var
  I: Integer;
  Res: Double;
  StartTick: LongInt;
begin
  with Calc do
  begin
    Expression := edtExp.Text;
    StartTick := GetTickCount;
    Screen.Cursor := crHourGlass;
    try
      For I := 0 to seEvalCount.Value - 1 do
        Res := Calc.Result;
      edtResult.Text := FloatToStr(Res);
      StatusBar1.SimpleText := 'Eval Time : ' +
        IntToStr(GetTickCount - StartTick) + ' msecs';
      StatusBar1.Update;
    finally
      Screen.Cursor := crDefault;
    end;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Calc := TKAParser.Create(Self);
  Calc.OnNoIdent := NoIdentEventHandler;
  Calc.OnNoStrIdent := NoStrIdentEventHandler;
//  Calc.MaxExpCache := 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Calc.Free;
end;


procedure TForm1.btnStrEvalClick(Sender: TObject);
begin
  with Calc do
  begin
    Expression := edtExp.Text;
    edtResult.Text := Calc.StrResult;
  end;

end;
procedure TForm1.UpdateVarList;
var
  I: Integer;
begin
  lbVars.Clear;
  for I := 0 to Calc.VarCount - 1 do
    lbVars.Items.Add(Calc.NameOfVar(I));
end;

procedure TForm1.UpdateStrVarList;
var
  I: Integer;
begin
  lbStrVars.Clear;
  for I := 0 to Calc.StrVarCount - 1 do
    lbStrVars.Items.Add(Calc.NameOfStrVar(I));
end;

procedure TForm1.UpdateFormList;
var
  I: Integer;
begin
  lbForm.Clear;
  for I := 0 to Calc.FormulaeCount - 1 do
    lbForm.Items.Add(Calc.NameOfFormula(I));
end;

procedure TForm1.Open1Click(Sender: TObject);
var
  S: String;

function NewSection(Line: String): Boolean;
begin
  if (UpperCase(Line) = '[FORMULAE]') or
    (UpperCase(Line) = '[STRING VARS]') or
    (UpperCase(Line) = '[VARS]') then
    Result := True
  else
    Result := False;
end;

procedure ReadFormulae;
var
  I: Integer;
begin
  while not Eof(T) do
  begin
    ReadLn(T, S);
    if NewSection(S) then
      Break
    else
    begin
      I := Pos('=', S);
      if I > 0 then
        Calc.Formula[Copy(S, 1, I-1)]:=Copy(S, I+1, Length(S));
    end;
  end;
end;

procedure ReadVars;
var
  I: Integer;
begin
  while not Eof(T) do
  begin
    ReadLn(T, S);
    if NewSection(S) then
      Break
    else
    begin
      I := Pos('=', S);
      if I > 0 then
        Calc.Vars[Copy(S, 1, I-1)]:= StrToFloat(Copy(S, I+1, Length(S)));
    end;
  end;
end;

procedure ReadStrVars;
var
  I: Integer;
begin
  while not Eof(T) do
  begin
    ReadLn(T, S);
    if NewSection(S) then
      Break
    else
    begin
      I := Pos('=', S);
      if I > 0 then
        Calc.StrVars[Copy(S, 1, I-1)]:=Copy(S, I+1, Length(S));
    end;
  end;
end;

begin
  if OpenDialog1.Execute then
    AssignFile(T, OpenDialog1.FileName)
  else
    Exit;
  try
    Reset(T);
    ReadLn(T, S);
    while not Eof(T) do
    begin
      if UpperCase(S) = '[FORMULAE]' then
        ReadFormulae
      else if UpperCase(S) = '[STRING VARS]' then
        ReadStrVars
      else if UpperCase(S) = '[VARS]' then
        ReadVars
      else
        ReadLn(T, S);
    end;
    UpdateVarList;
    UpdateStrVarList;
    UpdateFormList;
  finally
    CloseFile(T);
  end;
end;


procedure TForm1.lbVarsClick(Sender: TObject);
begin
  lbDep.Clear;
  lbStrDep.Clear;
  with lbVars do
    if ItemIndex > - 1 then
    begin
      edtExp.Text := Items[ItemIndex];
      edtResult.Text := FloatToStr(Calc.Vars[Items[ItemIndex]]);
      VarFormDep;
    end;
end;

procedure TForm1.lbStrVarsClick(Sender: TObject);
begin
  lbDep.Clear;
  lbStrDep.Clear;
  with lbStrVars do
    if ItemIndex > - 1 then
    begin
      edtExp.Text := Items[ItemIndex];
      edtResult.Text := Calc.StrVars[Items[ItemIndex]];
      StrVarFormDep;
    end;

end;

procedure TForm1.lbFormClick(Sender: TObject);
begin
  with lbForm do
    if ItemIndex > - 1 then
    begin
      edtExp.Text := Calc.Formula[Items[ItemIndex]];
      try
        btnEval.Click;
      except
        btnStrEval.Click;
      end;
      VarDep;
      FormDep;
    end;

end;

procedure TForm1.VarDep;
var
  Dep: TStringList;
begin
  Dep := TStringList.Create;
  try
    with lbForm do
    begin
      if ItemIndex > -1 then
      begin
        // variable dependencies
        Calc.GetFormulaVarDep(Items[ItemIndex], Dep);
        lbDep.Items.Clear;
        lbDep.Items.Assign(Dep);
        // String variable dependencies
        Dep.Clear;
        Calc.GetFormulaStrVarDep(Items[ItemIndex], Dep);
        lbStrDep.Items.Clear;
        lbStrDep.Items.Assign(Dep);
      end;
    end;
  finally
    Dep.Free;
  end;
end;

procedure TForm1.FormDep;
var
  Dep: TStringList;
begin
  Dep := TStringList.Create;
  try
    with lbForm do
    begin
      if ItemIndex > -1 then
      begin
        Calc.GetFormulaFormDep(Items[ItemIndex], Dep);
        lbFormDep.Items.Clear;
        lbFormDep.Items.Assign(Dep);
      end;
    end;
  finally
    Dep.Free;
  end;

end;

procedure TForm1.VarFormDep;
var
  Dep: TStringList;
begin
  Dep := TStringList.Create;
  try
    with lbVars do
    begin
      if ItemIndex > -1 then
      begin
        Calc.GetVarDep(Items[ItemIndex], Dep);
        lbFormDep.Items.Clear;
        lbFormDep.Items.Assign(Dep);
      end;
    end;
  finally
    Dep.Free;
  end;


end;

procedure TForm1.StrVarFormDep;
var
  Dep: TStringList;
begin
  Dep := TStringList.Create;
  try
    with lbStrVars do
    begin
      if ItemIndex > -1 then
      begin
        Calc.GetStrVarDep(Items[ItemIndex], Dep);
        lbFormDep.Items.Clear;
        lbFormDep.Items.Assign(Dep);
      end;
    end;
  finally
    Dep.Free;
  end;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.NoIdentEventHandler(Sender: TObject; ctype: TCalcType;
  const S: String; var Value: Double; var Handled: Boolean);
begin
  if S = 'UK' then
  begin
    Value := 100;
    Handled := True
  end;
end;

procedure TForm1.NoStrIdentEventHandler(Sender: TObject; ctype: TCalcType;
  const S: String; var Value: String; var Handled: Boolean);
begin
  if S = 'UKS' then
  begin
    Value := 'Hello';
    Handled := True;
  end;
end;

end.
