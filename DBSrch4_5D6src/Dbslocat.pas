unit Dbslocat;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, db, DBTables, Buttons,
  Txtsrch, DBsearch, Grids;

{************************************************************************}
{Locate Dialog box class}
{************************************************************************}
type

  TDLGLocate = class(TForm)
    bOK: TBitBtn;
    BitBtn2: TBitBtn;
    rgMatchType: TRadioGroup;
    cbxCaseSensitive: TCheckBox;
    lbConds: TListBox;
    Label1: TLabel;
    bbNew: TBitBtn;
    edtLogic: TEdit;
    Label2: TLabel;
    txtSearchString: TEdit;
    Label3: TLabel;
    cbxFields: TComboBox;
    Label4: TLabel;
    rgSearchDirection: TRadioGroup;
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure txtSearchStringChange(Sender: TObject);
    procedure bHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bbNewClick(Sender: TObject);
    procedure lbCondsClick(Sender: TObject);
    procedure lbCondsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtLogicKeyPress(Sender: TObject; var Key: Char);

  private
    Logic: String;
    LastIndex: Integer;
    DefaultLogic: Boolean;
    procedure UpdateLB;
    procedure UpdateControls;
    procedure DisplayLogic;
  public
    { Public declarations }
    HelpFile: string;
    procedure GetLocateInfoEx(var LI: TStringList; var L: String; var UL: String);
    procedure SetLocateInfo(SO: TDBSearch; const SrchConds: TStringList;
      const Logic: String);
    procedure AddLocateField(FieldName: string);
  end;


implementation

{$R *.DFM}
uses
  Calc;
var
  SrchObj: TTextSearch;

procedure TDLGLocate.GetLocateInfoEx(var LI: TStringList;var L: String; var UL: String);
var
  I: Integer;
  P: PLocateInfo;
begin
  for I := 0 to lbConds.Items.Count - 1 do
  begin
    New(P);
    with PLocateInfo(lbConds.Items.Objects[I])^ do
    begin
      P^.LocateField := LocateField;
      P^.LocateText := LocateText;
      P^.CaseSensitive := CaseSensitive;
      P^.MatchType := PLocateInfo(lbConds.Items.Objects[I])^.MatchType;
      P^.Evaluation := 0;
    end;
    LI.AddObject(lbConds.Items[I], TObject(P));
  end;
  L := Logic;
  UL := edtLogic.Text;
end;


procedure TDLGLocate.SetLocateInfo(SO: TDBSearch;
  const SrchConds: TStringList; const Logic: String);
var
  I: Integer;
  P: PLocateInfo;
begin
  // Set the Search Object
  SrchObj := SO;
  // Update the dialog box list
  for I := 0 to SrchConds.Count - 1 do
  begin
    New(P);
    with PLocateInfo(SrchConds.Objects[I])^ do
    begin
      P^.LocateField := LocateField;
      P^.LocateText := LocateText;
      P^.CaseSensitive := CaseSensitive;
      P^.MatchType := MatchType;
      P^.Evaluation := 0;
    end;
    lbConds.Items.AddObject(SrchConds[I], TObject(P));
  end;
  if lbConds.Items.Count > 0 then
  begin
    lbConds.ItemIndex := 0;
    UpdateControls;
  end
  else
  begin
    New(P);
    lbConds.Items.AddObject('#1', TObject(P));
    lbConds.ItemIndex := 0;
    with cbxFields do
      if SO.DefSearchField <> '' then
        ItemIndex := Items.IndexOf(SO.DefSearchField)
      else
        ItemIndex := 0;
    rgMatchType.ItemIndex := Integer(SO.MatchType);
  end;
  LastIndex := 0;
  edtLogic.Text := Logic;
  txtSearchStringChange(nil);

end;



{************************************************************************}
{
    AddLocateField
         FieldName (Input): Field to be added to searchable field list


     Adds fields to the combobox listing fields that can be searched
}
{************************************************************************}

procedure TDLGLocate.AddLocateField(FieldName: string);
begin
    cbxFields.Items.Add(FieldName);
end;



procedure TDLGLocate.bOKClick(Sender: TObject);
var
  MyCalc: TCalculator;
  I: Integer;
  Temp: String;
begin
  UpdateLB;
  if edtLogic.Text = '' then
  begin
    ModalResult := mrOK;
    Exit;
  end;
  Logic := UpperCase(edtLogic.Text);
  while Pos('OR', Logic) > 0 do
    Logic := Copy(Logic, 1, Pos('OR', Logic) - 1) + ' | ' +
      Copy(Logic, Pos('OR', Logic) + 2, Length(Logic));
  while Pos('AND', Logic) > 0 do
    Logic := Copy(Logic, 1, Pos('AND', Logic) - 1) + '&' +
      Copy(Logic, Pos('AND', Logic) + 3, Length(Logic));
  Temp := Logic;
  for I := 1 to lbConds.Items.Count do
    if Pos('#' + IntToStr(I), Temp) > 0 then
      Temp := Copy(Temp, 1, Pos('#' + IntToStr(I), Temp) - 1) + '1' +
        Copy(Temp, Pos('#' + IntToStr(I), Temp) + Length('#' + IntToStr(I)),
          Length(Temp));
  MyCalc := TCalculator.Create;
  try
    try
      MyCalc.Expression := Temp;
      MyCalc.Result;
    except
      Application.MessageBox(PChar('Error in Logic : ' + Logic), 'Parser Error', MB_OK);
      Exit;
    end;
    ModalResult := mrOK;
  finally
    MyCalc.Free;
  end;
end;


procedure TDLGLocate.bCancelClick(Sender: TObject);
begin
    ModalResult:=mrCancel;
end;

procedure TDLGLocate.txtSearchStringChange(Sender: TObject);
var
  I: Integer;
  P: PLocateInfo;

begin
   I := 0;
   with lbConds do
   while I < Items.Count do
   begin
     if I = ItemIndex then
     begin
       if txtSearchString.Text = '' then
         Break;
     end
     else
     begin
       P := PLocateInfo(Items.Objects[I]);
       if P^.LocateText = '' then
         Break;
     end;
     Inc(I);
   end;
   if (I >= lbConds.Items.Count) then
   begin
      bbNew.Enabled := True;
      bOK.Enabled:=True;
   end
   else
   begin
      bbNew.Enabled := False;
      bOK.Enabled:=False;
   end;
   if SrchObj.IsPatternWild(PChar(txtSearchString.Text)) then
     rgMatchType.ItemIndex := Integer(mtPattern)
   else if rgMatchType.ItemIndex = 3 then
     rgMatchType.ItemIndex := Integer(mtSubstring);
end;

procedure TDLGLocate.bHelpClick(Sender: TObject);
var
   pString: PChar;
begin
     pString:=StrAlloc(255);
     StrPCopy(pString, HelpFile);
     WinHelp(Application.Handle, pString, HELP_CONTENTS, 0);
     StrDispose(pString);
end;


procedure TDLGLocate.FormCreate(Sender: TObject);
begin
  DefaultLogic := True;
end;

procedure TDLGLocate.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  with lbConds.Items do
  for I := 0 to Count - 1 do
    Dispose(PLocateInfo(Objects[I]));
end;

procedure TDLGLocate.UpdateLB;
begin
  // Store changes if any
  with PLocateInfo(lbConds.Items.Objects[LastIndex])^ do
  begin
//    LocateField := cbxFields.Items[cbxFields.ItemIndex];
    LocateField := cbxFields.Text;
    LocateText :=  txtSearchString.Text;
    CaseSensitive := cbxCaseSensitive.Checked;
    MatchType := TMatchType(rgMatchType.ItemIndex);
  end;
end;

procedure TDLGLocate.bbNewClick(Sender: TObject);
var
  P: PLocateInfo;
begin
  UpdateLB;
  // Add a new condition
  New(P);
  with lbConds.Items do
    AddObject('#' + IntToStr(Count + 1), TObject(P));
  lbConds.ItemIndex := lbConds.Items.Count - 1;
  // Load the defaults into the condition
  with PLocateInfo(lbConds.Items.Objects[lbConds.ItemIndex])^ do
  begin
    LocateField := cbxFields.Text;
    LocateText :=  '';
    CaseSensitive := cbxCaseSensitive.Checked;
    MatchType := TMatchType(rgMatchType.ItemIndex);
  end;
  lbCondsClick(nil);
  DisplayLogic;
end;

procedure TDLGLocate.lbCondsClick(Sender: TObject);
begin
  if lbConds.ItemIndex > -1 then
  begin
    if (lbConds.ItemIndex <> LastIndex) then
    begin
      // Store edits from the controls
      UpdateLB;
      LastIndex := lbConds.ItemIndex;
      // Display the fresh data in the controls
      UpdateControls;
    end;
  end;
end;

procedure TDLGLocate.UpdateControls;
begin
  with PLocateInfo(lbConds.Items.Objects[lbConds.ItemIndex])^ do
  begin
    with cbxFields do
      ItemIndex := Items.IndexOf(LocateField);
    txtSearchString.Text := LocateText;
    cbxCaseSensitive.Checked := CaseSensitive;
    rgMatchType.ItemIndex := Ord(MatchType);
  end;
end;

procedure TDLGLocate.lbCondsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  I: Integer;
begin
  if (lbConds.ItemIndex > -1) and
    (lbConds.Items.Count > 1) and
    (Key = VK_DELETE) then
    with lbConds, lbConds.Items do
    begin
      I := ItemIndex;
      // Delete the currently selected item
      Dispose(PLocateInfo(Objects[ItemIndex]));
      Delete(ItemIndex);
      // Reset the current item and LastIndex
      if I > 0 then
        ItemIndex := I - 1
      else
        ItemIndex := 0;
      LastIndex := ItemIndex;
      // Show the data for the currently selected item
      UpdateControls;
      // Rename the conditions
      for I := 0 to Count - 1 do
        Strings[I] := '#' + IntToStr(I + 1);
      DisplayLogic;
    end;
end;

procedure TDLGLocate.DisplayLogic;
var
  Temp: String;
  I: Integer;
begin
  if DefaultLogic then
  begin
    Temp := '';
    for I := 1 to lbConds.Items.Count do
    begin
      Temp := Temp + '#' + IntToStr(I);
      if I < lbConds.Items.Count then
        Temp := Temp + ' or ';
    end;
    edtLogic.Text := Temp;
  end
  else
    edtLogic.Text := edtLogic.Text + ' or #' +
      IntToStr(lbConds.Items.Count);
end;

procedure TDLGLocate.edtLogicKeyPress(Sender: TObject; var Key: Char);
begin
  if DefaultLogic then
    DefaultLogic := False;

end;




end.

(*
// ==============================================================
// Copyright (c) 1996 by Aaron Castro and VK Thakur. All rights reserved
// Version 3.01   Date: 30/05/95
*)

