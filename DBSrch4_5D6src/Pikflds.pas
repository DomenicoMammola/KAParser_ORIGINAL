{ Form Template - Source and Destination Choices Lists }
unit Pikflds;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, SysUtils, DesignIntf, DesignEditors;

type
  TDualListDlg = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    SrcList: TListBox;
    DstList: TListBox;
    SrcLabel: TLabel;
    DstLabel: TLabel;
    IncludeBtn: TSpeedButton;
    IncAllBtn: TSpeedButton;
    ExcludeBtn: TSpeedButton;
    ExAllBtn: TSpeedButton;
    procedure IncludeBtnClick(Sender: TObject);
    procedure ExcludeBtnClick(Sender: TObject);
    procedure IncAllBtnClick(Sender: TObject);
    procedure ExcAllBtnClick(Sender: TObject);
    procedure MoveSelected(List: TCustomListBox; Items: TStrings);
    procedure SetItem(List: TListBox; Index: Integer);
    function GetFirstSelection(List: TCustomListBox): Integer;
    procedure SetButtons;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TSrchFldsProperty = class( TClassProperty )
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TdbSearchEditor = class(TDefaultEditor)
  public
    procedure EditProperty(const Prop: IProperty;
      var Continue: Boolean); override;
  end;

  TDefSrchFldProperty = class(TStringProperty)
    procedure GetValues(Proc: TGetStrProc); override;
    function GetAttributes: TPropertyAttributes; override;
  end;



implementation

{$R *.DFM}

uses
  dbSearch;


procedure TDualListDlg.IncludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(SrcList);
  MoveSelected(SrcList, DstList.Items);
  SetItem(SrcList, Index);
end;

procedure TDualListDlg.ExcludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(DstList);
  MoveSelected(DstList, SrcList.Items);
  SetItem(DstList, Index);
end;

procedure TDualListDlg.IncAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to SrcList.Items.Count - 1 do
    DstList.Items.AddObject(SrcList.Items[I], 
      SrcList.Items.Objects[I]);
  SrcList.Items.Clear;
  SetItem(SrcList, 0);
end;

procedure TDualListDlg.ExcAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to DstList.Items.Count - 1 do
    SrcList.Items.AddObject(DstList.Items[I], DstList.Items.Objects[I]);
  DstList.Items.Clear;
  SetItem(DstList, 0);
end;

procedure TDualListDlg.MoveSelected(List: TCustomListBox; Items: TStrings);
var
  I: Integer;
begin
  for I := List.Items.Count - 1 downto 0 do
    if List.Selected[I] then
    begin
      Items.AddObject(List.Items[I], List.Items.Objects[I]);
      List.Items.Delete(I);
    end;
end;

procedure TDualListDlg.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := SrcList.Items.Count = 0;
  DstEmpty := DstList.Items.Count = 0;
  IncludeBtn.Enabled := not SrcEmpty;
  IncAllBtn.Enabled := not SrcEmpty;
  ExcludeBtn.Enabled := not DstEmpty;
  ExAllBtn.Enabled := not DstEmpty;
end;

function TDualListDlg.GetFirstSelection(List: TCustomListBox): Integer;
begin
  for Result := 0 to List.Items.Count - 1 do
    if List.Selected[Result] then Exit;
  Result := LB_ERR;
end;

procedure TDualListDlg.SetItem(List: TListBox; Index: Integer);
var
  MaxIndex: Integer;
begin
  with List do
  begin
    SetFocus;
    MaxIndex := List.Items.Count - 1;
    if Index = LB_ERR then Index := 0
    else if Index > MaxIndex then Index := MaxIndex;
    Selected[Index] := True;
  end;
  SetButtons;
end;

{---------------- Property Editor Stuff ----------------------}

{------ Default Search Field ------------}

function TDefSrchFldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList];
end;  {GetAttributes}


procedure TDefSrchFldProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  TheComponent: TdbSearch;
begin
  TheComponent := GetComponent(0) as TdbSearch;
  if TheComponent.DataSource <> nil then
  begin
      if(TheComponent.SearchFields.Count > 0)then
         begin
         for I := 0 to TheComponent.SearchFields.Count - 1 do
            Proc(TheComponent.SearchFields[I]);
         end
      else
         begin
         for I := 0 to (TheComponent.DataSource.DataSet.FieldCount - 1) do
            Proc(TheComponent.DataSource.DataSet.Fields[I].DisplayLabel);
         end;
  end;
end;


{---- Search Fields ---}

function TSrchFldsProperty.GetAttributes: TPropertyAttributes;
begin
Result := [paDialog, paReadOnly];
end;  {GetAttributes}

function TSrchFldsProperty.GetValue: String;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

procedure TSrchFldsProperty.Edit;
var
   DualListDlg: TDualListDlg;
   FieldNames: TStrings;
   i: Integer;
   TheComponent: TdbSearch;
begin
  DualListDlg := TDualListDlg.Create(Application);
  try
    TheComponent := GetComponent(0) as TdbSearch;
    if TheComponent.DataSource <> nil then
    begin
      FieldNames := TStrings(GetOrdValue);
      for i := 0 to FieldNames.Count - 1 do
        DualListDlg.DstList.Items.Add(FieldNames[i]);
      With TheComponent.DataSource.Dataset, DualListDlg.SrcList.Items do
        for i := 0 to FieldCount - 1 do
          if (DualListDlg.DstList.Items.IndexOf(Fields[i].DisplayLabel) = -1) then
            Add(Fields[i].DisplayLabel);
      with DualListDlg do
      begin
        SetButtons;
        ShowModal;
        if ModalResult = mrOK then
          SetOrdValue(LongInt(DstList.Items));
      end;
    end
  finally
    DualListDlg.Free;
  end; {try}
end; {Edit}

{TdbSearchEditor}

procedure TdbSearchEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if (CompareText(PropName, 'SEARCHFIELDS') = 0) then
  begin
    Prop.Edit;
    Continue := False;
  end;
end;

end.

(*
// ==============================================================
// Copyright (c) 1996 by Aaron Castro and VK Thakur. All rights reserved
// Version 3.01   Date: 30/05/95
*)

