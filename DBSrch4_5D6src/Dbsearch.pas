unit Dbsearch;

interface
uses SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
Forms, DB, DBTables,
TxtSrch, Calc;

{************************************************************************}
{structure used to set and retrieve information from the locate dialogbox}
{************************************************************************}

type
  // Pointer to a LocateInfo structure
  PLocateInfo = ^TLocateInfo;
   TOnProgressEvent = procedure(Sender: TObject; var Continue: Boolean)
     of Object;
   TOnFoundEvent = procedure(Sender: TObject; var Continue: Boolean)
     of Object;
   TLocateInfo = record
      LocateField: string;        {The field to search}
      LocateText: string;         {The text with pattern to search for}
      CaseSensitive: boolean;     {Is the search case sensitive?}
      MatchType: TMatchType;      // Type of match 0-Whole Word, 1-Exact, 2-Pattern
      FieldNumber: Byte;              // Field No
      Evaluation: Byte;        // True or False. For program use only
      MatchStart: Integer;
      MatchEnd: Integer;
    end;

   EDBSearchPatternNotFound = class(Exception);

   TdbSearch = class(TTextSearch)
   private
     FCalc: TCalculator;
     FSrchConds: TStringList;
     FLogic: String;
     FUserLogic: String;
     FOnProgress: TOnProgressEvent;
     FOnFound: TOnFoundEvent;
     FReptProgInterval: Integer;
     FCurrRec: Integer;
     FDataSource: TDataSource;
     FDefSearchField,
     FSearchField: String;
     FSearchFields: TStrings;
     FActiveIndexFields: TStrings;
     FActiveIndexValues: TStrings;
     FDialogCaption,
     FKeyFields: String;
     FReverseSearch,
     FUseIndexForSearch: Boolean;
     FHasFilter: Boolean;
     function SearchInMemo(Blob: TBlobField; CaseSensitive: Boolean): Boolean;
     function GetDataSource: TDataSource;
     procedure SetDataSource(ds: TDataSource);
     procedure SetSearchFields(FieldNames: TStrings);
     function GetLocateInfo: boolean;
     function CanSearchOnIndex(FieldName: String): Boolean;
     function GetPrimaryKeyFields: String;
     procedure IndexedLocate(LocateInfo: TLocateInfo; FieldNumber: Integer;
      FromBeginning: Boolean);
     procedure SequentialLocate(LocateInfo: TLocateInfo; FieldNumber: Integer;
      FromBeginning: Boolean);
      {$ifdef demo}
      procedure Nag;
      {$endif}
     function _QualifiesFilter(SrchConds: TStringList; Logic: String): Boolean;
   protected
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
   public
     function QualifiesFilter: Boolean;
     procedure CondsLocate(SrchConds: TStringList; Logic: String; FromBeginning: Boolean);
     procedure DoLocate(LocateInfo: TLocateInfo; FromBeginning: Boolean);
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     procedure Locate;
     procedure LocateNext;
     procedure SetFilter;
     procedure ClearFilter;
     property SearchConditions: TStringList read FSrchConds;
     property SearchField: String read FSearchField;
   published
     property DataSource: TDataSource read GetDataSource write SetDataSource;
     property DefSearchField: string read FDefSearchField write FDefSearchField;
     property SearchFields: TStrings read FSearchFields write SetSearchFields;
     property UseIndexForSearch: Boolean read FUseIndexForSearch write FUseIndexForSearch
       default False;
     property ReptProgInterval: Integer read FReptProgInterval write
       FReptProgInterval default 100;
     property DialogCaption: String read FDialogCaption write
       FDialogCaption;
     property ReverseSearch: Boolean read FReverseSearch write
       FReverseSearch;
     property OnProgress: TOnProgressEvent read FOnProgress write
       FOnProgress;
     property OnFound: TOnFoundEvent read FOnFound write
       FOnFound;
   end;


implementation

uses
  Dbslocat;

constructor TdbSearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCalc := TCalculator.Create;
  FSrchConds := TStringList.Create;
  FSearchFields := TStringList.Create;
  FActiveIndexFields := TStringList.Create;
  FActiveIndexValues := TStringList.Create;
  FUseIndexForSearch := False;
  FReptProgInterval := 100;
  FDialogCaption := 'Locate Dialog';
end;

destructor TdbSearch.Destroy;
var
  I: Integer;
begin
  for I := 0 to FSrchConds.Count - 1 do
    Dispose(PLocateInfo(FSrchConds.Objects[I]));
  FSrchConds.Free;
  FSearchFields.Free;
  FActiveIndexFields.Free;
  FActiveIndexValues.Free;
  FCalc.Free;
  inherited Destroy;
end;

procedure TdbSearch.Notification(AComponent: TComponent;
                                 Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and
    (AComponent = DataSource) then
    DataSource := nil;
end;

function TdbSearch.GetDataSource: TDataSource;
begin
  Result := FDataSource;
end;

procedure TdbSearch.SetDataSource(ds: TDatasource);
begin
  if (FDataSource <> ds) then
  begin
    if FDataSource <> nil then
    begin
      FSearchFields.Clear;
      FDefSearchField := '';
    end;
    FDataSource := ds;
  end;
end;

procedure TdbSearch.SetSearchFields(FieldNames: TStrings);
begin
  FSearchFields.Assign(FieldNames);
end;

function TdbSearch.GetLocateInfo: Boolean;
var
  loopctr: integer;
  DLGLocate: TDLGLocate;
begin
   Result:=False;
   DLGLocate := TDLGLocate.Create(Owner);
   try
     DLGLocate.Caption := FDialogCaption;
     if FReverseSearch then
       DLGLocate.rgSearchDirection.ItemIndex := 1
     else
       DLGLocate.rgSearchDirection.ItemIndex := 0;
     DLGLocate.rgMatchType.Items[3] := 'Pattern Match ( ' + SingleCharWildCard + ' and '
     + MultiCharWildCard + ' )';
     if (FDataSource <> nil) then
     begin
       if FSearchFields.Count > 0 then
       begin
         for loopctr := 0 to FSearchFields.Count -1 do
           DLGLocate.AddLocateField(SearchFields[loopctr]);
       end
       else
         {Search is permitted on all table fields}
         for loopctr:=0 to FDataSource.DataSet.FieldCount - 1 do
           {Add only the ones the end-user can see}
           if(FDataSource.DataSet.Fields[loopctr].Visible)then
            DLGLocate.AddLocateField(FDataSource.DataSet.Fields[loopctr].DisplayLabel);
       // Initialize the dialogbox fields
       DLGLocate.SetLocateInfo(Self, FSrchConds, FUserLogic);
       {Show locate dialog}
       if(DLGLocate.ShowModal=mrOK)then
       begin
         {Get the results}
         for LoopCtr := 0 to FSrchConds.Count - 1 do
           Dispose(PLocateInfo(FSrchConds.Objects[LoopCtr]));
         FSrchConds.Clear;
         DLGLocate.GetLocateInfoEx(FSrchConds, FLogic, FUserLogic);
         FReverseSearch := (DLGLocate.rgSearchDirection.ItemIndex = 1);
         Result:=True;
       end;
     end;
   finally
     DLGLocate.Free;
   end;
end;

function TdbSearch.CanSearchOnIndex(FieldName: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  with (FDataSource.Dataset as TTable) do
  begin
    IndexDefs.Update;
    for I := 0 to IndexDefs.Count - 1 do
      if Pos(IndexDefs.Items[I].Fields, FieldName) = 1 then
        Result := True;
  end;
end;



procedure TdbSearch.DoLocate(LocateInfo: TLocateInfo; FromBeginning:
Boolean);
const
  CurrentIndex: String = '';
  CurrentTable: String = '';

var
   I: Integer;
   FieldNumber: Integer; {The index of the field that we are searching on}
begin

  FieldNumber := 0;
  // Set the TextSearch object properties
  MatchType := LocateInfo.MatchType;
  SearchPattern := LocateInfo.LocateText;
  case MatchType of
    mtEqualTo: SearchPattern := ' = ' + SearchPattern;
    mtGT: SearchPattern := ' > ' + SearchPattern;
    mtGTEqualTo: SearchPattern := ' >= ' + SearchPattern;
    mtLT: SearchPattern := ' < ' + SearchPattern;
    mtLTEqualTo: SearchPattern := ' <= ' + SearchPattern;
  end;
  FSearchField := LocateInfo.LocateField;

  {Get the index of the field to search}
  for I := 0 to FDataSource.Dataset.FieldCount - 1 do
    if FDataSource.Dataset.Fields[I].DisplayLabel=LocateInfo.LocateField then
    begin
      FieldNumber := I;
      Break;
    end;

  if FieldNumber >= FDataSource.Dataset.FieldCount then
    raise EDBSearchPatternNotFound.Create('Search Field not in table');

  if ((LocateInfo.MatchType = mtStartsWith) and
    (FReverseSearch = False) and
    (FDataSource.Dataset.ClassName = 'TTable') and
    FUseIndexForSearch and
    CanSearchOnIndex(LocateInfo.LocateText)) then
  begin
    if CurrentTable <> (FDataSource.Dataset.Name) then
      FKeyFields := GetPrimaryKeyFields;
    if CurrentIndex <> (FDataSource.Dataset as TTable).IndexName then
    begin
      FActiveIndexFields.Clear;
      with (FDataSource.Dataset as TTable) do
      try
        for I := 0 to IndexFieldCount - 1 do
          FActiveIndexFields.Add(IndexFields[I].FieldName);
      except
      end;
    end;
    IndexedLocate(LocateInfo, FieldNumber, FromBeginning);
  end
  else
    SequentialLocate(LocateInfo, FieldNumber, FromBeginning);
end;

function TdbSearch.GetPrimaryKeyFields: String;
var
  I: Integer;
begin
  with (FDataSource.Dataset as TTable) do
    for I := 0 to IndexDefs.Count - 1 do
      if (ixPrimary in IndexDefs[I].Options) then
      begin
        Result := IndexDefs[I].Fields;
        break;
      end;
end;

procedure TdbSearch.IndexedLocate(LocateInfo: TLocateInfo; FieldNumber: Integer;
  FromBeginning: Boolean);
const
  Jump: Integer = 1;
var
   PrimaryKeyValues,
   ActiveIndex: String;
   I: Integer;
   OldPos: TBookMark;


  function GetPrimaryKeyValues(KeyFields: String): String;
  var
    I: Integer;
  begin
      Result := '';
      with (FDataSource.DataSet as TTable) do
      repeat
        I := Pos(';', KeyFields);
        if I <> 0 then
        begin
          Result := Result + FieldByName(Copy(KeyFields, 1, I - 1)).AsString;
          KeyFields := Copy(KeyFields, I + 1, Length(KeyFields));
        end
        else
          Result := Result + FieldByName(KeyFields).AsString;
      until (I = 0);
  end;


begin
  With (FDataSource.Dataset as TTable) do
  begin
    DisableControls;
    // Save old position
    OldPos := GetBookMark;
    {Store the name of the currently active index}
    ActiveIndex := IndexName;
    try
      // Switch the index
      // Delphi will pick an index based on the columns specified in IndexFieldNames
      IndexFieldNames := Fields[FieldNumber].FieldName;
      SetKey;
      Fields[FieldNumber].AsString := LocateInfo.LocateText;
      GotoNearest;
      if FromBeginning then
        Jump := 1
      else
      begin
        MoveBy(Jump);
        Inc(Jump);
      end;

      // if we actually got the value that we were looking for
      if Pos(LocateInfo.LocateText, Fields[FieldNumber].AsString) <> 0 then
      begin
        // Store the value of the primary key
        PrimaryKeyValues := GetPrimaryKeyValues(FKeyFields);
        {Store the Active Index Key Values}
        FActiveIndexValues.Clear;
        for I := 0 to FActiveIndexFields.Count - 1 do
          FActiveIndexValues.Add(FieldByName(FActiveIndexFields[I]).AsString);
        {Get back to the original index and find the same record}
        IndexName := ActiveIndex;
        SetKey;
        for I := 0 to FActiveIndexFields.Count - 1 do
          IndexFields[I].AsString := FActiveIndexValues[I];
        GoToKey;
        // We may not have actually re-positioned on the record that we found earlier
        // Lets get to it by comparing the key value
        while (PrimaryKeyValues <> GetPrimaryKeyValues(FKeyFields)) and not eof do
          Next;
      end
      else
      // FindNearest didn't get to the value that we were looking for
      begin
        IndexName := ActiveIndex;
        GotoBookMark(OldPos);
        raise EDBSearchPatternNotFound.Create('Search pattern not found');
      end;
    finally
      FreeBookmark(OldPos);
      EnableControls;
    end;
  end;
end;

procedure TdbSearch.SequentialLocate(LocateInfo: TLocateInfo; FieldNumber: Integer;
  FromBeginning: Boolean);
var
   Oldpos: TBookmark;
   StringMatch,
   Continue,
   ContSearch: Boolean;
   Tmp: String;
   OldCursor: HCursor;
begin
  {$ifdef demo}
  if Random(10) = 9 then
    Nag;
  {$endif}
  Continue := True;
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  with FDataSource.DataSet do
  try
    DisableControls;
    {Save oldposition}
    Oldpos:=GetBookmark;
    {Start from first record or the next one}
    if(FromBeginning)then
    begin
      if FReverseSearch then
        Last
      else
        First;
      FCurrRec := 0;
    end
    else if FReverseSearch then
      Prior
    else
      Next;
    StringMatch := False;
    while True do
    begin
      if (FReverseSearch and bof) or
        (not FReverseSearch and eof) then
        Break;
      Inc(FCurrRec);
      if FCurrRec mod FReptProgInterval = 0 then
      begin
        Application.ProcessMessages;
        FCurrRec := 0;
        if Assigned(FOnProgress) then
        begin
          Continue := True;
          FOnProgress(Self, Continue);
          if not Continue then
            Break;
        end;
      end;

      if LocateInfo.MatchType in [mtEqualTo, mtGT, mtGTEqualTo,
        mtLT, mtLTEqualTo] then
      begin
        if (Fields[FieldNumber].DataType = ftString) then
        begin
          Tmp := Fields[FieldNumber].AsString;
          while Pos('''', Tmp) > 0 do
            Tmp := Copy(Tmp, 1, Pos('''', Tmp) - 1) +
            Copy(Tmp, Pos('''', Tmp) + 1, Length(Tmp));
          Tmp := '''' + Tmp + '''';
        end
        else if (Fields[FieldNumber].DataType = ftDateTime) then
          Tmp := DateToStr(TDate(Fields[FieldNumber].AsDateTime))
        else
          Tmp := Fields[FieldNumber].AsString;
        Tmp := Tmp + ' ' + SearchPattern;
        FCalc.Expression := Tmp;
        if Trunc(FCalc.Result) = 0 then
          StringMatch := False
        else
          StringMatch := True;

(*
        // Causes an Internal Error in the compiler
        StringMatch := Boolean(Trunc(FCalc.Result));
*)
      end
      else if(Fields[FieldNumber].DataType = ftBlob) then
        StringMatch := SearchInMemo(TBlobField(Fields[FieldNumber]),
          LocateInfo.CaseSensitive)
      else if(Fields[FieldNumber].DataType = ftMemo) then
        StringMatch := IsPatternInString(Fields[FieldNumber].AsString,
          LocateInfo.CaseSensitive)
      else
        StringMatch := IsPatternInString(Fields[FieldNumber].AsString,
          LocateInfo.CaseSensitive);
      if StringMatch then
        if Assigned(FOnFound) then
        begin
          ContSearch := False;
          FOnFound(Self, ContSearch);
          if ContSearch = False then
            Break;
        end
        else
          Break;
      if FReverseSearch then
        Prior
      else
        Next;
    end;
    {End of the data and no match? reposition to previous position}
    if(not StringMatch) and Continue then
      Gotobookmark(oldpos);
    Freebookmark(oldpos);
  finally;
    Screen.Cursor := OldCursor;
    EnableControls;
  end;
  if(not StringMatch)and Continue then
      raise EDBSearchPatternNotFound.Create('Search pattern not found');
end;

function TdbSearch.SearchInMemo(Blob: TBlobField; CaseSensitive: Boolean): Boolean;
var
   P: PChar;
begin
  with TBlobStream.Create(Blob, bmRead) do
  try
    GetMem(P, Size + 1);
    try
      FillChar(P^, Size + 1, #0);
      Read(P^, Size);
      Result := IsPatternInString(P, CaseSensitive);
    finally
      FreeMem(P, Size + 1);
    end;
  finally
    Free;
  end;
end;

procedure TdbSearch.Locate;
begin
  {Get info from dialog then do search}
  if(GetLocateInfo=True)then
  begin
    if (FSrchConds.Count = 1) and (FUserLogic = '') then
      DoLocate(PLocateInfo(FSrchConds.Objects[0])^, True)
    else
      CondsLocate(FSrchConds, FLogic, True);
  end;
end;

procedure TdbSearch.CondsLocate(SrchConds: TStringList; Logic: String; FromBeginning: Boolean);
var
   Oldpos: TBookmark;
   StringMatch,
   Continue,
   ContSearch: Boolean;
   I, J: Integer;
   P: PLocateInfo;
   OldCursor: HCursor;

begin
  StringMatch := False;
  {$ifdef demo}
  if Random(10) = 9 then
    Nag;
  {$endif}
  Continue := True;
  with FDataSource.DataSet do
  begin
    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    DisableControls;
    {Save oldposition}
    Oldpos:=GetBookmark;
    try
      if(FromBeginning)then
      begin
        // for each field in the conditions list
        for I := 0 to SrchConds.Count - 1 do
        begin
          P := PLocateInfo(SrchConds.Objects[I]);
          {Get the index of the field to search}
          P^.FieldNumber := 0;
          for J := 0 to FieldCount - 1 do
            if Fields[J].DisplayLabel = P^.LocateField then
            begin
              P^.FieldNumber := J;
              Break;
            end;
        end;
        if FReverseSearch then
          Last
        else
          First;
        FCurrRec := 0;
      end
      else if FReverseSearch then // Not from begining
        Prior
      else
        Next;

      {Get and compare data}
      // Database scan starts here
      while True do
      begin
        if (FReverseSearch and bof) or (not FReverseSearch and eof) then
          Break;
        Inc(FCurrRec);
        if FCurrRec mod FReptProgInterval = 0 then
        begin
          Application.ProcessMessages;
          FCurrRec := 0;
          if Assigned(FOnProgress) then
          begin
            Continue := True;
            FOnProgress(Self, Continue);
            if not Continue then
              Break;
          end;
        end;
        // Does this record meet the search criterion?
        StringMatch := _QualifiesFilter(SrchConds, Logic);
        if StringMatch then
          if Assigned(FOnFound) then
          begin
            ContSearch := False;
            FOnFound(Self, ContSearch);
            if ContSearch = False then
              Break;
          end
          else
            Break;
        if FReverseSearch then
          Prior
        else
          Next;
      end;
    finally
      {End of the data and no match? reposition to previous position}
      if(not StringMatch)and Continue then
        Gotobookmark(oldpos);
      EnableControls;
      Freebookmark(oldpos);
      Screen.Cursor := OldCursor;
    end;
  end;
  if(not StringMatch) and Continue then
      raise EDBSearchPatternNotFound.Create('Search pattern not found');
end;



procedure TdbSearch.LocateNext;
begin
   {Has a locate been done before?}
   if(FSrchConds.Count = 0)then
     Locate
   else if (FSrchConds.Count = 1) and (FUserLogic = '') then
     DoLocate(PLocateInfo(FSrchConds.Objects[0])^, False)
   else
     CondsLocate(FSrchConds, FLogic, False);
end;
{$ifdef demo}
procedure TDBSearch.Nag;
begin
  Application.MessageBox('Demo Version'#10#13 +
    'To register contact Vijainder K Thakur at vkt@pobox.com',
    'TDBSearch Ver 3.5', MB_OK);
end;
{$endif}

procedure TdbSearch.SetFilter;
var
  I, J: Integer;
  P: PLocateInfo;
begin
  if GetLocateInfo then
  begin
    FHasFilter := True;
    with FDatasource.Dataset do
    for I := 0 to FSrchConds.Count - 1 do
    begin
      P := PLocateInfo(FSrchConds.Objects[I]);
      {Get the index of the field to search}
      P^.FieldNumber := 0;
      for J := 0 to FieldCount - 1 do
        if Fields[J].DisplayLabel = P^.LocateField then
        begin
          P^.FieldNumber := J;
          Break;
        end;
    end;
  end
  else
    FHasFilter := False;
end;

function TdbSearch._QualifiesFilter(SrchConds: TStringList; Logic: String): Boolean;
var
  I, J: Integer;
  P: PLocateInfo;
  Cond, Tmp, LogOp: String;
begin
  with FDatasource.Dataset do
  for I := 0 to SrchConds.Count - 1 do
  begin
    P := PLocateInfo(SrchConds.Objects[I]);
    Self.MatchType := P^.MatchType;
    Self.SearchPattern := P^.LocateText;
    case MatchType of
      mtEqualTo: LogOp := ' = ';
      mtGT: LogOp :=  ' > ';
      mtGTEqualTo: LogOp := ' >= ';
      mtLT: LogOp := ' < ';
      mtLTEqualTo: LogOp := ' <= ';
    end;
    if(Fields[P^.FieldNumber].DataType = ftBlob) then
    begin
      P^.Evaluation := Byte(SearchInMemo(TBlobField(Fields[P^.FieldNumber]),
        P^.CaseSensitive));
      if P^.Evaluation = 1 then
      begin
        P^.MatchStart := Self.MatchStart;
        P^.MatchEnd := Self.MatchEnd;
      end;
    end
    else if(Fields[P^.FieldNumber].DataType = ftMemo) then
    begin
      P^.Evaluation := Byte(IsPatternInString(Fields[P^.FieldNumber].AsString,
        P^.CaseSensitive));
      if P^.Evaluation = 1 then
      begin
        P^.MatchStart := Self.MatchStart;
        P^.MatchEnd := Self.MatchEnd;
      end;
    end
    else if P^.MatchType in [mtEqualTo, mtGT, mtGTEqualTo,
      mtLT, mtLTEqualTo] then
    begin
      // Handle string fields
      if Fields[P^.FieldNumber].DataType = ftString then
        Tmp := QuotedStr(Fields[P^.FieldNumber].AsString) +
          LogOp + QuotedStr(SearchPattern)
      // Handle date fields
      else if Fields[P^.FieldNumber].DataType = ftDateTime then
        Tmp := FloatToStr(Fields[P^.FieldNumber].AsDateTime) +
          LogOp + FloatToStr(StrToDateTime(SearchPattern))
      else
        Tmp := Fields[P^.FieldNumber].AsString + LogOp + SearchPattern;
      FCalc.Expression := Tmp;
      P^.Evaluation := Trunc(FCalc.Result);
    end
    else
    begin
      P^.Evaluation :=
        Byte(IsPatternInString(Fields[P^.FieldNumber].Text,
          P^.CaseSensitive));
      if P^.Evaluation = 1 then
      begin
        P^.MatchStart := Self.MatchStart;
        P^.MatchEnd := Self.MatchEnd;
      end;
    end;
  end;
  if SrchConds.Count > 1 then
  begin
    Tmp := Logic;
    for I := 0 to SrchConds.Count - 1 do
    begin
      Cond := '#' + IntToStr(I+1);
      J := Pos(Cond, Tmp);
      if J > 0 then
        Tmp := Copy(Tmp, 1, J - 1) +
          IntToStr(Integer(PLocateInfo(SrchConds.Objects[I])^.Evaluation)) +
          Copy(Tmp, J + Length(Cond), Length(Tmp));
    end;
    FCalc.Expression := Tmp;
    if FCalc.Result = 0 then
      Result := False
    else
      Result := True;
  end
  else
    Result := Boolean(PLocateInfo(SrchConds.Objects[0])^.Evaluation);
end;

function TdbSearch.QualifiesFilter: Boolean;
begin
  if FHasFilter then
    Result := _QualifiesFilter(FSrchConds, FLogic)
  else
    Result := True;
end;

procedure TdbSearch.ClearFilter;
begin
  FHasFilter := False;
end;

end.

(*
// ==============================================================
// Copyright (c) 1999 Vijainder K Thakur. All rights reserved
// Version 4.0 Date: 31 Mar 1999
*)

