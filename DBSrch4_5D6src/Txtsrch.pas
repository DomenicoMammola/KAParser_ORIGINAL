unit Txtsrch;
interface
uses
  SysUtils, Classes;

const
   MAXMULTICHARWILDCARDLEN = 5;
type
  TMatchType = (mtStartsWith, mtWholeWord, mtSubString, mtPattern,
    mtEqualTo, mtGT, mtGTEqualTo, mtLT, mtLTEqualTo);

  TMatchInfo = record
    StartAt,
    EndAt: PChar;
    MatchString: String;
  end;

  TTextSearch = class(TComponent)
  private
    FMatchType: TMatchType;
    FPattern: Array [0..255] of char;
    FMultiCharWildCard: Array[0..MAXMULTICHARWILDCARDLEN -1]of Char;
    FSingleCharWildCard: Char;
    FMatchInfo: TMatchInfo;
    FStart: Pointer;
    FTarget: String;
    MultiWildCardLength: Integer;
    function GetMultiCharWildCard: String;
    procedure SetMultiCharWildCard(WildCard: String);
    function GetSearchPattern: String;
    procedure SetSearchPattern(Pattern: String);
    function StripNPAtTail(StrPtr: PChar): PChar;
    function GetMatchStart: Cardinal;
    function GetMatchEnd: Cardinal;
    function GetMatchString: String;
  protected
      function WholeWordMatch(Target: String): Boolean; virtual;
      function SubStringMatch(Target: String): Boolean; virtual;
      function StartsWithMatch(Target: String): Boolean; virtual;
      function PatternMatch(Target, Pattern: PChar): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsPatternWild(Pattern: PChar): Boolean;
    function IsPatternInString(Target: String; CaseSensitive: Boolean): Boolean;
    property MatchStart: Cardinal read GetMatchStart;
    property MatchEnd: Cardinal read GetMatchEnd;
    property MatchString: String read GetMatchString;
  published
    property MultiCharWildCard: String read GetMultiCharWildCard
      write SetMultiCharWildCard;
    property SingleCharWildCard: Char read FSingleCharWildCard
      write FSingleCharWildCard default '@';
    property SearchPattern: String read GetSearchPattern write SetSearchPattern;
    property MatchType: TMatchType read FMatchType write FMatchType
      default mtSubString;
end;

implementation

constructor TTextSearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MultiCharWildCard := '..';
  FSingleCharWildCard := '@';
  FMatchInfo.StartAt := nil;
  FMatchInfo.EndAt := nil;
  MatchType := mtSubString;
end;

destructor TTextSearch.Destroy;
begin
  inherited Destroy;
end;

function TTextSearch.GetMultiCharWildCard: String;
begin
   Result := StrPas(FMultiCharWildCard);
end;

procedure TTextSearch.SetMultiCharWildCard(WildCard: String);
begin
  if Length(WildCard) > MAXMULTICHARWILDCARDLEN then
    SetLength(WildCard, MAXMULTICHARWILDCARDLEN);
  StrPCopy(FMultiCharWildCard, WildCard);
  MultiWildCardLength := StrLen(FMultiCharWildCard);
end;

function TTextSearch.GetSearchPattern: String;
begin
  Result := StrPas(FPattern);
end;

procedure TTextSearch.SetSearchPattern(Pattern: String);
begin
  StrPCopy(FPattern, Pattern);
end;

function TTextSearch.IsPatternWild(Pattern: PChar): Boolean;
begin
  Result := (StrPos(Pattern, FMultiCharWildCard) <> nil) or
    (StrScan(Pattern, FSingleCharWildCard) <> nil);

end;

function TTextSearch.StartsWithMatch(Target: String): Boolean;
begin
  if Pos(SearchPattern, Target) <> 1 then
    Result := False
  else
  begin
    with FMatchInfo do
      EndAt := EndAt + Length(SearchPattern);
    Result := True;
  end;
end;

function TTextSearch.SubStringMatch(Target: String): Boolean;
var
  P: PChar; // Match Start
begin
  Result := False;
  P := StrPos(PChar(Target), PChar(SearchPattern));
  if P <> nil then
  begin
    with FMatchInfo do
    begin
      StartAt := P;
      EndAt := StartAt + StrLen(PChar(SearchPattern));
    end;
    Result := True;
  end;
end;

function TTextSearch.WholeWordMatch(Target: String): Boolean;
const
  Punctuations : set of #0..#255 = [' ', '.', ';', ':'];
var
  P: PChar; // Match Start
  PatternLength: Cardinal;
begin
  Result := False;
  PatternLength := StrLen(PChar(SearchPattern));
  P := StrPos(PChar(Target), PChar(SearchPattern));
  if P <> nil then
  begin
    if ((P + PatternLength = PChar(Target) + StrLen(PChar(Target))) or
     ((P + PatternLength)^ in Punctuations))  and
     ((P  = Target) or ((P - 1)^ in Punctuations)) then
    begin
      FMatchInfo.StartAt := P;
      FMatchInfo.EndAt := P + PatternLength;
      Result := True;
    end;
  end
  else
    Result := False;
end;

{Removes non printing characters at the end of a string}
function TTextSearch.StripNPAtTail(StrPtr: PChar): PChar;
var
  P: PChar;
begin
  P := StrPtr + StrLen(StrPtr);
  while Ord(P^) < 32 do
  begin
    P^ := #0;
    P := P - 1;
  end;
  Result := StrPtr;
end;

function TTextSearch.PatternMatch(Target, Pattern: PChar): Boolean;
var
  NextWC, P: PChar;
//  TargetStart: PChar;

  SearchStr: Array[0..255] of char;
begin
  {To leverage the existing code we use PChars}
//  TargetStart := Target;

  if 0 = StrComp(Pattern, FMultiCharWildCard) then
  begin
    FMatchInfo.EndAt := Target;
    Result := True
  end
  else if (Target^ = Chr(0)) and (Pattern^ <> Chr(0)) then
    Result := False
  else if Target^ = Chr(0) then
  begin
    FMatchInfo.EndAt := Target;
    Result := True;
  end
  {The pattern starts with a multi char wild card}
  else if 0 = StrLComp(Pattern, FMultiCharWildCard, MultiWildCardLength) then
  begin
    {and there are no wild card characters ahead}
    if not IsPatternWild(Pattern + MultiWildCardLength) then
    begin
      P := StrPos(Target, Pattern + MultiWildCardLength);
      {If pattern is at the end of the target}
      if (P <> nil)
      and (((StrLen(P) = StrLen(Pattern + MultiWildCardLength)) or
      (StrLen(StripNPAtTail(P)) = StrLen(Pattern + MultiWildCardLength))))
      then
      begin
        if FMatchInfo.StartAt = FStart then
          FMatchInfo.StartAt := P;
        Result := True;
        FMatchInfo.EndAt := FMatchInfo.StartAt + StrLen(P);
      end
      else
        Result := False;
    end
    else
    {There are wild card characters ahead}
    begin
      {Detect any wild cards ahead}
      NextWC := StrPos(Pattern + MultiWildCardLength, FMultiCharWildCard);
      P := StrScan(Pattern + MultiWildCardLength, FSingleCharWildCard);
      if (NextWC = nil) then
        NextWC := P
      else if (P <> nil) and (P < NextWC) then
        NextWC := P;
      // Extract the substring between the wildcards
      StrLCopy(SearchStr, Pattern + MultiWildCardLength,
        NextWC - Pattern - MultiWildCardLength);

      P := StrPos(Target, SearchStr);
      {We didn't find the part after the wild card '..' or we found
      it at the very start!
      The line below amended on the suggestion of Reinhard Kalinke
      100417.3504@compuserve.com. Thank you Reinhard!}
      {The string within the wild cards of the pattern could not be found in the target}
      if (P = nil) {or (P = Target)} then
        Result := False
      else
      begin
        if FMatchInfo.StartAt = FStart then
          FMatchInfo.StartAt := P;
        // The following code eliminates a bug due to which a pattern match was
        // being prematurely declared false. The bug was pointed out by Ralph Stoesser
        // of Institut fuer Rechtsgeschichte,
        // Freie Universitaet Berlin (rastoess@zedat.fu-berlin.de) Thanks Ralph
        while True do
        begin
          Result := PatternMatch(P + StrLen(SearchStr), NextWC);
          if not Result then
          begin
            P := StrPos(P + StrLen(SearchStr), SearchStr);
            if (P = nil) then
            begin
              Result := False;
              break;
            end
            else
              FMatchInfo.StartAt := P;
          end
          else
            break;
        end;

      end;
    end;
  end
  else if Pattern^ = FSingleCharWildCard then
  begin
    repeat
      Inc(Pattern);
      Inc(Target);
    until (Pattern^ <> FSingleCharWildCard) or (Target^ = Char(0));
    Result := PatternMatch(Target, Pattern);
  end
  else if Target^ = Pattern^ then
  begin
    if FMatchInfo.StartAt = FStart then
      FMatchInfo.StartAt := Target;
    repeat
      Inc(Pattern);
      Inc(Target);
    until (Pattern^ = Chr(0)) or (Target^ = Char(0)) or (Target^ <> Pattern^);
    FMatchInfo.EndAt := Target;
    Result := PatternMatch(Target, Pattern)
  end
  else
    Result := False;
end;

function TTextSearch.IsPatternInString(Target: String;
  CaseSensitive: Boolean): Boolean;
begin

  if(not CaseSensitive)then
  begin
    Target := UpperCase(Target);
    SearchPattern := UpperCase(SearchPattern);
  end;
  // Initialize the Match Info structure
  FStart := PChar(Target);
  FTarget := Target;
  with FMatchInfo do
  begin
    StartAt := FStart;
    EndAt := FStart;
    MatchString := '';
  end;
  if MatchType = mtWholeWord then
    Result := WholeWordMatch(Target)
  else if MatchType = mtSubString then
    Result := SubStringMatch(Target)
  else if MatchType = mtStartsWith then
    Result := StartsWithMatch(Target)
  else
    Result := PatternMatch(PChar(Target), PChar(SearchPattern));

end;

function TTextSearch.GetMatchStart: Cardinal;
begin
  Result := FMatchInfo.StartAt - FStart + 1;
end;

function TTextSearch.GetMatchEnd: Cardinal;
begin
  Result := FMatchInfo.EndAt - FStart + 1;
end;

function TTextSearch.GetMatchString: String;
begin
  Result := Copy(FTarget, GetMatchStart, GetMatchEnd - GetMatchStart);
end;

end.
(*
// ==============================================================
// Copyright (c) 1996 by Aaron Castro and VK Thakur. All rights reserved
// Version 3.01   Date: 30/05/95
*)

