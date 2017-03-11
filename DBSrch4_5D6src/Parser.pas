unit parser;

interface

uses
  SysUtils, Windows;
const

  SIdentifierExpected = 'Identifier expected';
  SParseError = '%s on line %d';
  SStringExpected = 'String expected';
  SNumberExpected = 'Number expected';
  STokenExpected = 'Token: %d expected';
  SSymbolExpected = '%s expected';
  SInvalidNumber = 'Invalid numeric value';
  SInvalidString = 'Invalid string constant';


type
  EParserError = class(Exception);

type
  TToken = (
    { } toEof, toError,
    {7} toLbrace, toRbrace, toInteger, toFloat, toSymbol, toDate,
    {5} toInv, toNot,
    {4} toMul, toDiv, toMod, toPer,
    {3} toAdd, toSub,
    {2} toLt, toLe, toEq, toNe, toGe, toGt,
    {1} toOr, toXor, toAnd, toString
  );
  TMyParser = class(TObject)
  private
    FBuffer: PChar;
    FBufPtr: PChar;
    FBufEnd: PChar;
    FSource: PChar;
    FSourcePtr: PChar;
    FSourceEnd: PChar;
    FTokenPtr: PChar;
    FStringPtr: PChar;
    FSourceLine: Integer;
    FToken: TToken;
    procedure SkipBlanks;
    procedure Init(Buf: PChar);
  public
    procedure CheckToken(T: TToken);
    procedure CheckTokenSymbol(const S: string);
    procedure Error(const Ident: string);
    procedure ErrorFmt(const Ident: string; const Args: array of const);
    procedure ErrorStr(const Message: string);
    function NextToken: TToken;
    function SourcePos: Longint;
    function TokenFloat: Extended;
    function TokenInt: Longint;
    function TokenString: string;
    function TokenDate: Extended;
    function TokenSymbolIs(const S: string): Boolean;
    property SourceLine: Integer read FSourceLine;
    property Token: TToken read FToken;
    property Source: PChar read FSource write init;
  end;
implementation

procedure TMyParser.Init(Buf: PChar);
begin
  FSource := Buf;
  FBuffer := Buf;
  FBufPtr := FBuffer;
  FBufEnd := FBuffer + Sizeof(Buf);
  FSourcePtr := FBuffer;
  FSourceEnd := FBuffer;
  FTokenPtr := FBuffer;
  FSourceLine := 1;
  NextToken;
end;


procedure TMyParser.CheckToken(T: TToken);
begin
  if Token <> T then
    case T of
      toSymbol:
        Error(SIdentifierExpected);
      toString:
        Error(SStringExpected);
      toInteger, toFloat:
        Error(SNumberExpected);
    else
      ErrorFmt(STokenExpected, [Integer(T)]);
    end;
end;

procedure TMyParser.CheckTokenSymbol(const S: string);
begin
  if not TokenSymbolIs(S) then ErrorFmt(SSymbolExpected, [S]);
end;

procedure TMyParser.Error(const Ident: string);
begin
  ErrorStr(Ident);
end;

procedure TMyParser.ErrorFmt(const Ident: string; const Args: array of const);
begin
  ErrorStr(Format(Ident, Args));
end;

procedure TMyParser.ErrorStr(const Message: string);
begin
  raise EParserError.CreateFmt(SParseError, [Message, FSourceLine]);
end;

function TMyParser.NextToken: TToken;
var
  I: Integer;
  P, S: PChar;
begin
  SkipBlanks;
  P := FSourcePtr;
  FTokenPtr := P;
  case P^ of
    'A'..'Z', 'a'..'z', '_':
      begin
        Inc(P);
        while P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do Inc(P);
        Result := toSymbol;
      end;
    '#', '''':
      begin
        S := P;
        while True do
          case P^ of
            '#':
              begin
                Inc(P);
                I := 0;
                while P^ in ['0'..'9'] do
                begin
                  I := I * 10 + (Ord(P^) - Ord('0'));
                  Inc(P);
                end;
                S^ := Chr(I);
                Inc(S);
              end;
            '''':
              begin
                Inc(P);
                while True do
                begin
                  case P^ of
                    #0, #10, #13:
                      Error(SInvalidString);
                    '''':
                      begin
                        Inc(P);
                        if P^ <> '''' then Break;
                      end;
                  end;
                  S^ := P^;
                  Inc(S);
                  Inc(P);
                end;
              end;
          else
            Break;
          end;
        FStringPtr := S;
        Result := toString;
      end;
    '$':
      begin
        Inc(P);
        while P^ in ['0'..'9', 'A'..'F', 'a'..'f'] do Inc(P);
        Result := toInteger;
      end;
    '0'..'9':
      begin
        Inc(P);
        while P^ in ['0'..'9'] do Inc(P);
        Result := toInteger;
//        while P^ in ['0'..'9', '.', 'e', 'E', '+', '-'] do
        if P^ = '.' then
          while P^ in ['0'..'9', '.'] do
          begin
            Inc(P);
            Result := toFloat;
          end
        else if P^ = '/' then
          while P^ in ['0'..'9', '/'] do
          begin
            Inc(P);
            Result := toDate;
          end;
      end;
    '+':
      begin
        Inc(P);
        Result := toAdd;
      end;
    '-':
      begin
        Inc(P);
        Result := toSub;
      end;
    '*':
      begin
        Inc(P);
        Result := toMul;
      end;
    '/':
      begin
        Inc(P);
        Result := toDiv;
      end;
    '%':
      begin
        Inc(P);
        Result := toMod;
        if (P^ = '%') then
        begin
          Inc(P);
          Result := toPer;
        end;
      end;
    '~':
      begin
        Inc(P);
        Result := toInv;
      end;
    '^':
      begin
        Inc(P);
        Result := toXor;
      end;
    '&':
      begin
        Inc(P);
        Result := toAnd;
      end;
    '|':
      begin
        Inc(P);
        Result := toOr;
      end;
    '=':
      begin
        Inc(P);
        Result := toEq;
      end;
    '<':
      begin
        Inc(P);
        Result := toLt;
        if (P^ = '=') then
        begin
          Inc(P);
          Result := toLe;
        end
        else if(P^ = '>') then
        begin
          Inc(P);
          Result := toNe;
        end;
      end;
    '>':
      begin
        Inc(P);
        Result := toGt;
        if (P^ = '=') then
        begin
          Inc(P);
          Result := toGe;
        end
        else if(P^ = '<') then
        begin
          Inc(P);
          Result := toNe;
        end;
      end;
    '!':
      begin
        Inc(P);
        Result := toNot;
        if (P^ = '=') then
        begin
          Inc(P);
          Result := toNe;
        end;
      end;
    '(':
      begin
        Inc(P);
        Result := toLBrace;
      end;
    ')':
      begin
        Inc(P);
        Result := toRBrace;
      end;

    else
    begin
      if P^ <> Chr(0) then
      begin
        Result := toError;
        Dec(P);
      end
      else
        Result := toEof;
    end;
  end;
  FSourcePtr := P;
  FToken := Result;
end;


procedure TMyParser.SkipBlanks;
begin
  while True do
    if FSourcePtr^ in [#0, #33..#255] then
      Exit
    else
    Inc(FSourcePtr);
end;

function TMyParser.SourcePos: Longint;
begin
  Result := FTokenPtr - FBuffer + 1;
end;

function TMyParser.TokenFloat: Extended;
begin
  Result := StrToFloat(TokenString);
end;

function TMyParser.TokenInt: Longint;
begin
  Result := StrToInt(TokenString);
end;

function TMyParser.TokenString: string;
var
  L: Integer;
begin
  if FToken = toString then
    L := FStringPtr - FTokenPtr
  else
    L := FSourcePtr - FTokenPtr;
  SetString(Result, FTokenPtr, L);
end;

function TMyParser.TokenSymbolIs(const S: string): Boolean;
begin
  Result := (Token = toSymbol) and (CompareText(S, TokenString) = 0);
end;
function TMyParser.TokenDate: Extended;
begin
  Result := Double(StrToDate(TokenString));

end;

end.
