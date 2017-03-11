unit Calc;
interface

uses
  Sysutils, Parser;
const
  SSyntaxError = 'Syntax error.';

type
  ECalculate = class(Exception);

  TCalculator = class(TMyParser)
    private
      procedure RaiseError(const ErrMsg: String);
      function CExtended(B: Boolean): Extended;
      procedure Expr2(var R: Extended);
      procedure Expr3(var R: Extended); // ADD and Subtract
      procedure Expr4(var R: Extended); // MUL, DIV, MOD, PERCENT
      procedure Expr5(var R: Extended);
      procedure Expr6(var R: Extended);
      procedure Term(var R: Extended);
      procedure SetExpression(const Exp: String);
      function GetResult: Extended;
      function GetExpression : String;
    protected
    public
      property Expression: String read GetExpression write SetExpression;
      property Result: Extended read GetResult;
  end;


implementation

function TCalculator.CExtended(B: Boolean): Extended;
begin
  if (B) then CExtended := 1.0 else CExtended := 0.0;
end;

procedure TCalculator.Term(var R: Extended);
var
  OldToken: TToken;
  S: String;
begin
  Case Token of
    toInteger,
    toFloat:
      begin
        R := TokenFloat;
        NextToken;
      end;
    toDate:
      begin
        R := TokenDate;
        NextToken;
      end;
    toString:
      begin
        S := TokenString;
        NextToken;
        if token in [toLt, toLe, toEq, toNe, toGe, toGt] then
        begin
          OldToken := Token;
          NextToken;
          if (Token = toString) then
          case OldToken of
            toLt: R := CExtended(S < TokenString);
            toLe: R := CExtended(S <= TokenString);
            toEq: R := CExtended(S = TokenString);
            toNe: R := CExtended(S <> TokenString);
            toGe: R := CExtended(S >= TokenString);
            toGt: R := CExtended(S > TokenString);
          end;
          NextToken;
        end;
      end;
    toLBrace:
      begin
        NextToken;
        Expr6(R);
        if (token = toRBrace) then
          NextToken
        else
          RaiseError(SSyntaxError);
      end;
    else {case}
      RaiseError(SSyntaxError);
  end;
end;


procedure TCalculator.Expr2(var R: Extended);
var
  OldToken: TToken;
begin
  if Token in [toNot, toInv, toAdd, toSub] then
  begin
    OldToken := Token;
    NextToken;
    Expr2(R);
    case OldToken of
      toNot:
        if Trunc(R) = 0 then
          R := 1.0
        else
          R := 0.0;
      toInv: R := (not Trunc(R));
      toAdd:;
      toSub: R := -R;
    end;
  end
  else
    Term(R);
end;

procedure TCalculator.Expr3(var R: Extended);
var
  OldToken: TToken;
  V: Extended;
begin
  Expr2(R);
  while token in [toMul, toDiv, toMod, toPer] do
  begin
    OldToken := Token;
    NextToken;
    Term(V);
    case OldToken of
      toMul: R := R * V;
      toDiv: R := R / V;
      toMod: R := Trunc(R) mod Trunc(V);
      toPer: R := R * V / 100;
    end;
  end;
end;

procedure TCalculator.Expr4(var R: Extended);
var
  V: Extended;
  OldToken: TToken;
begin
  Expr3(R);
  while token in [toAdd, toSub ] do begin
    OldToken := Token;
    NextToken;
    Expr3(V);
    case OldToken of
      toAdd: R := R + V;
      toSub: R := R - V;
    end;
  end;
end;

procedure TCalculator.Expr5(var R: Extended);
var
  V: Extended;
  OldToken : TToken;
begin
  Expr4(R);
  while token in [toLt, toLe, toEq, toNe, toGe, toGt] do begin
    OldToken := Token;
    NextToken;
    Expr4(V);
    case OldToken of
      toLt: R := CExtended(R < V);
      toLe: R := CExtended(R <= V);
      toEq: R := CExtended(R = V);
      toNe: R := CExtended(R <> V);
      toGe: R := CExtended(R >= V);
      toGt: R := CExtended(R > V);
    end;
  end;
end;

procedure TCalculator.Expr6(var R: Extended);
var
  V: Extended;
  OldToken : TToken;
begin
  Expr5(R);
  while token in [toOr, toAnd, toXor] do begin
    OldToken := Token;
    NextToken;
    Expr5(V);
    case OldToken of
      toOr: R := Trunc(R) or Trunc(V);
      toAnd: R := Trunc(R) and Trunc(V);
      toXor: R := Trunc(R) xor Trunc(V);
    end;
  end;
end;

function TCalculator.GetResult: Extended;
begin
  Expr6(Result);
  if not (Token = toEOF) then
    RaiseError(SSyntaxError);
end;

function TCalculator.GetExpression: String;
begin
  Result := Source;
end;

procedure TCalculator.SetExpression(const Exp: String);
begin
  Source := PChar(Exp);
end;
procedure TCalculator.RaiseError(const ErrMsg: String);
begin
  raise ECalculate.Create(ErrMsg);
end;
end.


