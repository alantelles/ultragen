unit LexerClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TokenClass, Tokens, StrUtils;

type
  TLexer = class
  private
    FPos: integer;
    FText: string;
    FCurrChar: string;
    FScriptLine, FLineChar: integer;
    FScopeType:TStringlist;
  public
    property PLineChar:integer read FLineChar;
    property PScriptLine:integer read FScriptLine;
    constructor Create(AText: string);
    procedure EParseError;
    procedure Advance(steps:integer=1);
    procedure SkipSpace;
    function Peek(Ahead:integer = 1):string;
    function GetNextToken: TToken;
    function GetId: TToken;
    function GetNumber: string;
    function GetString(Delim:string):TToken;
  end;

implementation

uses
  ExceptionsClasses;

constructor TLexer.Create(AText:string);
begin
  FText := AText;
  FPos := 1;
  FScriptLine := 1;
  FLineChar := 1;
  FCurrChar := FText[FPos];
  FScopeType := TStringList.Create;
end;

function TLexer.Peek(Ahead:integer = 1):string;
var
  APeekPos:integer;
  i :integer;
  Ret:string = '';
begin
  for i:=1 to Ahead do
  begin
    APeekPos := FPos + i;
    if APeekPos > Length(FText) then
      Result := NONE
    else
      Ret := ret + FText[APeekPos];
  end;
  Result := Ret;
end;

function TLexer.GetId: TToken;
var
  Ret:string = '';
  TokenFound: TToken;
begin
  Ret := FCurrChar;
  Advance;
  while (FCurrChar <> NONE) and (Pos(FCurrChar, LETTERS+SET_NUMBERS) > 0) do
  begin
    Ret := Ret + FCurrChar;
    Advance;
  end;
  Tokenfound := TToken(ReservedWords[ret]);
  if Tokenfound = nil then
    TokenFound := TToken.Create(T_ID, Ret)
  else
    FScopeType.Add(TokenFound.PType);
  Result := TokenFound;
end;

function TLexer.GetString(Delim:string):TToken;
var
  Ret:string = '';
  IsLong: boolean = False;
  TokenFound: TToken;
begin
  Advance;
  if Delim = T_STRENC_MULTI then
  begin
    Advance(2);
    IsLong := True;
  end;
  while (FCurrChar <> NONE) and
        (
          (not IsLong and (FCurrChar <> Delim) and ((FCurrChar + Peek(1)) <> sLineBreak)) or
          (IsLong and ((FCurrChar + Peek(2)) <> Delim))
        ) do

  begin
    Ret := Ret + FCurrChar;
    Advance;
  end;
  Advance(Length(Delim));
  Result := TToken.Create(TYPE_STRING, Ret);
end;

procedure TLexer.EParseError;
var
  msg:string;
begin
  msg := 'Unexpected character "'+ FCurrChar +'" at < Line: '+ IntToStr(FScriptLine) +', Char: '+ IntToStr(FLineChar) + ' >';
  raise ELexicalError.Create(msg);
end;

function TLexer.GetNumber:string;
var
  Return: string = '';
  GotDot: boolean = False;
begin
  while (Pos(FCurrChar, SET_NUMBERS) > 0) and (FCurrChar <> NONE) do
  begin
    Return := Return + FCurrChar;
    if FCurrChar = '.' then
    begin
      if Gotdot then
        EParseError
      else
        GotDot := True;
		end;
		Advance;
	end;
  Result := Return;
end;


procedure TLexer.SkipSpace;
begin
  while (FCurrChar = ' ') do
    Advance;
end;

procedure TLexer.Advance(steps:integer=1);
var
  i:integer;
begin
  for i:=1 to steps do
  begin
    FPos := FPos + 1;
    FLineChar := FLineChar + 1;
    if FPos > Length(FText) then
      FCurrChar := NONE
    else
      FCurrChar := FText[FPos];
  end;
end;


function TLexer.GetNextToken:TToken;
var
  AuxStr:string;
begin

  while FCurrChar <> NONE do
  begin
    if FCurrChar = ' ' then
    begin
      SkipSpace;
      continue;
    end;

    if (FCurrChar = ',') then
    begin
      Advance;
      Result := TToken.Create(T_COMMA, ',');
      exit
    end;

    if (FCurrChar + Peek(3)) = T_LANG_TRUE then
    begin
      Advance(4);
      Result := TToken.Create(TYPE_BOOLEAN, T_LANG_TRUE);
      exit
    end;

    if (FCurrChar + Peek(4)) = T_LANG_FALSE then
    begin
      Advance(5);
      Result := TToken.Create(TYPE_BOOLEAN, T_LANG_FALSE);
      exit
    end;

    if (FCurrChar = T_STRENC_SINGLE) then
    begin
      Result := GetString(T_STRENC_SINGLE);
      exit;
    end;

    if (FCurrChar + Peek(2)) = T_STRENC_MULTI then
    begin
      Result := GetString(T_STRENC_MULTI);
      exit;
    end;

    if (FCurrChar = T_STRENC_DOUBLE) then
    begin
      Result := GetString(T_STRENC_DOUBLE);
      exit;
    end;

    if (FCurrChar + Peek(2)) = 'end' then
    begin
      Advance(3);
      AuxStr := FScopeType[FSCopeType.Count-1];
      FScopeType.Delete(FScopeType.Count-1);
      Result := TToken.Create(T_END+AuxStr,'end of block '+AuxStr);
      exit
    end

    else if Pos(FCurrChar, LETTERS + '_' ) > 0 then
    begin
      Result := GetId();
      exit
    end;

    if FCurrChar = '=' then
    begin
      Advance;
      Result := TToken.Create(T_ASSIGN, '=');
      exit
    end;

    if (FCurrChar = #13) and (Peek(1) = #10)  then
    begin
      Advance;
      Advance;
      FScriptLine := FScriptLine + 1;
      FLineChar := 1;
      Result := TToken.Create(T_NEWLINE, #13#10);
      exit
    end;

    // leq, geq, neq
    if (FCurrChar = '<') and (Peek = '=') then
    begin
      Advance;
      Advance;
      Result := TToken.Create(T_LEQ, '<=');
      exit
    end;

    if (FCurrChar = '>') and (Peek = '=') then
    begin
      Advance;
      Advance;
      Result := TToken.Create(T_GEQ, '>=');
      exit
    end;

    if (FCurrChar = '!') and (Peek = '=') then
    begin
      Advance;
      Advance;
      Result := TToken.Create(T_NEQ, '!=');
      exit
    end;

    // end leq, neq, geq

    // and , or

    if (FCurrChar + Peek(1)) = '&&' then
    begin
      Advance;
      Advance;
      Result := TToken.Create(T_AND, '&&');
      exit
    end;

    if (FCurrChar + Peek(1)) = '||' then
    begin
      Advance;
      Advance;
      Result := TToken.Create(T_OR, '||');
      exit
    end;

    // end and, or

    if FCurrChar = '!' then
    begin
      Advance;
      Result := TToken.Create(T_NOT, '!');
      exit
    end;

    if FCurrChar = '>' then
    begin
      Advance;
      Result := TToken.Create(T_GT, '>');
      exit
    end;

    if FCurrChar = '<' then
    begin
      Advance;
      Result := TToken.Create(T_LT, '<');
      exit
    end;

    if (FCurrChar = '=') and (Peek = '=') then
    begin
      Advance;
      Advance;
      Result := TToken.Create(T_EQ, '==');
      exit
    end;


    if Pos(FCurrChar, SET_NUMBERS) > 0 then
    begin
      AuxStr := GetNumber();
      if (Pos('.', AuxStr) = 0) then
        Result := TToken.Create(TYPE_INTEGER, AuxStr)
      else
        Result := TToken.Create(TYPE_FLOAT, AuxStr);
      exit
		end;

    if FCurrChar = '+' then
    begin
      Advance;
      Result := TToken.Create(T_PLUS, '+');
      exit
		end;

    if FCurrChar = '*' then
    begin
      Advance;
      Result := TToken.Create(T_MULT, '*');
      exit
		end;

    
    if (FCurrChar + Peek(1)) = '//' then
    begin
      Advance;
      Advance;
      Result := TToken.Create(T_INT_DIV, '//');
      exit
		end;

    if FCurrChar = '/' then
    begin
      Advance;
      Result := TToken.Create(T_DIV, '/');
      exit
		end;

    if FCurrChar = '%' then
    begin
      Advance;
      Result := TToken.Create(T_MODULUS, '%');
      exit
		end;


    if FCurrChar = '-' then
    begin
      Advance;
      Result := TToken.Create(T_MINUS, '-');
      exit
		end;

    if FCurrChar = '(' then
    begin
      Advance;
      Result := TToken.Create(T_LPAREN, '(');
      exit
		end;

    if FCurrChar = ')' then
    begin
      Advance;
      Result := TToken.Create(T_RPAREN, ')');
      exit
		end;



    EParseError;
    Break;
	end;
  Result := TToken.Create(EOF, NONE);
end;

end.

