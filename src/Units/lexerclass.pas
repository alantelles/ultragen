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
    function PassLineComment: string;
    function GetString(Delim:string):TToken;
  end;

implementation

uses
  ExceptionsClasses;

constructor TLexer.Create(AText:string);
begin
  FText := AText;
  if Length(FText) = 0 then
    exit;
  FPos := 1;
  FScriptLine := 1;
  FLineChar := 1;
  FCurrChar := FText[FPos];
  FScopeType := TStringList.Create;
end;

function TLexer.PassLineComment: string;
begin
  {$IFDEF Unix}
  while FCurrChar <> SLineBreak do
  begin
    Advance();
	end;
  {$ENDIF}
  {$IFDEF Windows}
  while (FCurrChar + Peek(1)) <> SLineBreak do
    Advance();
  {$ENDIF}
  Result := '';
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
  begin
    if (TokenFound.PType <> T_ELSE) and
       (TokenFound.PType <> T_ELSE_IF)
       then
      FScopeType.Add(TokenFound.PType);
  end;
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
    Advance(Length(T_STRENC_MULTI) - 1);
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
  while (Pos(FCurrChar, SET_NUMBERS+'.') > 0) and (FCurrChar <> NONE) do
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

    if (FCurrChar = T_STRENC_SINGLE) then
    begin
      Result := GetString(T_STRENC_SINGLE);
      exit;
    end;


    if (FCurrChar = T_LINE_COMMENT) then
    begin
      Advance;
      Result := TToken.Create(T_COMMENT, PassLineComment);
      exit
    end;

    if (FCurrChar + Peek(Length(T_STRENC_MULTI) - 1)) = T_STRENC_MULTI then
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
    end;



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

    if FCurrChar = '=' then
    begin
      Advance;
      Result := TToken.Create(T_ASSIGN, '=');
      exit
    end;
    {$IFDEF UNIX}
    if (FCurrChar = sLineBreak)  then
    begin
      Advance;
      FScriptLine := FScriptLine + 1;
      FLineChar := 1;
      Result := TToken.Create(T_NEWLINE, sLineBreak);
      exit
    end;
    {$ENDIF}
    {$IFDEF Windows}
    if (FCurrChar + Peek(1)) = sLineBreak  then
    begin
      Advance;
      Advance;
      FScriptLine := FScriptLine + 1;
      FLineChar := 1;
      Result := TToken.Create(T_NEWLINE, sLineBreak);
      exit
    end;
    {$ENDIF}

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

    if FCurrChar = '[' then
    begin
      Advance;
      Result := TToken.Create(T_LIST_START, T_LIST_START);
      Exit
    end;

    if FCurrChar = ']' then
    begin
      Advance;
      Result := TToken.Create(T_LIST_END, T_LIST_END);
      exit
    end;


    if Pos(FCurrChar, LETTERS + '_' ) > 0 then
    begin
      Result := GetId();
      exit
    end;

    if FCurrChar = '.' then
    begin
      Advance;
      Result := TToken.Create(T_ATTR_ACCESSOR, ATTR_ACCESSOR);
      exit;
    end;

    if (FCurrChar = ',') then
    begin
      Advance;
      Result := TToken.Create(T_COMMA, ',');
      exit
    end;

    EParseError;
    Break;
	end;
  Result := TToken.Create(EOF, NONE);
end;

end.

