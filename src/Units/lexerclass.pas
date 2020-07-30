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
    FExtension, FFileName: string;
    FCurrChar: string;
    FScriptLine, FLineChar: integer;
    FScopeType: TStringList;
    FScriptMode, FLineScript, FInterpol: boolean;
  public
    property PExtension: string read FExtension;
    property PFileName: string read FFileName;
    property PLineChar: integer read FLineChar;
    property PScriptLine: integer read FScriptLine;
    property PScriptMode: boolean read FScriptMode write FScriptMode;
    constructor Create(AFileName: string; fromFile: boolean = True);
    procedure EParseError;
    procedure Advance(steps: integer = 1);
    procedure Rewind(steps: integer = 1);
    procedure SkipSpace;
    function Peek(Ahead: integer = 1): string;
    function GetNextToken: TToken;
    function GetId: TToken;
    function GetModulePath: TToken;
    function GetNumber: string;
    function PassLineComment: string;
    function GetString(Delim: string): TToken;
    function GetInnerAttribute: TToken;
    function GetPlainText: TToken;

  end;

implementation

uses
  ExceptionsClasses;

constructor TLexer.Create(AFileName: string; fromFile: boolean = True);
var
  dotpos: integer;
  ext: string;
  AFile: TStringList;
  Acd, Adir, Afn:string;
begin
  FScriptMode := False;
  FLineScript := False;
  FInterpol := False;
  AFile := TStringList.Create;


  Acd := GetCurrentDir;
  if Length(Acd) > 0 then
    Acd := Acd + DirectorySeparator;
  Adir := ExtractFilePath(AFileName);
  if Length(Adir) > 0 then
    Adir := Adir + DirectorySeparator;
  AFn := ExtractFileName(AFileName);
  FFileName := Acd + ADir + Afn;
  try
    try
      if fromFile then
      begin
        AFile.LoadFromFile(AFileName);
        AFile.SkipLastLineBreak := True;
        AFile.LineBreak := #10;
        dotpos := RPos('.', AFileName);
        if dotpos > 0 then
          ext := Copy(AFilename, dotpos, Length(AFileName));
        if ext = '.ultra' then
          FScriptMode := True;
        FExtension := ext;
        FText := AFile.Text;
      end
      else
      begin
        FExtension := '.ultra';
        FText := Trim(AFileName);
        FScriptMode := True;
      end;
      if Length(FText) = 0 then
        exit;
      FPos := 1;
      FScriptLine := 1;
      FLineChar := 1;
      FCurrChar := FText[FPos];
      FScopeType := TStringList.Create;
    except
      raise ERunTimeError.Create('The specified file "' + AFileName + '" does not exist', AFileName, -1, -1);
    end;

  finally
    AFile.Free;
  end;
end;

function TLexer.GetPlainText: TToken;
var
  ret: string = '';
begin
  // texto {{
  while (FCurrchar + Peek(1) <> '{{') and
        (FCurrChar <> NONE)  do
  begin
    if (FCurrChar = #10)  then
    begin
      ret := ret + FCurrChar;
      break
    end
		else
    begin
      ret := ret + FCurrChar;
      Advance;
		end;
  end;
  Result := TToken.Create(T_PLAIN_TEXT, Ret, FScriptLine, FLineChar, FFileName);
end;

function TLexer.GetInnerAttribute: TToken;
var
  iToken: integer;
  ret: string = '';
begin
  Advance;
  Advance;
  while Pos(FCurrChar, LETTERS) > 0 do
  begin
    Ret := Ret + FCurrChar;
    advance;
  end;
  //TF := TToken(InnerAttributes[Ret]);
  //TF := TToken(Inners.Find(Ret));
  iToken := Inners.IndexOfName(Ret);
  if (iToken > -1) and (Ret = Inners.Names[iToken]) then
    Result := TToken.Create(Inners.ValueFromIndex[iToken], Ret, FScriptLine, FLineChar, FFileName)
  else
    EParserError.Create('Undefined referenced inner attribute "' + Ret + '"', FFileName, FScriptLine, FLineChar);
end;

function TLexer.PassLineComment: string;
begin
  while (FCurrChar <> #10) and (FCurrChar <> EOF) do
  begin
    Advance();
  end;
  Result := '';
end;

function TLexer.Peek(Ahead: integer = 1): string;
var
  APeekPos: integer;
  i: integer;
  Ret: string = '';
begin
  for i := 1 to Ahead do
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
  Ret: string = '';
  FoundType: string;
  iToken: integer;
begin
  Ret := FCurrChar;
  Advance;
  while (FCurrChar <> NONE) and (Pos(FCurrChar, LETTERS + SET_NUMBERS + '_') > 0) do
  begin
    Ret := Ret + FCurrChar;
    Advance;
  end;
  iToken := KW.IndexOfName(Ret);
  if (iToken > -1) and (KW.Names[iToken] = Ret) then
  begin
    FoundType := KW.ValueFromIndex[iToken];
    if Copy(FoundType, 1, 6) = 'block:' then
      FScopeType.Add(FoundType);
    Result := TToken.Create(FoundType, FoundType, FScriptLine, FLineChar, FFileName);
  end
  else
    Result := TToken.Create(T_ID, Ret, FScriptLine, FLineChar, FFileName);
end;

function TLexer.GetModulePath: TToken;
var
  Ret: string = '';
  FoundType: string;
  iToken: integer;
begin
  Ret := FCurrChar;
  Advance;
  while (FCurrChar <> NONE) and (Pos(FCurrChar, LETTERS + SET_NUMBERS + '_' + '.') > 0) do
  begin
    Ret := Ret + FCurrChar;
    Advance;
  end;
  Result := TToken.Create(T_MODULE_PATH, Ret, FScriptLine, FLineChar, FFileName);
end;

function TLexer.GetString(Delim: string): TToken;
var
  Ret: string = '';
  IsLong: boolean = False;
begin
  Advance;
  if Delim = T_STRENC_MULTI then
  begin
    Advance(Length(T_STRENC_MULTI) - 1);
    IsLong := True;
  end;
  while (FCurrChar <> NONE) and
    ((not IsLong and (FCurrChar <> Delim) and
      ((FCurrChar + Peek(1)) <> sLineBreak)) or (IsLong and
      ((FCurrChar + Peek(2)) <> Delim))) do

  begin
    if FCurrChar = ESCAPE_SYMBOL then
    begin
      Advance;
      if FCurrChar = 'n' then
        Ret := Ret + #10
      else if FCurrChar = 't' then
        Ret := Ret + #9
      else if FCurrChar = 'r' then
        Ret := Ret + #13
      else
        Ret := Ret + FCurrChar;
      Advance;
      Continue;
    end;
    Ret := Ret + FCurrChar;
    Advance;
  end;
  Advance(Length(Delim));
  Result := TToken.Create(TYPE_STRING, Ret, FScriptLine, FLineChar, FFileName);
end;

procedure TLexer.EParseError;
var
  msg: string;
begin
  msg := 'Unexpected character "' + FCurrChar + '" at < Line: ' +
    IntToStr(FScriptLine) + ', Char: ' + IntToStr(FLineChar) + ' >';
  ELexicalError.Create(msg, FFileName, FScriptLine, FLineChar);
end;

function TLexer.GetNumber: string;
var
  Return: string = '';
  GotDot: boolean = False;
begin
  while (Pos(FCurrChar, SET_NUMBERS + '.') > 0) and (FCurrChar <> NONE) do
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
  while (FCurrChar = ' ') or (FCurrChar = #13) do
    Advance;
end;

procedure TLexer.Advance(steps: integer = 1);
var
  i: integer;
begin
  for i := 1 to steps do
  begin
    FPos := FPos + 1;
    FLineChar := FLineChar + 1;
    if FPos > Length(FText) then
      FCurrChar := NONE
    else
      FCurrChar := FText[FPos];
  end;
end;

procedure TLexer.Rewind(steps: integer = 1);
var
  i: integer;
begin
  for i := 1 to steps do
  begin
    FPos := FPos - 1;
    FLineChar := FLineChar - 1;
    if FPos > Length(FText) then
      FCurrChar := NONE
    else
      FCurrChar := FText[FPos];
  end;
end;

function TLexer.GetNextToken: TToken;
var
  AuxStr: string;
begin

  while FCurrChar <> NONE do
  begin
    if FScriptMode then
    begin
          {$INCLUDE 'lexer_script_mode_chars.pp'}
    end
    else
    begin
          {$INCLUDE 'lexer_plain_text_mode_chars.pp'}
    end;
    EParseError;
    Break;
  end;
  Result := TToken.Create(EOF, NONE, FScriptLine, FLineChar, FFileName);
end;

end.


