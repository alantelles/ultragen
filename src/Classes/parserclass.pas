unit ParserClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, DateUtils, Process,

  { Globals }
  TypesGlobals, VariablesGlobals, ConstantsGlobals,

  { Classes }
  TemplateClass, GenFileClass, GenFileSetClass;

type
  TTempParser = class
  private
    FTemplate: TTemplate;
    FLineTime: TDateTime;
    FTokenPos: integer;
  public
    constructor Create(var ATemplate: TTemplate);
    property LineTime: TDateTime read FLineTime;
    class function IsInToken(Line, Test: string): boolean; static;
    function ParseLine(Line: string): TParseResult;
    function ParseToken(AToken: string): string;
    function ParseTemplate(var OutputParsedLines: TStringList): TTempParser;
    function IsFunction(AToken: string): boolean;
    function IsReserved(AToken: string): boolean;
    function IsVari(AToken: string): boolean;
    function IsLiteralString(AToken: string): boolean;
    function IsTimeStr(AToken: string): boolean;
    function IsNumber(AToken: string): boolean;
    function IsImported(AToken: string): boolean;
    function IsFromGenSet(AToken: string): boolean;
    function IsFromParent(AToken: string): boolean;
    function IsAnAlias(AToken: string): boolean;
    function GetLiteral(AToken: string): string;
    function GetTimeStr(AToken: string): string;
    function ParseParams(AList: string; var ArgsAsList: TStringList): TTempParser;
    function PrintSection(ASection: string): string;
    function ParseFunction(AFuncName: string; var Params: TStringList): string;
    function ExternalExists(AFuncPath: string): boolean;
    function ExternalPath(AFuncPath: string): string;
    function CallExternal(ExtPath, FuncName: string; var Params: TStringList): string;
    function CallExternal(ExtPath: string; var Params: TStringList): string;
    function InsertTemplate(ATempName, AgenName: string): string;
    function InsertTemplate(ATempName: string): string;
    function PrintPlainText(AFileName:string): string;
  end;

implementation

uses
  DateTimeFunctions,
  BooleansFunctions,
  FileHandlingUtils,
  StringsFunctions,
  ListFunctions;

constructor TTempParser.Create(var ATemplate: TTemplate);
begin
  FTemplate := ATemplate;
end;

function TTempParser.IsAnAlias(AToken: string): boolean;
var
  Return: boolean = False;
  Line: string;
begin
  for Line in FTemplate.Imported do
  begin
    if Line = AToken then
    begin
      Return := True;
      Break;
    end;
  end;
  Result := Return;
end;

function TTempParser.IsFromParent(AToken: string): boolean;
begin
  Result := Pos('PARENT.@', AToken) = 1;
end;

function TTempParser.IsReserved(AToken:string): boolean;
var
  s:string;
  Return:boolean=False;
begin
  for s in RESERVED_WORDS do
  begin
    if s = AToken then
    begin
      Return := True;
      Break
    end;
  end;
  Result := Return;
end;

function TTempParser.IsFunction(AToken: string): boolean;
var
  OpenPoint, ClosePoint: integer;
  OnlyAllowedChars: boolean = True;
  c: integer;
  FuncName: string;
begin
  OpenPoint := Pos(PARAM_OPEN, AToken);
  ClosePoint := RPos(PARAM_CLOSE, AToken);
  FuncName := Trim(Copy(AToken, 1, OpenPoint - 1));
  for c := 1 to Length(FuncName) do
  begin
    if ((c > 1) and (Pos(FuncName[c], FUNCTION_ALLOWED + NUMBERS + FUNC_SYMBOLS) = 0)) or
      ((c = 1) and (Pos(FuncName[c], FUNCTION_ALLOWED) = 0)) then
    begin
      OnlyAllowedChars := False;
    end;
  end;
  Result := (OpenPoint < ClosePoint) and OnlyAllowedChars;
end;

function TTempParser.IsVari(AToken: string): boolean;
begin
  Result := Pos(OVER_STATE, AToken) = 1;
end;

function TTempParser.IsLiteralString(AToken: string): boolean;
var
  Part: string;
  StrStart, StrEnd, NoQuotesInside: boolean;
begin
  StrStart := Pos(STR_ENCLOSE, AToken) = 1;
  StrEnd := RPos(STR_ENCLOSE, AToken) = Length(AToken);
  Part := Copy(AToken, 2, Length(AToken) - 2);
  NoQuotesInside := Pos(STR_ENCLOSE, Part) = 0;
  Result := StrStart and StrEnd and NoQuotesInside;
end;

function TTempParser.IsTimeStr(AToken: string): boolean;
var
  Return: boolean = False;
  AStr: string;
begin
  for AStr in TIME_STR do
  begin
    if AStr = AToken then
    begin
      Return := True;
      Break;
    end;
  end;
  Result := Return;
end;

function TTempParser.IsNumber(AToken: string): boolean;
var
  Return: boolean;
begin
  try
    StrToInt(AToken);
    Return := True;
  except
    Return := False;
  end;
  Result := Return;
end;

class function TTempParser.IsInToken(Line, Test: string): boolean;
var
  Part, Disc: string;
  i: integer;
  TkOpen, StrOpen: boolean;
begin
  //{@fifth} teste @fifth {Uppercase(@fifth)} {Uppercase('@fifth')}
  Part := '';
  Disc := '';
  TkOpen := False;
  StrOpen := False;
  for i := 1 to Length(Line) do
  begin
    if (Line[i] = TOKEN_OPEN) and (not StrOpen) then
    begin
      TkOpen := True;
    end
    else if (Line[i] = STR_ENCLOSE) and (TkOpen) then
    begin
      StrOpen := not StrOpen;
    end;
  end;
  Result := True;
end;

function TTempParser.IsImported(AToken: string): boolean;
var
  PosAttr, PosOver: integer;
begin
  PosAttr := Pos(ATTR_ACCESSOR, AToken);
  PosOver := Pos(OVER_STATE, AToken);
  //zika.telles  true
  //zika.@telles true
  //zika.telles() false
  //.tomorino false
   { if (((PosAttr > 1) and (PosOver > PosAttr)) or
       ((PosAttr > 1) and (PosOver = 0))) and (not IsFunction(AToken)) then}
  if (PosAttr > 1) and ((PosOver = 0) or (PosOver > PosAttr)) and
    (not IsFunction(AToken)) then
    Result := True
  else
    Result := False;
end;

function TTempParser.IsFromGenSet(AToken: string): boolean;
var
  PosSetRef, PosAttr, PosOver: integer;
begin
  AToken := Trim(AToken);
  PosSetRef := Pos(FROM_GEN_SET, AToken);
  PosAttr := Pos(ATTR_ACCESSOR, AToken);
  //&genfile.akey true
  //&genfile.akey() false
  if (PosSetRef = 1) and (PosAttr > 2) and (not IsFunction(AToken)) then
    Result := True
  else
    Result := False;
end;

function TTempParser.PrintSection(ASection: string): string;
var
  i: integer;
  Line: string;
  APair: TKVPair;
  ATemplate: TTemplate;
  OutputParsed: TStringList;
  AGen: TGenFileSet;
begin
  OutputParsed := TStringList.Create;
  ASection := Trim(ASection);
  i := FTemplate.Sections.IndexOf(ASection);
  ATemplate := TTemplate.Create;
  ATemplate.Load(TStringList(FTemplate.Sections.Objects[i]), FTemplate.Sections[i]);
  ATemplate.Variables := FTemplate.Variables;
  AGen := TGenFileSet.Create;
  AGen := FTemplate.GenFileSet;
  ATemplate.ParseTemplate(AGen);
  Line := ATemplate.ParsedLines.Text;
  OutputParsed.Free;
  Result := Copy(Line, 1, Length(Line) - Length(sLineBreak));
end;

function TTempParser.ParseLine(Line: string): TParseResult;
const
  F = False;
  V = True;
  ANY = '';
var
  Expected, Token, k, ParsedLine: string;
  i: integer;
  StrOpen, tkOpen, Maybe, EscapeNext: boolean;
  Return: TParseResult;
begin
  StrOpen := F;
  tkOpen := F;
  Maybe := F;
  EscapeNext := F;
  Token := '';
  ParsedLine := '';
  for i := 1 to length(Line) do
  begin
    k := Line[i];
    if (Line[i] = ESCAPER) and (not EscapeNext) and (not StrOpen) then
    begin
      EscapeNext := V;
      continue;
    end;
    if EscapeNext then
    begin
      k := Line[i];
      if not tkOpen then
        ParsedLine := ParsedLine + Line[i]
      else
        Token := Token + Line[i];
      EscapeNext := False;
      continue;
    end;
    if (Line[i] = FTemplate.TokenClose) and (not StrOpen) and tkOpen then
    begin
      Maybe := True;
      Token := Token + Line[i];
      Expected := FTemplate.TokenClose;
    end;
    if (Line[i] = Expected) and (tkOpen) and (not StrOpen) and (Maybe) then
    begin
      tkOpen := False;
      Expected := ANY;
      Token := Token + Line[i];
      Token := Copy(Token, 3, Length(Token) - Length(FTemplate.TokenOpen) *
        2 - Length(FTemplate.TokenClose) * 2);

      ParsedLine := ParsedLine + ParseToken(Trim(Token));
      Token := '';
      continue;
    end;
    if (Line[i] = STR_ENCLOSE) and tkOpen then
    begin
      StrOpen := not StrOpen;
    end;
    if (Expected = ANY) and (Line[i] = FTemplate.TokenOpen) and (not StrOpen) then
    begin
      FTokenPos := i - 2;
      Maybe := True;
      Expected := FTemplate.TokenOpen;
    end;
    if (Line[i] = Expected) and (Maybe) and (not tkOpen) and (not StrOpen) then
    begin

      tkOpen := True;
      Maybe := False;
      Expected := ANY;
    end;

    if tkOpen then
      Token := Token + Line[i];

    if (not tkOpen) and (Line[i] <> FTemplate.TokenClose) then
      ParsedLine := ParsedLine + Line[i];
  end;
  Return.Value := ParsedLine;
  Return.ParseOk := True;
  Result := Return;
end;

function TTempParser.GetTimeStr(AToken: string): string;
var
  ADateTime: TDateTime;
  StrDate: string;
  i: integer;
begin
  StrDate := '';
  if AToken = 'NOW' then
    StrDate := FormatDateTime(DATE_INTERCHANGE_FORMAT, Now)
  else if AToken = 'LINE' then
    StrDate := FormatDateTime(DATE_INTERCHANGE_FORMAT, FLineTime)
  else if AToken = 'GEN' then
    StrDate := FormatDateTime(DATE_INTERCHANGE_FORMAT, FTemplate.GenTime)
  else
  begin
    StrDate := '';
    for i := 1 to Length(DATE_INTERCHANGE_FORMAT) do
    begin
      if DATE_INTERCHANGE_FORMAT[i] = '-' then
        StrDate := StrDate + '-'
      else if DATE_INTERCHANGE_FORMAT[i] = ':' then
        StrDate := StrDate + ':'
      else if DATE_INTERCHANGE_FORMAT[i] = '.' then
        StrDate := StrDate + '.'
      else if DATE_INTERCHANGE_FORMAT[i] = ' ' then
        StrDate := StrDate + ' '
      else
      begin
        if i <= Length(AToken) then
          StrDate := StrDate + AToken[i]
        else
          StrDate := StrDate + '0';
      end;
    end;
  end;
  Result := StrDate;
end;

function TTempParser.GetLiteral(AToken: string): string;
var
  Temp, Return: string;
begin

  Temp := Trim(AToken);
  if Length(Temp) > 0 then
  begin
    if IsReserved(AToken) then
      Return := AToken
    else if IsNumber(AToken) then
      Return := AToken
    else if IsTimeStr(AToken) then
      Return := GetTimeStr(AToken)
    else if (IsLiteralString(AToken)) then
      //It's a string literal
      Return := Copy(Temp, 2, Length(Temp) - 2)
    else if (Copy(Temp, 1, Length(OVER_STATE)) = OVER_STATE) then
      //It's a value from a variable
      Return := FTemplate.GetVariable(Copy(Temp, Pos(OVER_STATE, Temp) + 1,
        Length(Temp)))
    else if (Temp[1] <> STR_ENCLOSE) and (Temp[Length(Temp)] <> STR_ENCLOSE) then
      Return := FTemplate.GenFile.GetValue(Temp).Value;
  end
  else
    Return := '';
  Result := Return;
end;

function TTempParser.PrintPlainText(AFileName:string): string;
var
  TextLoad:TStringList;
  Return:string='';
begin
  AFileName := Trim(AFileName);
  if FileExists(AFileName) then
  begin
     TextLoad := TStringList.Create;
     Textload.LoadFromFile(AFileName);
     Return := Copy(TextLoad.Text,1,Length(TextLoad.Text)-2);
     TextLoad.Free;
  end;
  Result := Return;
end;

function TTempParser.InsertTemplate(ATempName, AGenName: string): string;
var
  Return, Line: string;
  AGen: TGenFileSet;
  ATemp: TTemplate;
begin
  Return := '';
  AGen := TGenFileSet.Create;
  AGen.Add(AGenName);
  ATemp := TTemplate.Create(ATempName);
  ATemp.ParseTemplate(AGen);
  for Line in ATemp.ParsedLines do
  begin
    if ATemp.ParsedLines.IndexOf(Line) = 0 then
      Return := Return + Line + sLineBreak
    else
      Return := Return + RepeatStr(' ', FTokenPos) + Line + sLineBreak;
  end;
  ATemp.Free;
  AGen.Free;
  Return := Copy(Return, 1, Length(Return) - 2);
  Result := Return;
end;

function TTempParser.InsertTemplate(ATempName: string): string;
var
  Return, Line: string;
  AGen: TGenFileSet;
  ATemp: TTemplate;
begin
  Return := '';
  AGen := TGenFileSet.Create;
  ATemp := TTemplate.Create(ATempName);
  ATemp.ParseTemplate(AGen);
  for Line in ATemp.ParsedLines do
  begin
    if ATemp.ParsedLines.IndexOf(Line) = 0 then
      Return := Return + Line + sLineBreak
    else
      Return := Return + RepeatStr(' ', FTokenPos) + Line + sLineBreak;
  end;
  ATemp.Free;
  AGen.Free;
  Return := Copy(Return, 1, Length(Return) - 2);
  Result := Return;
end;

function TTempParser.ParseToken(AToken: string): string;
var
  GenVar: TParseResult;
  Return, GenDef, GenKey, TokenLiteral, ImportName, aaa: string;
  IsANumber, IsFromGen, IsAFunction, IsAVari, FirstIsFromGen, IsLiteral,
  IsTime, IsImportedVal, IsFromAGenSet, IsAlias, IsAReserved: boolean;
  Params: TStringList;
  i, DotPos: integer;
begin
  if IsFromParent(AToken) then
    AToken := ReplaceStr(AToken, 'PARENT.@', '@');
  IsAReserved := IsReserved(AToken);
  IsAlias := IsAnAlias(AToken);
  IsTime := IsTimeStr(AToken);
  IsAFunction := IsFunction(AToken);
  IsAVari := IsVari(AToken);
  IsLiteral := IsLiteralString(AToken);
  IsANumber := IsNumber(AToken);
  IsFromAGenSet := IsFromGenSet(AToken);
  IsImportedVal := IsImported(AToken) and (not IsLiteral);
  IsFromGen := (not IsAFunction) and (not IsAVari) and (not IsLiteral) and
    (not IsTime) and (not IsANumber) and (not IsImportedVal) and
    (not IsAlias) and (not IsFromAGenSet) and (not IsAReserved);
  if IsFromGen then
  begin
    AToken := Trim(AToken);
    if Pos(PARAM_SEP, AToken) = 0 then
    begin
      GenVar := FTemplate.GenFileSet.GetValue(AToken);
      aaa := '';
    end
    else
    begin
      GenDef := Copy(AToken, Pos(PARAM_SEP, AToken) + 1, Length(AToken));
      GenKey := Copy(AToken, 1, Pos(PARAM_SEP, AToken) - 1);
      IsAFunction := IsFunction(GenDef);
      if IsAFunction then
        GenVar := FTemplate.GenFileSet.GetValue(GenKey, ParseToken(GenDef))
      else
        GenVar := FTemplate.GenFileSet.GetValue(GenKey, GetLiteral(GenDef));
    end;
    Return := GenVar.Value;

  end
  else if IsFromAGenSet then
  begin
    if Pos(PARAM_SEP, AToken) = 0 then
    begin
      GenVar := FTemplate.GenFileSet.GetValue(AToken);
      aaa := '';
    end
    else
    begin
      GenDef := Copy(AToken, Pos(PARAM_SEP, AToken) + 1, Length(AToken));
      GenKey := Copy(AToken, 1, Pos(PARAM_SEP, AToken) - 1);
      IsAFunction := IsFunction(GenDef);
      if IsAFunction then
        GenVar := FTemplate.GenFileSet.GetValue(GenKey{, ParseToken(GenDef)})
      else
        GenVar := FTemplate.GenFileSet.GetValue(GenKey{, GetLiteral(GenDef)});
    end;
    Return := GenVar.Value;
  end
  else if IsAlias then
  begin
    Return := AToken;
  end
  else if IsImportedVal then
  begin
    AToken := Trim(AToken);
    DotPos := Pos(ATTR_ACCESSOR, AToken);
    ImportName := Copy(AToken, 1, DotPos - 1);
    GenKey := Copy(AToken, DotPos + 1, Length(AToken));
    Return := FTemplate.GetImportedValue(ImportName, ParseToken(GenKey));
  end
  else if IsAVari then
    Return := GetLiteral(AToken)
  else if IsAFunction then
  begin
    GenKey := Trim(Copy(AToken, 1, Pos(PARAM_OPEN, AToken) - 1));
    GenDef := Copy(AToken, Pos(PARAM_OPEN, AToken) + 1,
      (RPos(PARAM_CLOSE, AToken) - Pos(PARAM_OPEN, AToken))-1);
    Params := TStringList.Create;
    if Length(GenDef) > 0 then
    begin
      ParseParams(GenDef, Params);
      for i := 0 to Params.Count - 1 do
      begin
        aaa := ParseToken(Params[i]);
        Params[i] := ParseToken(Params[i]);
      end;
    end;
    Return := ParseFunction(GenKey, Params);
    Params.Free;
  end
  else if IsLiteral or IsAReserved then
    Return := GetLiteral(AToken)
  else if IsTime then
    Return := GetTimeStr(AToken)
  else if IsANumber then
    Return := AToken;
  Result := Return;
end;

function TTempParser.ParseParams(AList: string;
  var ArgsAsList: TStringList): TTempParser;
var
  StrOpen, Escaping: boolean;
  FuncLevel, i: integer;
  Part, z: string;
begin
  FuncLevel := 0;
  StrOpen := False;
  Part := '';
  ArgsAsList.Clear;
  AList := Alist + PARAM_SEP;
  Escaping := False;
  for i := 1 to Length(AList) do
  begin
    z := AList[i];
    if (AList[i] = ESCAPER) and (not Escaping) and (not StrOpen) then
    begin
      Escaping := True;
    end;
    if not Escaping then
    begin
      if not StrOpen then
      begin
        if AList[i] = PARAM_OPEN then
          Inc(FuncLevel);
        if AList[i] = PARAM_CLOSE then
          Dec(FuncLevel);
      end;
      if (AList[i] = STR_ENCLOSE) then
      begin
        StrOpen := not StrOpen;
      end;
      if (AList[i] = PARAM_SEP) then
      begin
        if (FuncLevel = 0) and (not StrOpen) then
        begin
          ArgsAsList.Add(Trim(Part));
          Part := '';
          Continue;
        end;
      end;
      Part := Part + AList[i];
    end
    else
    begin
      Part := Part + AList[i];
      Escaping := False;
    end;
  end;
  Result := Self;
end;

function TTempParser.ParseFunction(AFuncName: string; var Params: TStringList): string;
var
  Return, a, b: string;
  ExtName, ExtPath: string;
  i: integer;
begin
  AFuncName := Trim(AFuncName);
  //if exists a default
  //the value must be inserted at position
  if (Pos(ATTR_ACCESSOR, AFuncName) = 0) and (Pos(EXT_FUNC_SEP, AFuncName) = 0) then
  begin
    { Template attributes }
    if (AFuncName = 'templateName') and (Params.Count = 0) then
      Return := FTemplate.Name
    else if (AFuncName = 'templatePath') and (Params.Count = 0) then
      Return := FTemplate.FullName
    else if (AFuncName = 'exportLocation') and (Params.Count = 0) then
      Return := FTemplate.ExportLocation
    else if (AFuncName = 'levelsUp') and (Params.Count = 2) then
      Return := FileHandlingUtils.GetFilePath(Params[0],
        StrToInt(Params[1]) * (-1))
    else if (AFuncName = 'genPath') and (Params.Count = 1) then
    begin
      try
        i := StrToInt(Params[0]);
      except
        i := FTemplate.GenFileSet.IndexOf(Params[0]);
      end;

      Return := FTemplate.GenFileSet.GenFiles[i].GenFile.FullName;
    end
    else if (AFuncName = 'genPath') and (Params.Count = 2) then
    begin
      try
        i := StrToInt(Params[0]);
      except
        i := FTemplate.GenFileSet.IndexOf(Params[0]);
      end;

      Return := GetFilePath(FTemplate.GenFileSet.GenFiles[i].GenFile.FullName,(StrToInt(Params[1])*(-1)));
    end
    else if (AFuncName = 'genRelativePath') and (Params.Count = 2) then
    begin
      try
        i := StrToInt(Params[0]);
      except
        i := FTemplate.GenFileSet.IndexOf(Params[0]);
      end;

      Return := GetFileRelative(FTemplate.GenFileSet.GenFiles[i].GenFile.FullName,(StrToInt(Params[1])*(-1)));
    end
    else if (AFuncName = 'fileName') and (Params.Count = 1) then
      Return := GetFileName(Params[0])
    else if (AFuncName = 'fileName') and (Params.Count = 2) then
      Return := GetFileName(Params[0],StrToBoolean(Params[1]))


    { Interaction manipulations }
    else if (AFuncName = 'insert') and (Params.Count = 2) then
      Return := InsertTemplate(Params[0], Params[1])
    else if (AFuncName = 'insert') and (Params.Count = 1) then
    begin
      a := Params[0];
      Return := InsertTemplate(Params[0]);
    end
    else if (AFuncName = 'section') and (Params.Count = 1) then
      Return := PrintSection(Params[0])
    else if (AFuncName = 'text') and (Params.Count = 1) then
      Return := PrintPlainText(Params[0])
    else if (AFuncName = 'file') and (Params.Count = 1) then
      Return := FileHandlingUtils.PrintFileIfExists(Params[0],'',Params[0])
    else if (AFuncName = 'file') and (Params.Count = 2) then
      Return := FileHandlingUtils.PrintFileIfExists(Params[0],Params[1],Params[0])
    else if (AFuncName = 'file') and (PArams.Count = 3) then
      Return := FileHandlingUtils.PrintFileIfExists(Params[0],Params[1],Params[2])

    { Booleans Functions }
    else if (AFuncName = 'booleanToInt') and (Params.Count = 1) then
      Return := BooleansFunctions.BooleanToInt(Params[0])
    else if (AFuncName = 'intToBoolean') and (Params.Count = 1) then
      Return := BooleansFunctions.IntToBoolean(Params[0])
    else if (AFuncName = 'booleanToStr') and (Params.Count = 1) then
      Return := BooleansFunctions.IntToBoolean(Params[0])
    else if (AFuncName = 'booleanToStr') and (Params.Count = 2) then
      Return := BooleansFunctions.BooleanToStr(Params[0], Params[1])
    else if (AFuncName = 'booleanToStr') and (Params.Count = 3) then
      Return := BooleansFunctions.BooleanToStr(Params[0], Params[1], Params[2])

    { DateTime Functions }
    else if (AFuncName = 'date') and (Params.Count = 1) then
      Return := DateTimeFunctions.PrintDate(Params[0],
        FormatDateTime(DATE_INTERCHANGE_FORMAT, Now))
    else if (AFuncName = 'date') and (Params.Count = 2) then
      Return := DateTimeFunctions.PrintDate(Params[0], GetTimeStr(Params[1]))
    else if (AFuncName = 'dateMs') and (Params.Count = 0) then
      Return := DateTimeFunctions.PrintDateMs(
        FormatDateTime(DATE_INTERCHANGE_FORMAT, Now))
    else if (AFuncName = 'dateMs') and (Params.Count = 1) then
      Return := DateTimeFunctions.PrintDateMs(GetTimeStr(Params[0]))

    { ListFunctions }
    else if (AFuncName = 'count') and (Params.Count = 1) then
      Return := ListFunctions.PrintCount(Params[0], PARAM_SEP)
    else if (AFuncName = 'count') and (Params.Count = 2) then
      Return := ListFunctions.PrintCount(Params[0], Params[1])
    else if (AFuncName = 'range') and (Params.Count = 1) then
      Return := ListFunctions.PrintRange(StrToInt(Params[0]))
    else if (AFuncName = 'index') and (Params.Count = 2) then
      Return := FTemplate.GetVariable(Params[0] + '[' + Params[1] + ']')

    { Simple REGEX Functions }
    else if (AFuncName = 'match') and (Params.Count = 3) then
      Return := FTemplate.GetWild(Params[0], Params[1], Params[2])
    else if (AFuncName = 'match') and (Params.Count = 2) then
    begin
      a := TGenFile(FTemplate.Imported.Objects[FTemplate.Imported.IndexOf(Params[1])]).IfNotFound;
      Return := FTemplate.GetWild(Params[0], Params[1], a);
    end

    { String Manipulation }
    else if (AFuncName = 'concat') then
      Return := StringsFunctions.Concat(Params)
    else if (AFuncName = 'lower') and (Params.Count = 1) then
      Return := AnsiLowerCase(Params[0])
    else if (AFuncName = 'upper') and (Params.Count = 1) then
      Return := AnsiUpperCase(Params[0])
    else if (AFuncName = 'slice') and (Params.Count = 3) then
      Return := StringsFunctions.Slice(Params[0], StrToInt(Params[1]),
        StrToInt(Params[2]))
    else if (AFuncName = 'substring') and (Params.Count = 2) then
      Return := Copy(Params[0], StrToInt(Params[1]) + 1, Length(Params[0]))
    else if (AFuncName = 'substring') and (Params.Count = 3) then
      Return := Copy(Params[0], StrToInt(Params[1]) + 1, StrToInt(Params[2]))
    else if (AFuncName = 'replace') and (Params.Count > 2) then
      Return := StringsFunctions.ReplaceStrExtended(Params)
    else if (AFuncName = 'repeat') and (Params.Count = 2) then
      Return := StringsFunctions.RepeatStr(Params[0], StrToInt(Params[1]))
    else if (AFuncName = 'leftZeros') and (Params.Count = 2) then
      Return := StringsFunctions.LeftZeros(Params[0], StrToInt(Params[1]))
    else if (AFuncName = 'reverse') and (Params.Count = 1) then
      Return := ReverseString(Params[0])
    else if (AFuncName = 'md5') and (Params.Count = 1) then
      Return := StringsFunctions.StrToMD5(Params[0])
    else if (AFuncName = 'superHash') and (Params.Count = 1) then
      Return := StringsFunctions.SuperHash(Params[0])
    else if (AFuncName = 'indexOf') and (Params.Count = 2) then
      Return := IntToStr(Pos(Params[0],Params[1]))
    else if ExternalExists(AFuncName) then
      Return := CallExternal(PROCESSORS_FOLDER + DirectorySeparator + AFuncName, Params)
    else
      Return := '';
  end
  else
  begin
    if ExternalExists(EXTENSIONS_FOLDER + DirectorySeparator + AFuncName) or
      ExternalExists(PROCESSORS_FOLDER + DirectorySeparator + AFuncName) then
    begin
      if Pos(ATTR_ACCESSOR, AFuncName) > 0 then
      begin
        ExtPath := Copy(AFuncName, 1, Pos(ATTR_ACCESSOR, AFuncName) - 1);
        ExtName := Copy(AFuncName, Pos(ATTR_ACCESSOR, AFuncName) + 1, Length(AFuncName));
        //Call a function from an extension
        Return := CallExternal(EXTENSIONS_FOLDER + DirectorySeparator +
          ExtPath, ExtName, Params);
      end
      else
        Return := CallExternal(PROCESSORS_FOLDER + DirectorySeparator +
          AFuncName, Params);
    end;
  end;
  Result := Return;
end;

function TTempParser.ExternalExists(AFuncPath: string): boolean;
begin
  if ExternalPath(AFuncPath) <> '' then
    Result := True
  else
    Result := False;
end;

function TTempParser.ExternalPath(AFuncPath: string): string;
var
  Exts, Return: string;
begin
  AFuncPath := ReplaceStr(AFuncPath, EXT_FUNC_SEP, DirectorySeparator);
  if Pos(ATTR_ACCESSOR, AFuncPath) > 0 then
    AFuncPath := Copy(AFuncPath, 1, Pos(ATTR_ACCESSOR, AFuncPath) - 1);
  Return := '';
  for Exts in EXT_ACCEPTED do
  begin
    if FileExists(AFuncPath + Exts) then
    begin
      Return := AFuncPath + Exts;
      Break;
    end;
  end;
  Result := Return;
end;

function TTempParser.CallExternal(ExtPath, FuncName: string;
  var Params: TStringList): string;
begin
  Params.Insert(0, FuncName);
  Result := CallExternal(ExtPath, Params);
end;

function TTempParser.CallExternal(ExtPath: string; var Params: TStringList): string;
const
  BUFFER_SIZE = 2048;
var
  FuncPath, P, Return: string;
  AProcess: TProcess;
  OutStream: TStream;
  Buf: array[1..BUFFER_SIZE] of byte;
  BytesRead: longint;
begin
  FuncPath := ExternalPath(ExtPath);
  AProcess := TProcess.Create(nil);
  AProcess.Executable := FuncPath;
  AProcess.Parameters.AddStrings(Params);
  AProcess.Options := [poUsePipes];
  AProcess.Execute;
  OutStream := TMemoryStream.Create;
  repeat
    BytesRead := AProcess.Output.Read(Buf, BUFFER_SIZE);
    OutStream.Write(Buf, BytesRead);
  until BytesRead = 0;
  AProcess.Free;
  with TStringList.Create do
  begin
    OutStream.Position := 0;
    LoadFromStream(OutStream);
    Return := Text;
    Free;
  end;
  Result := Copy(Return, 1, Length(Return) - 2);
end;

function TTempParser.ParseTemplate(var OutputParsedLines: TStringList): TTempParser;
var
  Key, Value, Line, LineTrim, Temp: string;
  PosAssoc, PosVarAssoc: integer;
  Unary: boolean;
  AOpt, AOver, ATarget, AKey: string;
  PosAs, i, Times: integer;
  Params:TStringList;
begin
  for Line in FTemplate.TempLines do
  begin
    LineTrim := Trim(Line);
    PosAs := Pos(OVER_ASSOC, LineTrim);
    if (PosAs > 0) then
    begin
      AOpt := Copy(LineTrim, 1, PosAs);
      if AOpt = (OVER_STATE + 'filter' + OVER_ASSOC) then
      begin
        FTemplate.Filters.Add(Copy(LineTrim, Length(AOpt) + 1, Length(LineTrim)));
      end
      else if AOpt = (OVER_STATE + 'bypass' + OVER_ASSOC) then
      begin
        FTemplate.Bypasses.Add(Copy(LineTrim, Length(AOpt) + 1, Length(LineTrim)));
      end;
    end;
  end;
  if FTemplate.EvalFilter and (not FTemplate.EvalBypass) then
  begin
    FTemplate.CanSave := True;
    //for Line in FTemplate.TempLines do
    i := 0;
    while (i < FTemplate.TempLines.Count) do
    begin
      if Trim(FTemplate.TempLines[i]) = OVER_STATE + 'else' then
        FTemplate.Skip := not FTemplate.Skip
      else if Trim(FTemplate.TempLines[i]) = OVER_STATE + 'endif' then
        FTemplate.Skip := False;
      if FTemplate.Skip then
      begin
        i := i + 1;
        Continue;
      end;
      if FTemplate.Rewind then
      begin
        //i := FTemplate.ForGoto - 1;
        i := FTemplate.ForLoops[FTemplate.ForLevel].GoToLine - 1;
        FTemplate.Rewind := False;
        Continue;
      end;
      FTemplate.LineNumber := i;
      LineTrim := Trim(FTemplate.TempLines[i]);
      if Length(LineTrim) > 0 then
      begin
        if (Copy(LineTrim, 1, Length(COMMENT_TOKEN)) = COMMENT_TOKEN) or
           (LineTrim = RepeatStr(COMMENT_TOKEN,3)) or (FTemplate.CommentBlock) then
        begin
          if LineTrim = RepeatStr(COMMENT_TOKEN,3) then
            FTemplate.CommentBlock := not FTemplate.CommentBlock;
          i := i + 1;
          continue;
        end;
        if Copy(LineTrim, 1, Length(OVER_STATE)) = OVER_STATE then
        begin
          PosAssoc := Pos(OVER_ASSOC, LineTrim);
          PosVarAssoc := Pos(VAR_ASSOC, LineTrim);
          if PosAssoc > 0 then
          begin
            Key := Copy(LineTrim, Length(OVER_STATE) + 1, PosAssoc -
              Length(OVER_ASSOC) - 1);
            Value := Copy(FTemplate.TempLines[i], Pos(
              OVER_ASSOC, FTemplate.TempLines[i]) + Length(OVER_ASSOC),
              Length(FTemplate.TempLines[i]));
            FTemplate.SetPredefined(Key, Value);
            //Value := ParseLine(Value).Value;
            Unary := False;
          end
          else if PosVarAssoc > 0 then
          begin
            Key := Copy(LineTrim, Length(OVER_STATE) + 1, PosVarAssoc -
              Length(OVER_ASSOC) - 1);
            Value := Copy(FTemplate.TempLines[i], Pos(
              VAR_ASSOC, FTemplate.TempLines[i]) + Length(VAR_ASSOC),
              Length(FTemplate.TempLines[i]));
            FTemplate.SetVariable(Key, Value,True);
          end
          else
          begin
            Key := Copy(LineTrim, Length(OVER_STATE) + 1,
              Length(FTemplate.TempLines[i]));
            Value := '';
            Unary := True;
            FTemplate.SetPredefined(Key, Value);
          end;

        end
        else
          OutputParsedLines.Add(ParseLine(FTemplate.TempLines[i]).Value);
      end
      else
      begin
        if (FTemplate.RenderBlank) then
        begin
          OutputParsedLines.Add(ParseLine(FTemplate.TempLines[i]).Value);
        end;
      end;
      if not FTemplate.Rewind then
        i := i + 1;
    end;
  end
  else
    FTemplate.CanSave := False;
  Result := Self;
end;

end.
