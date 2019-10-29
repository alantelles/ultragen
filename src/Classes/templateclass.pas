unit TemplateClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DateUtils, StrUtils, Process,

  { Globals }
  TypesGlobals, VariablesGlobals, ConstantsGlobals,

  { Utils }
  BooleansFunctions,

  { Classes }
  GenFileClass, GenFileSetClass;

type
  TOverrides = record
    OutFileName: string;
    Extension: string;
    CopyTo: TStringList;
    Overwrite: boolean;
    Filters: TStringList;
    Bypasses: TStringList;
    ExpAtRoot: boolean;
    RenderBlank: boolean;
  end;

  TDefaultParam = record
    FuncName:string;
    ParamPos:integer;
    DefValue:string;
  end;

  TDefaultParamArray = array of TDefaultParam;

  TForLevel = record
    Times: integer;
    GoToLine: integer;
    ControlVar: string;
    List: TStringList;
    PauseTime:integer;
    Infinite:boolean;
    Limit:integer;
  end;
  TForRecursion = array of TForLevel;

type
  TTemplate = class
  private
    FScriptMode, FCommentBlock: boolean;
    FFullName, FOutFilePath: string;
    FTokenOpen, FTokenClose: string;
    FVariables: array of TKVPair;
    FImported, FSections: TStringList;
    FLines, FParsed: TStringList;
    FGenFile: TGenFile;
    FGenFileSet: TGenFileSet;
    FOverrides: TOverrides;
    FExpLocation: string;
    FGenTime: TDateTime;
    FGenImport: TGenFile;
    FCanSave: boolean;
    FLineNumber, FForGoto, FForTimes: integer;
    FForLevel: integer;
    FForLoops: TForRecursion;
    FRewind, FSkip: boolean;
  public
    constructor Create(ATempName: string = ''; AExpLocation: string = '.');
    property RenderBlank: boolean read FOverrides.RenderBlank
      write FOverrides.RenderBlank;
    property ForLoops: TForRecursion read FForloops write FForLoops;
    property TokenOpen: string read FTokenOpen write FTokenOpen;
    property TokenClose: string read FTokenClose write FTokenClose;
    property LineNumber: integer read FLineNumber write FLineNumber;
    property ForGoto: integer read FForGoto write FForGoto;
    property ForTimes: integer read FForTimes write FForTimes;
    property Rewind: boolean read FRewind write FRewind;
    property Skip: boolean read FSkip write FSkip;
    property CanSave: boolean read FCanSave write FCanSave;
    property FullName: string read FFullName;
    property GenFile: TGenFile read FGenFile;
    property GenFileSet: TGenFileSet read FGenFileSet;
    property TempLines: TStringList read FLines write FLines;
    property ParsedLines: TStringList read FParsed write FParsed;
    property GenTime: TDateTime read FGenTime;
    property ExportLocation: string read FExpLocation write FExpLocation;
    property OutFilePath: string read FOutFilePath;
    property ScriptMode:boolean read FScriptMode write FScriptMode;
    property CommentBlock:boolean read FCommentBlock write FCommentBlock;
    property Imported: TStringList read FImported write FImported;
    property Filters: TStringList read FOverrides.Filters write FOverrides.Filters;
    property Bypasses: TStringList read FOverrides.Bypasses write FOverrides.Bypasses;
    property Variables: TDict read FVariables write FVariables;
    property Sections: TStringList read FSections write FSections;
    property ForLevel: integer read FForLevel write FForLevel;
    function Name: string;
    function SetPredefined(AKey, AValue: string): boolean;
    function Load(ATempName: string): TTemplate;
    function Load(ATempList: TStringList; ATempName: string): TTemplate;
    procedure LoadText(var Params:TStringList);
    function Save: TTemplate;
    procedure ProcessTemplate(var Params:TStringList);
    function GetVariable(AVarName: string): string;
    function SetVariable(AKey, AValue: string; Parse:boolean=False): TTemplate;
    function DropVariable(AKey: string): TTemplate;
    function GetImportedValue(AnAlias, AKey: string): string;
    function ImportGenFile(var Params:TStringList): TTemplate;
    procedure ExtendTemplate(ATemplate: string; Parent: string = '');
    procedure IncludeTemplate(var Params:TStringList);
    procedure Execute(var Params:TStringList);
    function ParseTemplate(var AGen: TGenFileSet): TTemplate;
    function ParseTemplate(var AGen: TGenFileSet; var OutputParsed: TStringList): TTemplate;
    function ParseTemplate(var AGen: TGenFile): TTemplate;
    function ParseTemplate(var AGen: TGenFile; var OutputParsed: TStringList): TTemplate;
    function GetWild(ASearch, AnAlias, ADefault: string): string;
    procedure Print;
    procedure PrintLine(var Params:TStringList; Tee:boolean=False);
    procedure PrintParsed;
    procedure ForPrepare(var Params:TstringList; ForLoop:boolean = True);
    procedure EndFor;
    procedure LoopPrepare(var Params:TstringList; ForLoop:boolean = True);
    procedure EndLoop;
    function EvalFilter: boolean;
    function EvalBypass: boolean;
    procedure ExplodeStr(var Params:TStringList);
    procedure Clear;
    procedure IfPrepare(var Params:TStringList; IfNot: boolean);
    procedure ElseDecision;
    procedure EndIf;
    procedure ListFiles(var Params:TStringList);
    procedure Move(var Params:TStringList);
    procedure TempFileCopy(var Params:TStringList);
    destructor Destroy; override;
  end;

implementation

uses FileHandlingUtils,
  ParserClass,
  StringsFunctions;

constructor TTemplate.Create(ATempName: string = ''; AExpLocation: string = '.');
begin
  FOverrides.Overwrite := False;
  FOverrides.CopyTo := TStringList.Create;
  FOverrides.Filters := TStringList.Create;
  FOverrides.Filters.NameValueSeparator := OVER_PARAM;
  FOverrides.Bypasses := TStringList.Create;
  FOverrides.Bypasses.NameValueSeparator := OVER_PARAM;
  FCommentBlock := False;
  FScriptMode := False;

  if (Length(AExpLocation) > 0) and (AExpLocation[Length(AExpLocation)] =
    DirectorySeparator) then
    AExpLocation := Copy(AExpLocation, 1, Length(AExpLocation) - 1)
  else if Length(AExpLocation) = 0 then
    AExpLocation := '.';
  FExpLocation := AExpLocation;

  FFullName := '';
  FTokenOpen := TOKEN_OPEN;
  FTOkenClose := TOKEN_CLOSE;

  FImported := TStringList.Create;
  FSections := TStringList.Create;
  FLines := TStringList.Create;
  FParsed := TStringList.Create;
  if ATempName <> '' then
  begin
    FFullName := ExpandFileName(ATempName);
    Load(FFullName);
  end;
end;
procedure TTemplate.ProcessTemplate(var Params:TStringList);
var
  ATemplate:TTemplate;
  AGenSet:TGenFileSet;
  TempPath:string;
  i:integer;
begin
  TempPath := Params[0];
  ATemplate := TTemplate.Create(TempPath);
  AGenSet := TGenFileSet.Create;
  if Params[1] <> '' then
    AGenSet.Add(Params[1]);
  if Params.Count > 2 then
  begin
    for i:=2 to Params.Count-1 do
      ATemplate.SetVariable('param['+IntToStr(i-2)+']',Params[i]);
  end;
  ATemplate.ParseTemplate(AGenSet);
  ATemplate.Save;
  ATemplate.Free;
end;

procedure TTemplate.Move(var Params:TStringList);
var
  ANewName:string;
begin
  if FileExists(Params[0]) then
  begin
    ANewName := GetFileName(Params[0]);
    CreateDirTree(Params[1]);
    if Params.Count = 3 then
      ANewName := Params[2];
    CreateDirTree(Params[1]+DirectorySeparator+ANewName);
    RenameFile(Params[0],RemoveLastBackslash(Params[1])+DirectorySeparator+ANewName);
  end;
end;
procedure TTemplate.TempFileCopy(var Params:TStringList);
var
  ANewName:string;
begin
  if FileExists(Params[0]) then
  begin
    ANewName := GetFileName(Params[0]);
    CreateDirTree(Params[1]);
    if Params.Count = 3 then
      ANewName := Params[2];
    CreateDirTree(Params[1]+DirectorySeparator+ANewName);
    CopyFile(Params[0],RemoveLastBackslash(Params[1])+DirectorySeparator+ANewName);
  end;
end;

procedure TTemplate.Execute(var Params:TStringList);
var
  AProcess:TProcess;
  i:integer;
begin
  AProcess := TProcess.Create(nil);
  if not FileExists(Params[0]) then
  begin
    WriteLn('Process not found');
    Exit;
  end;
  AProcess.Executable := Params[0];
  AProcess.Options := AProcess.Options+[poWaitOnExit];
  if Params.Count > 1 then
  begin
    for i:=1 to Params.Count-1 do
      AProcess.Parameters.Add(Params[i]);
  end;
  AProcess.Execute;
end;

procedure TTemplate.PrintLine(var Params:TStringList; Tee:boolean=False);
var
  Return:string='';
  P:string;
begin
  for P in Params do
    Return := Return + P;
  if Tee then
    FParsed.Add(Return);
  WriteLn(Return);
end;

procedure TTemplate.IncludeTemplate(var Params:TStringList);
var
  ATemp: TTemplate;
  APair: TKVPair;
  IncName, TempAlias, ALine: string;
  i: integer;
  Dump: TGenFile;
begin
  IncName := Params[0];
  if (Params.Count > 1) and (Params[1] <> '') then
    TempAlias := Params[1]
  else
    TempAlias := GetFileName(GetFileName(IncName, False), False);
  ATemp := TTemplate.Create(IncName, FExpLocation);
  ATemp.ParseTemplate(FGenFileSet);
  for ALine in ATemp.ParsedLines do
    FParsed.Add(ALine);
  for APair in ATemp.Variables do
    SetVariable(TempAlias + ATTR_ACCESSOR + APair.Key, APair.Value);
  ATemp.Free;
end;

function TTemplate.Name: string;
begin
  Result := GetFileName(GetFileName(FFullName, False), False);
end;

procedure TTemplate.ExplodeStr(var Params:TStringList);
var
  Explode: TStringList;
  i: integer;
begin
  Explode := TStringList.Create;
  Explode.StrictDelimiter := True;
  if Params.Count > 2 then
    Explode.Delimiter := Params[2][1]
  else
    Explode.Delimiter := PARAM_SEP;
  Explode.DelimitedText := Params[0];
  if Params.Count = 4 then
  begin
    if Params[3] = ASC then
      Explode.Sort
    else if Params[3] = DESC then
    begin
      Explode.Sort;
      ReverseList(Explode);
    end;
  end;
  if Explode.Count > 0 then
  begin
    for i := 0 to Explode.Count - 1 do
      SetVariable(Params[1] + '[' + IntToStr(i) + ']', Explode[i]);
  end;
  Explode.Free;
end;

procedure TTemplate.ListFiles(var Params:TStringList);
var
  APath: string;
  AVarName: string;
  AFilter: string;
  LookSub: boolean;
  ADelimiter: string;
  Files: TStringList;
  AParser: TTempParser;
  Sort: boolean = False;
  i:integer;
begin
  AFilter := '';
  LookSub := False;
  //ADelimiter := FILES_SECURE_SEP;

  APath := Params[0];
  AVarName := Params[1];
  if Params.Count > 2 then
    AFilter := Params[2];
  if Params.Count > 3 then
    LookSub := StrToBoolean(Params[3]);
  Files := TStringList.Create;
  FindAllFiles(Files, APath, AFilter, LookSub);
  if Params.Count > 4 then
  begin
    if Params[4] = ASC then
      Files.Sort
    else if Params[4] = DESC then
    begin
      Files.Sort;
      ReverseList(Files);
    end;
  end;
  SetVariable(AVarName, Files.Text);
  if Files.Count > 0 then
  begin
    for i:=0 to Files.Count - 1 do
      SetVariable(AVarName+'['+IntToStr(i)+']',Files[i]);
  end;
end;

procedure TTemplate.LoadText(var Params:TStringList);
var
  AText:TStringList;
  i:integer;
begin
  AText := TStringList.Create;
  if FileExists(Params[0]) then
  begin
    AText.LoadFromFile(Params[0]);
    if AText.Count > 0 then
    begin
      SetVariable(Params[1],AText.Text);
      for i:=0 to AText.Count-1 do
        SetVariable(Params[1]+'['+IntToStr(i)+']',AText[i]);
    end;
  end;
  AText.Free;
end;

procedure TTemplate.ExtendTemplate(ATemplate: string; Parent: string = '');
var
  Temp, Vars, Params: TStringList;
  Line, Part, TempAlias, IncName, VarName, AVar: string;
  IsPredef: boolean;
  AParser: TTempParser;
begin
  Params := TStringList.Create;
  AParser := TTempParser.Create(Self);
  AParser.ParseParams(ATemplate, Params);
  IncName := Params[0];
  if (Params.Count > 1) and (Params[1] <> '') then
    TempAlias := Params[1]
  else
    TempAlias := GetFileName(GetFileName(IncName, False), False);
  Temp := TStringList.Create;
  Temp.LoadFromFile(IncName);
  Vars := TStringList.Create;
  for Line in Temp do
  begin
    if Pos(OVER_STATE + 'extend' + OVER_ASSOC, Trim(Line)) = 1 then
    begin
      Part := Copy(Line, Pos(OVER_ASSOC, Line) + Length(OVER_ASSOC), Length(Line));
      ExtendTemplate(Part, TempAlias);
    end
    else
    begin
      Part := Line;
      if (Pos(OVER_STATE, Trim(Line)) = 1) then
      begin
        VarName := Copy(Trim(Line), Pos(OVER_STATE, Line) + Length(OVER_STATE),
          Length(Trim(Line)) - Length(OVER_ASSOC));
        if (Pos(OVER_ASSOC, Line) > 0) then
          VarName := Copy(VarName, 1, Pos(OVER_ASSOC, Line) - Length(OVER_ASSOC) - 1);
        if Parent <> '' then
          Parent := Parent + ATTR_ACCESSOR;
        for IncName in PREDEF_OVERR do
          if VarName = IncName then
          begin
            IsPredef := True;
            Continue;
          end;
        if not IsPredef then
        begin
          Vars.Add(VarName);
          Part := Copy(Line, Pos(OVER_STATE, Line) + Length(OVER_STATE), Length(Line));
          Part := OVER_STATE + Parent + TempAlias + ATTR_ACCESSOR + Part;
        end;
      end
      else
      begin
        for AVar in Vars do
        begin
          Part := ReplaceStr(Part, OVER_STATE + AVar, OVER_STATE + Parent +
            TempAlias + ATTR_ACCESSOR + AVar);
        end;
      end;
      FLines.Add(Part);
    end;
  end;
  Vars.Free;
  Temp.Free;
end;


procedure TTemplate.ForPrepare(var Params:TStringList;ForLoop:boolean = True);
var
  Values: TStringList;
  ParamAsStr, Iterated: string;
  len, i: integer;
begin
  FForLevel := FForLevel + 1;
  SetLength(FForLoops, FForLevel+1);
  FForLoops[FForLevel].ControlVar := Params[1];
  FForLoops[FForLevel].GoToLine := FLineNumber + 2;

  FForLoops[FForLevel].List := TStringList.Create;
  if Params.Count < 3 then
  begin
    FForLoops[FForLevel].List.Delimiter := PARAM_SEP;
    FForLoops[FForLevel].List.StrictDelimiter := True;
    FForLoops[FForLevel].List.DelimitedText := Params[0]
  end
  else
  begin
    if Params[2] <> LINE_BREAK then
    begin
      FForLoops[FForLevel].List.Delimiter := Params[2][1];
      FForLoops[FForLevel].List.StrictDelimiter := True;
      Iterated := Params[0];
      FForLoops[FForLevel].List.DelimitedText := Iterated
    end
    else
    begin
      FForLoops[FForLevel].List.Text := Params[0]
    end;
  end;

  if Params.Count = 4 then
  begin
    if Params[3] = ASC then
      FForLoops[FForLevel].List.Sort
    else if Params[3] = DESC then
    begin
      FForLoops[FForLevel].List.Sort;
      ReverseList(FForLoops[FForLevel].List);
    end;
  end;
  FForLoops[FForLevel].Times := FForLoops[FForLevel].List.Count;
  if FForLoops[FForLevel].List.Count > 0 then
  begin
    SetVariable(FForLoops[FForLevel].ControlVar,
      FForLoops[FForLevel].List[FForLoops[FForLevel].List.Count - FForLoops[FForLevel].Times]);
    SetVariable(FForLoops[FForLevel].ControlVar + '.i', IntToStr(
      FForLoops[FForLevel].List.Count - FForLoops[FForLevel].Times));
  end;
  Rewind := False;
end;

procedure TTemplate.EndFor;
begin
  //FForTimes := FForTimes - 1;
  FForLoops[FForLevel].Times := FForLoops[FForLevel].Times - 1;
  FRewind := True;

  if FForLoops[FForLevel].Times <= 0 then
  begin
    DropVariable(FForLoops[FForLevel].ControlVar);
    DropVariable(FForLoops[FForLevel].ControlVar + '.i');
    FForLoops[FForLevel].List.Free;
    FRewind := False;
    FForLevel := FForLevel - 1;
    SetLength(FForLoops, FForLevel + 1);
  end
  else
  begin
    SetVariable(FForLoops[FForLevel].ControlVar,
      FForLoops[FForLevel].List[FForLoops[FForLevel].List.Count - FForLoops[FForLevel].Times]);
    SetVariable(FForLoops[FForLevel].ControlVar + '.i', IntToStr(
      FForLoops[FForLevel].List.Count - FForLoops[FForLevel].Times));
  end;
end;

procedure TTemplate.LoopPrepare(var Params:TStringList;ForLoop:boolean = True);
var
  Values: TStringList;
  ParamAsStr, Iterated: string;
  len, i,Times, Pause: integer;
begin
  //loop:[times],[control var],[pause (ms)]
  //loop:30,500,'i'
  //loop:0,'i',500 (infinite loop)
  FForLoops[FForLevel].ControlVar := '';
  Times := StrToInt(Params[0]);
  FForLevel := FForLevel + 1;
  SetLength(FForLoops, FForLevel+1);
  FForLoops[FForLevel].PauseTime := 0;
  if Times  = 0 then
    FForLoops[FForLevel].Infinite := True;
  if Params.Count = 2 then
  begin
    try
      Pause := StrToInt(Params[1]);
      FForLoops[FForLevel].PauseTime := Pause;
    except
      FForLoops[FForLevel].ControlVar := Params[1];
    end;
  end;
  if Params.Count = 3 then
  begin
    Pause := StrToInt(Params[1]);
    FForLoops[FForLevel].PauseTime := Pause;
    FForLoops[FForLevel].ControlVar := Params[2];
  end;
  FForLoops[FForLevel].GoToLine := FLineNumber + 2;
  FForLoops[FForLevel].Limit := Times;
  FForLoops[FForLevel].Times := 0;
  if FForLoops[FForLevel].ControlVar <> '' then
  begin
    SetVariable(FForLoops[FForLevel].ControlVar, IntToStr(FForLoops[FForLevel].Times));
  end;
  Rewind := False;
end;

procedure TTemplate.EndLoop;
begin
  FForLoops[FForLevel].Times := FForLoops[FForLevel].Times + 1;
  if FForLoops[FForLevel].Infinite then
    FForLoops[FForLevel].Limit := FForLoops[FForLevel].Limit+FForLoops[FForLevel].Times + 1;
  FRewind := True;
  if FForLoops[FForLevel].PauseTime > 0 then
    Sleep(FForLoops[FForLevel].PauseTime);
  if FForLoops[FForLevel].Times = (FForLoops[FForLevel].Limit) then
  begin
    DropVariable(FForLoops[FForLevel].ControlVar);
    FRewind := False;
    FForLevel := FForLevel - 1;
    SetLength(FForLoops, FForLevel + 1);
  end
  else
  begin
    if FForLoops[FForLevel].ControlVar <> '' then
      SetVariable(FForLoops[FForLevel].ControlVar , IntToStr(FForLoops[FForLevel].Times));
  end;
end;

function TTemplate.ImportGenFile(var Params:TStringList): TTemplate;
var
  GenName: string;
  DefValue: string;
  GenAlias: string;
  GenSep: string;
  i: integer;
begin
  GenName := Params[0];
  DefValue := DEF_IF_NOT;
  GenAlias := ReplaceStr(ReplaceStr(GetFileName(Params[0], False), '.', ''), '-', '');
  GenSep := GEN_SEP;
  if Params.Count > 1 then
    GenAlias := Params[1];
  if Params.Count > 2 then
    DefValue := Params[2];
  if Params.Count = 4 then
    GenSep := Params[3];
  FGenImport := TGenFile.Create;
  FGenImport.IfNotFound := DefValue;
  FGenImport.GenSeparator := GenSep;
  FGenImport.Load(GenName);
  i := FImported.IndexOf(GenAlias);
  if i = -1 then
    FImported.AddObject(GenAlias, FGenImport)
  else
    FImported.Objects[i] := FGenImport;
  Result := Self;
end;

procedure TTemplate.IfPrepare(var Params:TStringList; IfNot: boolean);
var
  a,b,c: string;
  Logic: boolean;
  d:integer;

begin
  a := Params[0];
  b := Params[1];
  if a = 'EMPTY' then
    Logic := Length(Params[1]) = 0
  else if b = 'CONTAINS' then
  begin
    c := Params[2];
    d := Pos(c,a);
    Logic := d > 0;
  end;
  if (Logic and (not IfNot)) or ((not Logic) and IfNot) then
    FSkip := False
  else
    FSkip := True;
end;

procedure TTemplate.ElseDecision;
begin
  //FSkip := not FSkip;
end;

procedure TTemplate.EndIf;
begin
  //FSkip := False;
end;

function TTemplate.Load(ATempName: string): TTemplate;
var
  Temp: TStringList;
  SectionLines: TStringList;
  Line, Part: string;
  SectionOpen: boolean = False;
begin
  Temp := TStringList.Create;
  try
    Temp.LoadFromFile(ATempName);
    FLines.Clear;
    Clear;
    FFullname := ATempName;
    FOverrides.Extension := Copy(FFullName, RPos(EXT_SEP, FFullName), Length(FFullName));
    for Line in Temp do
    begin
      if Pos(OVER_STATE + 'extend' + OVER_ASSOC, Trim(Line)) = 1 then
      begin
        Part := Copy(Line, Pos(OVER_ASSOC, Line) + Length(OVER_ASSOC), Length(Line));
        ExtendTemplate(Part);
      end
      else if Pos(OVER_STATE + 'section' + OVER_ASSOC, Trim(Line)) = 1 then
      begin
        SectionOpen := True;
        Part := Copy(Line, Pos(OVER_ASSOC, Line) + Length(OVER_ASSOC), Length(Line));
        SectionLines := TStringList.Create;
        FSections.AddObject(Part, SectionLines);
      end
      else if (Pos(OVER_STATE + 'endsection', Trim(Line)) = 1) and
        (Length(Trim(Line)) = Length(OVER_STATE + 'endsection')) then
        SectionOpen := False
      else if SectionOpen then
        TStringList(FSections.Objects[FSections.IndexOf(Part)]).Add(Line)
      else
      begin
        FLines.Add(Line);
      end;
    end;
  finally
    Temp.Free;
  end;
  Result := Self;
end;

function TTemplate.Load(ATempList: TStringList; ATempName: string): TTemplate;
var
  Line, Part, Temp: string;
  SectionLines: TStringList;
  SectionOpen: boolean = False;
begin
  try
    FLines.Clear;
    Clear;
    FFullname := ATempName;
    FOverrides.Extension := Copy(FFullName, RPos(EXT_SEP, FFullName), Length(FFullName));
    for Line in ATempList do
    begin
      if Pos(OVER_STATE + 'extend' + OVER_ASSOC, Trim(Line)) = 1 then
      begin
        Part := Copy(Line, Pos(OVER_ASSOC, Line) + Length(OVER_ASSOC), Length(Line));
        ExtendTemplate(Part);
      end
      else if Pos(OVER_STATE + 'section' + OVER_ASSOC, Trim(Line)) = 1 then
      begin
        SectionOpen := True;
        Part := Copy(Line, Pos(OVER_ASSOC, Line) + Length(OVER_ASSOC), Length(Line));
        SectionLines := TStringList.Create;
        FSections.AddObject(Part, SectionLines);
      end
      else if (Pos(OVER_STATE + 'endsection', Trim(Line)) = 1) and
        (Length(Trim(Line)) = Length(OVER_STATE + 'endsection')) then
        SectionOpen := False
      else if SectionOpen then
        TStringList(FSections.Objects[FSections.IndexOf(Part)]).Add(Line)
      else
      begin
        FLines.Add(Line);
      end;
    end;
  finally
  end;
  Result := Self;
end;

function TTemplate.Save: TTemplate;
var
  Temp, FullOutFilePath: string;
  Tries: integer;
begin
  if CanSave then
  begin
    if not FOverrides.ExpAtRoot then
      FOutFilePath :=
        FExpLocation + DirectorySeparator + Name + DirectorySeparator +
        FOverrides.OutFileName + FOverrides.Extension
    else
      FOutFilePath :=
        FExpLocation + DirectorySeparator + FOverrides.OutFileName +
        FOverrides.Extension;
    CreateDirTree(FOutFilePath);
    if FOverrides.Overwrite or (not FileExists(FOutFilePath)) then
    begin
      Tries := 0;
      repeat
        Inc(Tries);
        FParsed.SaveToFile(FOutFilePath);
      until FileExists(FOutFilePath);
    end;
    for Temp in FOverrides.CopyTo do
    begin
      Tries := 0;
      repeat
        Inc(Tries);
        FullOutFilePath := Temp + DirectorySeparator + FOverrides.OutFileName +
          FOverrides.Extension;
        CreateDirTree(FullOutFilePath);
        CopyFile(FOutFilePath, FullOutFilePath);
      until FileExists(FullOutFilePath) or (Tries > MAX_TRIES);
    end;
  end;
  Result := Self;
end;

function TTemplate.SetPredefined(AKey, AValue: string): boolean;
var
  Return: boolean = True;
  AParser:TTempParser;
  Params:TStringList;
  i:integer;
  a:string;
begin
  Params := TSTringList.Create;
  AParser := TTempParser.Create(Self);
  AParser.ParseParams(AValue, Params);
  if Params.Count > 0 then
  begin
    for i:=0 to Params.Count-1 do
    begin
      a := AParser.ParseToken(Params[i]);
      Params[i] := a;
    end;
  end;
  AKey := Trim(AKey);
  case (AKey) of
    'outFileName': FOverrides.OutFileName := Params[0];
    'copyTo': FOverrides.CopyTo.Add(RemoveLastBackslash(Params[0]));
    'exportTo': FExpLocation := Params[0];
    'extension': FOverrides.Extension := Params[0];
    'overwrite': FOverrides.Overwrite := True;
    'exportAtRoot': FOverrides.ExpAtRoot := True;
    'import': ImportGenFile(Params);
    'include': IncludeTemplate(Params);
    'for': ForPrepare(Params);
    'endFor': EndFor;
    'loop' : LoopPrepare(Params);
    'endLoop' : EndLoop;
    'if': IfPrepare(Params, False);
    'ifNot': IfPrepare(Params, True);
    'else': ElseDecision;
    'endIf': EndIf;
    'explode': ExplodeStr(Params);
    'print' : PrintLine(Params);
    'tee' : PrintLine(Params, True);
    'processTemplate' : ProcessTemplate(Params);
    'tokenEnclosers':
    begin
      FTokenOpen := Params[0][1];
      FTokenClose := Params[0][2];
    end;
    'execute':Execute(Params);
    'drop': DropVariable(Params[0]);
    'loadText' : LoadText(Params);
    'listFiles': ListFiles(Params);
    'move' : Move(Params);
    'copy' : TempFileCopy(Params);
    'renderBlank': FOverrides.RenderBlank := True;
    else
      Return := False;
  end;
  PArams.Free;
  Result := Return;
end;

procedure TTemplate.Print;
var
  Line: string;
begin
  for Line in FLines do
    WriteLn(Line);
end;

procedure TTemplate.PrintParsed;
var
  Line, S: string;
begin
  for Line in FParsed do
  begin
    S := ReplaceStr(Line, 'ú', '&uacute;');
    WriteLn(ReplaceStr(S, '\=', '='));
  end;
end;

function TTemplate.GetVariable(AVarName: string): string;
var
  Line: TKVPair;
  Return: string = '';
begin
  for Line in FVariables do
  begin
    if Line.Key = AVarName then
    begin
      Return := Line.Value;
      Break;
    end;
  end;
  Result := Return;
end;

function TTemplate.GetImportedValue(AnAlias, AKey: string): string;
var
  i: integer;
  Return: string;
  AParser: TTempParser;
begin
  Return := '';
  i := FImported.IndexOf(AnAlias);
  if i > -1 then
  begin
    AParser := TTempParser.Create(Self);
    Return := TGenFile(FImported.Objects[i]).GetValue(AKey).Value;
    AParser.Free;
  end;
  Result := Return;
end;

function TTemplate.GetWild(ASearch, AnAlias, ADefault: string): string;
var
  Jump, i, j: integer;
  Pairs: array of TKVPair;
  Temp, Right, AWild, Ret: string;
  WildLeft, WildRight, Match: boolean;
begin
  if FImported.Count > 0 then
  begin
    for i := 0 to FImported.Count - 1 do
    begin
      if FImported[i] = AnAlias then
      begin
        Pairs := TGenFile(FImported.Objects[i]).Pairs;
        if Length(Pairs) > 0 then
        begin
          for j := 0 to Length(Pairs) - 1 do
          begin
            Right := Pairs[j].Key;
            Match := True;
            repeat
              Jump := Pos('*', Right);
              if Jump = 1 then
                Jump := Pos('*', Copy(Right, 2, Length(Right))) + 1;
              if (Jump = 1) or (Jump = 0) then
                Jump := Length(Right);
              Temp := Copy(Right, 1, Jump);
              Right := Copy(Right, Length(Temp), Length(Right));
              WildLeft := Pos('*', Temp) = 1;
              WildRight := RPos('*', Temp) = Length(Temp);
              AWild := ReplaceStr(Temp, '*', '');
              if WildLeft and WildRight then
                Match := Pos(AWild, ASearch) > 0
              else if WildRight then
                Match := Pos(AWild, ASearch) = 1
              else if WildLeft then
                Match := Pos(AWild, ASearch) = (Length(ASearch) - Length(AWild)) + 1
              else
                Match := (ASearch = AWild);
              if not Match then
                break;
            until Length(Right) = 1;
            if Match then
            begin
              Ret := Pairs[j].Value;
              Break;
            end;
          end;
          if not Match then
            Ret := ADefault;
        end;
      end;

    end;
  end;
  Result := Ret;
end;

function TTemplate.SetVariable(AKey, AValue: string; Parse:boolean=False): TTemplate;
var
  i: integer;
  AParser:TTempParser;
begin
  AParser := TTempParser.Create(Self);
  if Parse then
    AValue := AParser.ParseToken(AValue);
  if Length(FVariables) > 0 then
  begin
    for i in [0..Length(FVariables) - 1] do
    begin
      if FVariables[i].Key = AKey then
      begin
        FVariables[i].Value := AValue;
        Result := Self;
        Exit;
      end;
    end;
  end;
  SetLength(FVariables, Length(FVariables) + 1);
  FVariables[Length(FVariables) - 1].Key := AKey;
  FVariables[Length(FVariables) - 1].Value := AValue;
  AParser.Free;
  Result := Self;
end;

function TTemplate.DropVariable(AKey: string): TTemplate;
var
  i: integer;
  auxPair: TKVPair;
begin
  if Length(FVariables) > 0 then
  begin
    for i in [0..Length(FVariables) - 1] do
    begin
      if FVariables[i].Key = AKey then
      begin
        auxPair := FVariables[Length(FVariables) - 1];
        FVariables[Length(FVariables) - 1] := FVariables[i];
        FVariables[i] := auxPair;
        SetLength(FVariables, Length(FVariables) - 1);
        Result := Self;
        Exit;
      end;
    end;
  end;
  Result := Self;
end;

function TTemplate.ParseTemplate(var AGen: TGenFileSet;
  var OutputParsed: TStringList): TTemplate;
var
  AParser: TTempParser;
begin
  if FFullName <> '' then
  begin
    FGenFileSet := AGen;
    if Length(Agen.GenFiles) > 0 then
      FOverrides.OutFileName := AGen.GenFiles[0].GenFile.OnlyName
    else
      FOverrides.OutFileName := Name;
    FForGoto := -1;
    FForTimes := 0;
    SetLength(FForLoops, 0);
    FForLevel := -1;
    FRewind := False;
    AParser := TTempParser.Create(Self);
    AParser.ParseTemplate(OutputParsed);
    AParser.Free;
    Result := Self;
  end
  else
  begin
    WriteLn('ERROR: No template path was provided');
    Exit;
  end;
end;

function TTemplate.ParseTemplate(var AGen: TGenFileSet): TTemplate;
begin
  FParsed.Clear;
  Result := ParseTemplate(AGen, FParsed);
end;

function TTemplate.ParseTemplate(var AGen: TGenFile;
  var OutputParsed: TStringList): TTemplate;
var
  AParser: TTempParser;
begin
  if FFullName <> '' then
  begin
    FGenFile := AGen;
    FOverrides.OutFileName := AGen.OnlyName;
    FForGoto := -1;
    FForTimes := 0;
    SetLength(FForLoops, 0);
    FForLevel := -1;
    FRewind := False;
    AParser := TTempParser.Create(Self);
    AParser.ParseTemplate(OutputParsed);
    AParser.Free;
    Result := Self;
  end
  else
  begin
    WriteLn('ERROR: No template path was provided');
    Exit;
  end;
end;

function TTemplate.ParseTemplate(var AGen: TGenFile): TTemplate;
begin
  FParsed.Clear;
  Result := ParseTemplate(AGen, FParsed);
end;

function TTemplate.EvalFilter: boolean;
var
  i: integer;
  AKey, AValue, ATarget: string;
  Return: boolean = False;
begin
  if FOverrides.Filters.Count > 0 then
  begin
    for i := 0 to FOverrides.Filters.Count - 1 do
    begin
      AKey := FOverrides.Filters.Names[i];
      ATarget := FOverrides.Filters.Values[AKey];
      AValue := FGenFileSet.GetValue(AKey).Value;
      if Pos(ATarget, AValue) > 0 then
      begin
        Return := True;
        Break;
      end;
    end;
  end
  else
    Return := True;
  Result := Return;
end;

function TTemplate.EvalBypass: boolean;
var
  i: integer;
  AKey, AValue, ATarget: string;
  Return: boolean = False;
begin
  if FOverrides.Bypasses.Count > 0 then
  begin
    for i := 0 to FOverrides.Bypasses.Count - 1 do
    begin
      AKey := FOverrides.Bypasses.Names[i];
      ATarget := FOverrides.Bypasses.Values[AKey];
      AValue := FGenFileSet.GetValue(AKey).Value;
      if Pos(ATarget, AValue) > 0 then
      begin
        Return := True;
        Break;
      end;
    end;
  end;
  Result := Return;
end;

procedure TTemplate.Clear;
var
  i: integer;
begin
  FOverrides.CopyTo.Clear;
  FOverrides.Filters.Clear;
  FOverrides.Bypasses.Clear;
  if FImported.Count > 0 then
  begin
    for i := 0 to FImported.Count - 1 do
      FImported.Objects[i].Free;

  end;
  FImported.Clear;
  FParsed.Clear;
end;

destructor TTemplate.Destroy;
var
  i: integer;
begin
  FOverrides.CopyTo.Free;
  FOverrides.Filters.Free;
  FOverrides.Bypasses.Free;
  if FSections.Count > 0 then
  begin
    for i := 0 to FSections.Count - 1 do
      FSections.Objects[i].Free;
  end;
  if FImported.Count > 0 then
  begin
    for i := 0 to FImported.Count - 1 do
      FImported.Objects[i].Free;
  end;
  FSections.Free;
  FImported.Free;
  FLines.Free;
  FParsed.Free;
end;

end.
