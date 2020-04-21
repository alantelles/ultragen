unit TemplateClass;

{$mode objfpc}{$H+}
{$LongStrings ON}

interface

uses
  LazUTF8, Classes, SysUtils, FileUtil, DateUtils, StrUtils, Process, crt, httpdefs,
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
  cthreads,
    {$ENDIF}
  {Widestring manager needed for widestring support}
  cwstring,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows, {for setconsoleoutputcp}
  {$ENDIF}

  { Globals }
  WebServerClass, TypesGlobals, ConstantsGlobals,

  { Utils }
  BooleansFunctions, JsonToGenClass,

  { Classes }
  GenFileClass, GenFileSetClass;

const
  FORDEV = 'FORDEV';
  LOOPDEV = 'LOOPDEV';
  IFDEV = 'IFDEV';
  NODEV = 'NODEV';

type
  TOverrides = record
    OutFileName: string;
    Extension: string;
    CopyTo: TStringList;
    Overwrite: boolean;
    Filters: TStringList;
    Bypasses: TStringList;
    Stricts: TStringList;
    ExpAtRoot: boolean;
    RenderBlank: boolean;
  end;

  TUserFunction = record
    FunctionName: string;
    Args: TStringList;
    Lines: TStringList;
    HasReturn: boolean;
  end;

  TWebVars = record
    SessionId, SessionPath: string;
    SessionDuration: integer;
    Request: TRequest;
    Response: TResponse;
  end;

  TDefaultParam = record
    FuncName: string;
    ParamPos: integer;
    DefValue: string;
  end;

  TDefaultParamArray = array of TDefaultParam;

  TForLevel = record
    Times: int64;
    GoToLine: integer;
    ControlVar: string;
    List: TStringList;
    PauseTime: integer;
    Infinite: boolean;
    Limit: integer;
  end;
  TForRecursion = array of TForLevel;
  TIfrecursion = array of boolean;
  TUserFunctions = array of TUserFunction;

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
    FIfLevel: integer;
    FElseLevel: integer;
    FForLoops: TForRecursion;
    FIfTests: TIfRecursion;
    FRewind, FSkip: boolean;
    FLoopTypeLast, FLoopType: string;
    FForSkip: boolean;
    FAbort: boolean;
    FWebVars: TWebVars;
    FUserFunctions: TUserFunctions;
    FAddToFunction: boolean;
    FOrderReturn: boolean;
    FReturnValue: string;
    FIncludedAliases: TStringList;
    FErrorLocation:TErrorLocation;

  public
    constructor Create(ATempName: string = ''; AExpLocation: string = '.');
    property RenderBlank: boolean read FOverrides.RenderBlank
      write FOverrides.RenderBlank;
    property DoAbort: boolean read FAbort write FAbort;
    //property IncludedAliases:TStringList read FIncludedAliases;
    property Extension: string read FOverrides.Extension write FOverrides.Extension;
    property AddToFunction: boolean read FAddToFunction write FAddToFunction;
    property ForSkip: boolean read FForSkip write FForSkip;
    property LoopTypeLast: string read FLoopTypeLast write FLoopTypeLast;
    property LoopType: string read FLoopType write FLoopType;
    property ForLoops: TForRecursion read FForloops write FForLoops;
    property TokenOpen: string read FTokenOpen write FTokenOpen;
    property TokenClose: string read FTokenClose write FTokenClose;
    property LineNumber: integer read FLineNumber write FLineNumber;
    property ForGoto: integer read FForGoto write FForGoto;
    property ForTimes: integer read FForTimes write FForTimes;
    property Rewind: boolean read FRewind write FRewind;
    property Skip: boolean read FSkip write FSkip;
    property CanSave: boolean read FCanSave write FCanSave;
    property FullName: string read FFullName write FFullName;
    property GenFile: TGenFile read FGenFile;
    property GenFileSet: TGenFileSet read FGenFileSet write FGenFileSet;
    property TempLines: TStringList read FLines write FLines;
    property ParsedLines: TStringList read FParsed write FParsed;
    property GenTime: TDateTime read FGenTime;
    property ExportLocation: string read FExpLocation write FExpLocation;
    property OutFilePath: string read FOutFilePath;
    property ScriptMode: boolean read FScriptMode write FScriptMode;
    property CommentBlock: boolean read FCommentBlock write FCommentBlock;
    property Imported: TStringList read FImported write FImported;
    property Filters: TStringList read FOverrides.Filters write FOverrides.Filters;
    property Bypasses: TStringList read FOverrides.Bypasses write FOverrides.Bypasses;
    property Stricts: TStringList read FOverrides.Stricts write FOverrides.Stricts;
    property Variables: TDict read FVariables write FVariables;
    property Sections: TStringList read FSections write FSections;
    property ForLevel: integer read FForLevel write FForLevel;
    property IfLevel: integer read FIfLevel write FIfLevel;
    property ElseLevel: integer read FElseLevel write FElseLevel;
    property IfRecursion: TIfRecursion read FIfTests write FIfTests;
    property WebVars: TWebVars read FWebVars write FWebVars;
    property UserFunctions: TUserFunctions read FUserFunctions write FUserFunctions;
    property OrderReturn: boolean read FOrderReturn write FOrderReturn;
    property ReturnValue: string read FReturnValue;
    procedure ParseTokens(indexes: array of integer; var AParams:TStringList);

    function Name: string;
    function SetPredefined(AKey, AValue: string): boolean;
    function Load(ATempName: string): TTemplate;
    function Load(ATempList: TStringList; ATempName: string): TTemplate;
    procedure SetErrorLocation;
    procedure SetFunctionsLength(NewLen: integer);
    procedure LoadText(var Params: TStringList);
    function Save: TTemplate;
    procedure ProcessTemplate(var Params: TStringList; DoPrint: boolean = False);
    function GetVariable(AVarName: string): string;
    function SetVariable(AKey, AValue: string; Parse: boolean = False; IsConst:boolean = False): TTemplate;
    function DropVariable(AKey: string): TTemplate;
    function HasKey(AKey:string; AnAlias:string=''):boolean;
    procedure ExtendTemplate(ATemplate: string; Parent: string = '');
    procedure ImportModule(var Params: TStringList);
    procedure IncludeTemplate(var Params: TStringList);
    procedure Execute(var Params: TStringList; Async:boolean; silent:boolean=False);
    procedure InputValue(var Params: TStringList; DoParse: boolean);
    function ParseTemplate(var AGen: TGenFileSet): string;
    function ParseTemplate(var AGen: TGenFileSet; var OutputParsed: TStringList): string;
    function ParseTemplate(var AGen: TGenFile): string;
    function ParseTemplate(var AGen: TGenFile; var OutputParsed: TStringList): string;
    function GetWild(ASearch, AnAlias, ADefault: string): string;
    function GetWild(ASearch, AnAlias: string): string;
    function RouteMatch(ASearch, AnAlias: string; ADefault: string = ''): string;
    procedure Print;
    procedure PrintLine(var Params: TStringList; ToConsole, ToOutput: boolean; SameLine:boolean = False);
    procedure ParseAbort(var Params: TStringList);
    procedure PrintParsed;
    procedure ForPrepare(var Params: TStringList; ForLoop: boolean = True);
    procedure BreakFor;
    procedure ContinueFor;
    procedure EndFor;
    procedure LoopPrepare(var Params: TStringList;var PureParams: TStringList; ForLoop: boolean = True);
    procedure EndLoop;
    function EvalFilter: boolean;
    function EvalBypass: boolean;
    function EvalStrict: boolean;
    procedure ExplodeStr(var Params: TStringList; var PureParams: TStringList);
    procedure Clear;
    procedure IfPrepare(var Params: TStringList; var PureParams: TStringList;
      IfNot: boolean);
    procedure ElseDecision;
    procedure EndIf;
    procedure ListFiles(var Params: TStringList; var PureParams: TStringList);
    procedure ListDirs(var Params: TStringList; var PureParams: TStringList);
    procedure LimitedListFiles(var Params: TStringList; var PureParams: TStringList);
    procedure Move(var Params: TStringList);
    procedure TempFileCopy(var Params: TStringList);
    procedure DoPause(var Params: TStringList);
    //Gen file handling
    procedure ChangeGenAlias(var Params:TStringList);
    procedure SetGenValue(var Params: TStringList);
    procedure SaveGen(var Params: TStringList);
    procedure CreateGen(var Params: TStringList);
    procedure UnloadGen(var Params: TStringList);
    procedure LoadGenFolder(var Params: TStringList);
    procedure SetGenName(var Params: TStringList);
    procedure MapGenKeys(var Params: TStringList; DoMap: boolean);
    procedure GroupKeys(var Params: TStringList);
    procedure CaptureGen(var Params: TStringList);
    // end gen file handling
    //queue procs
    procedure CreateQueue(var Params:TStringList; var PureParams:TStringList);
    procedure QueueTask(var Params:TStringList);
    procedure DestroyQueue(var Params:TStringList);
    procedure ActivateQueue(var Params:TStringList);
    //end queue
    //textsave
    procedure LogText(var Params:TStringList);
    //end textsave
    //Web module procedures
    procedure RedirectTo(var Params: TStringList);
    procedure DestroySession(var Params: TStringList);
    procedure SetSessionVar(var Params: TStringList);
    procedure DropSessionVar(var Params: TStringList);
    procedure SetCookie(var Params: TStringList);
    procedure SetRawCookie(var Params: TStringList);
    procedure CreateSession(var Params: TStringList);
    procedure DropCookie(var Params: TStringList);
    procedure SetWebVars(var AWebVars:TWebVars);
    procedure ParseJson(var Params: TStringList);
    function RequestRest(var Params: TStringList; var PureParams: TStringList):string;
    function RequestRestPost(var Params: TStringList; var PureParams: TStringList):string;
    //end web procedures
    function MapElem(var Params:TStringList;var PureParams:TStringList):string;
    { parei a mudança de parse aqui }
    procedure StartFunction(var Params: TStringList; var PureParams: TStringList;
      HasRet: boolean);
    function ArrowFunction(var Params: TStringList; var PureParams: TStringList):string;
    procedure MakeFunctionsRoom;
    procedure InitializeVars(var Params: TStringList; SelfAssign:boolean=False);
    procedure FunctionReturn(var Params: TStringList);
    procedure EndFunction;
    procedure AddLineToFunction(ALine: string);
    function FindFunction(AnAlias:string):integer;
    function ExecuteFunction(FuncName: string; HasRet: boolean;
      var Params: TStringList): string;
    procedure POC(PureParams, Params:TStringList);

    destructor Destroy; override;
  end;

implementation

uses FileHandlingUtils,
  ParserClass,
  StringsFunctions, VariablesGlobals,
  fphttpclient, openssl,
  { ExceptionsClasses }
  VariableExceptionsClass,
  GenExceptionsClass,
  AliasExceptionClass,QueueListClass,
  FileExceptionClass,
  ExtensionClass;

constructor TTemplate.Create(ATempName: string = ''; AExpLocation: string = '.');
begin
  FLoopType := NODEV;
  FLoopTypeLast := NODEV;
  FOverrides.Overwrite := False;
  FOverrides.CopyTo := TStringList.Create;
  FOverrides.Filters := TStringList.Create;
  //FOverrides.Filters.NameValueSeparator := OVER_PARAM;
  FOverrides.Bypasses := TStringList.Create;
  //FOverrides.Bypasses.NameValueSeparator := OVER_PARAM;
  FOverrides.Stricts := TStringList.Create;
  FCommentBlock := False;
  FScriptMode := False;
  FIncludedAliases := TStringList.Create;

  //FWebVars.Request := TRequest.Create;
  //FWebVars.Response := TResponse.Create;

  if (Length(AExpLocation) > 0) and (AExpLocation[Length(AExpLocation)] =
    DirectorySeparator) then
    AExpLocation := Copy(AExpLocation, 1, Length(AExpLocation) - 1)
  else if Length(AExpLocation) = 0 then
    AExpLocation := '.';
  FExpLocation := AExpLocation;

  FFullName := '';
  FTokenOpen := TOKEN_OPEN;
  FTOkenClose := TOKEN_CLOSE;
  SetLength(FUserFunctions, 0);
  FImported := TStringList.Create;
  FImported.SkipLastLineBreak := True;
  FSections := TStringList.Create;
  FSections.SkipLastLineBreak := True;
  FLines := TStringList.Create;
  FLines.SkipLastLineBreak := True;
  FParsed := TStringList.Create;
  FParsed.SkipLastLineBreak := True;
  if ATempName <> '' then
  begin
    FFullName := ExpandFileName(ATempName);
    Load(FFullName);
  end;
end;

procedure TTemplate.ParseTokens(indexes: array of integer; var AParams:TStringList);
var
  i:integer;
  AParser:TTempParser;
begin
  AParser := TTempParser.Create(self);
  if Length(indexes) = 0 then
  begin
    if AParams.Count > 0 then
    begin
      for i:=0 to AParams.Count - 1 do
        AParams[i] := AParser.ParseToken(AParams[i]);
    end;
  end
  else
  begin
    for i:=0 to Length(indexes)-1 do
    begin
      try
        AParams[indexes[i]] := AParser.ParseToken(AParams[indexes[i]]);
      except

      end;
    end;
  end;
  AParser.Free;
end;

procedure TTemplate.POC(PureParams, Params:TStringList);

begin

end;

procedure TTemplate.SetErrorLocation;
begin
  with FErrorLocation do
  begin
    LineNumber := FLineNumber;
    TempName := FFullName;
    Line := FLines[FLineNumber];
  end;
end;

function TTemplate.ArrowFunction(var Params: TStringList; var PureParams: TStringList):string;
var
  Ret:string='';
  ATemplate:TTemplate;
  AParser:TTempParser;
  AGenSet:TGenFileSet;
  ArgsStr, Arg, P:string;
  ArgsList:TStringList;
  i:integer=0;
begin
  AGenSet := TGenFileSet.Create;
  FGenFileSet.CopyGenSet(AGenSet);
  ATemplate := TTemplate.Create;
  ArgsStr := Copy(PureParams[0],2,Length(PureParams[0])-2);
  AParser := TTempParser.Create(Self);
  ArgsList := TStringList.Create;
  AParser.ParseParams(ArgsStr,ArgsList);
  for Arg in ArgsList do
  begin
    P := AParser.ParseToken(Arg);
    ATemplate.SetVariable('arg'+IntToStr(i),P);
    i := i+1;
  end;
  AParser.Free;
  PureParams.Delete(0);
  ATemplate.TempLines.AddStrings(PureParams);
  ATemplate.FullName := 'arrow';
  ATemplate.ScriptMode := True;
  Ret := ATemplate.ParseTemplate(AGenSet,FParsed);
  ATemplate.Free;
  Result := Ret;
end;

function TTemplate.FindFunction(AnAlias:string):integer;
var
  Ret:integer = -1;
  i, len:integer;
begin
  len := Length(FUserFunctions);
  if len > 0 then
  begin
    for i:=0 to len-1 do
    begin
      if FUserFunctions[i].FunctionName = AnAlias then
      begin
        Ret := i;
        break;
      end;
    end;
  end;
  Result := Ret;
end;

procedure TTemplate.SetWebVars(var AWebVars:TWebVars);
begin

  FWebVars := AWebVars;
end;

procedure TTemplate.SetFunctionsLength(NewLen: integer);
begin
  SetLength(FUserFunctions, NewLen);
end;

procedure TTemplate.SetCookie(var Params: TStringList);
var
  C: TCookie;
begin
  ParseTokens([], Params);
  C := FWebVars.Response.Cookies.Add;
  C.Name := Params[0];
  C.Value := Params[1];
  if Params.Count > 2 then
  begin
    try
      C.Expires := ScanDateTime('yyyy-mm-dd hh:nn:ss', Params[2]);
    except
      WriteLn('WARNING: Date given in incorrect format. Cookie won''t have expire date');
    end;
  end;
end;

procedure TTemplate.SetRawCookie(var Params: TStringList);
var
  //RawPart: string = '';
  C: TCookie;
  i:integer;
begin
  ParseTokens([], Params);
  C := FWebVars.Response.Cookies.Add;
  C.Name := Params[0];
  C.Value := Params[1];
  if Params.Count > 2 then
  begin
    i := FGenFileSet.IndexOf(Params[2]);
    if i > -1 then
    begin

      try
        C.Expires := ScanDateTime('yyyy-mm-dd hh:nn:ss', FGenFileSet.GenFiles[i].GenFile.GetValue('expires').Value);
      except
        WriteLn('WARNING: Date given in incorrect format. Cookie won''t have expire date');
      end;
      C.Path := FGenFileSet.GenFiles[i].GenFile.GetValue('path', '/').Value;
      C.Domain := FGenFileSet.GenFiles[i].GenFile.GetValue('domain', '.').Value;
      C.Secure := StrToBoolean(FGenFileSet.GenFiles[i].GenFile.GetValue('secure').Value);
    end;
  end;
  // if Params.Count = 3 then
  //  RawPart := '; ' + Params[2];
  // FParsed.Add('<script>document.cookie="' + Params[0] + '=' + Params[1] + RawPart + '"</script>');
end;

procedure TTemplate.DropCookie(var Params: TStringList);
var
  C: TCookie;
begin
  ParseTokens([], Params);
  C := FWebVars.Response.Cookies.Add;
  C.Name := Params[0];
  C.Expires := ScanDateTime('yyyy-mm-dd', '1970-01-01');
end;

procedure TTemplate.DestroySession(var Params: TStringList);
var
  ParamsC: TStringList;
begin
  ParseTokens([], Params);
  ParamsC := TStringList.Create;
  ParamsC.Add('sessionID');
  DropCookie(ParamsC);
  ParamsC.Clear;
  ParamsC.Add('session');
  UnloadGen(ParamsC);
  ParamsC.Free;
  SysUtils.DeleteFile(FWebVars.SessionPath + DirectorySeparator + FWebVars.SessionId + '.gen');

end;

procedure TTemplate.SetSessionVar(var Params: TStringList);
var
  AGenFile: TGenFile;
  SessionFile: string;
  i: integer;
begin
  ParseTokens([], Params);
  if FWebVars.SessionId <> '' then
  begin
    SessionFile := FWebVars.SessionPath + DirectorySeparator + FWebVars.SessionId + '.gen';
    AGenFile := TGenFile.Create;
    AGenFile.Load(SessionFile);
    AGenFile.SetValue('_session:' + Params[0], Params[1]);
    AGenFile.Save;
    AGenFile.Free;
    i := FGenFileSet.IndexOf('session');
    FGenFileSet.GenFiles[i].GenFile.Load(SessionFile);
  end;
end;

procedure TTemplate.DropSessionVar(var Params: TStringList);
var
  AGenFile: TGenFile;
  SessionFile: string;
  i: integer;
begin
  ParseTokens([],Params);
  SessionFile := FWebVars.SessionPath + DirectorySeparator + FWebVars.SessionId + '.gen';
  AGenFile := TGenFile.Create;
  AGenFile.Load(SessionFile);
  AGenFile.DropKey('_session:' + Params[0]);
  AGenFile.Save;
  AGenFile.Free;
  i := FGenFileSet.IndexOf('session');
  FGenFileSet.GenFiles[i].GenFile.Load(SessionFile);
end;

procedure TTemplate.RedirectTo(var Params: TStringList);
begin
  ParseTokens([], Params);
  FWebVars.Response.Code := 303;
  FWebVars.Response.Location := Params[0];
end;

procedure TTemplate.CreateSession(var Params: TStringList);
var
  ASession: TGenFile;
  SessionId, SessionFile: string;
  ParamsC: TStringList;
  i: integer;
begin
  if FWebVars.SessionId = '' then
  begin
    SessionId := CreateSessionId;
    FwebVars.SessionId := SessionId;
    ASession := TGenFile.Create;
    ASession.SetValue('_session:sessionID', SessionId);
    ASession.SetValue('_session:expiresAt', FormatDateTime(
      DATE_INTERCHANGE_FORMAT, IncMinute(Now, FWebVars.SessionDuration)));
    ASession.FullName := FWebVars.SessionPath + DirectorySeparator + SessionId + '.gen';
    FileHandlingUtils.CreateDirTree(ASession.FullName);
    ASession.Save;
    ASession.Free;
    ParamsC := TStringList.Create;
    ParamsC.Add('sessionID');
    ParamsC.Add(SessionId);
    SetCookie(ParamsC);
    ParamsC.Free;
    i := FGenFileSet.IndexOf('session');
    FGenFileSet.GenFiles[i].GenFile.Load(FWebVars.SessionPath +
      DirectorySeparator + SessionId + '.gen');
  end
  else
    WriteLn('Session already created');
end;

procedure TTemplate.ParseJson(var Params: TStringList);
var
  AJson: TJson2Gen;
  i: integer;
  Errorlocation:TErrorLocation;
begin
  ParseTokens([], Params);
  with ErrorLocation do
  begin
    LineNumber := FLineNumber;
    TempName := FFullName;
    Line := FLines[FLineNumber];
  end;
  EGenError.Create(E_GEN_ALREADY_EXISTS,ErrorLocation,'',Params[0],-1).TestAliasExists(FGenFileSet).ERaise(False);
  EAliasError.Create(E_FORBIDDEN_ALIAS_NAME,ErrorLocation,Params[0]);
  i := FGenFileSet.Add(True, Params[0]);
  if Params.Count = 3 then
    AJson := TJson2Gen.Create(Params[1], FGenFileSet.GenFiles[i].GenFile, Params[2])
  else
    AJson := TJson2Gen.Create(Params[1], FGenFileSet.GenFiles[i].GenFile);
  AJson.ParseJson;
  AJson.Free;
end;

function TTemplate.RequestRestPost(var Params: TStringList; var PureParams: TStringList):string;
var
  Requirer: TFPHttpClient;
  Return: string = '';
  SS:TStringStream;
  URL:string;
begin
  //ParseTokens([], Params);
  InitSSLInterface;
  Requirer := TFPHttpClient.Create(nil);
  Requirer.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
  try
    Requirer.AllowRedirect := True;
    Requirer.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    URL := Params[0];
    Params.Delete(0);
    Params.SkipLastLineBreak := True;
    Params.LineBreak := '&';
    if Params.Count > 0 then
    begin
      SS := TStringStream.Create(Params.Text);
      SS.Position := 0;
      Requirer.RequestBody := SS;
    end;
    Return := Requirer.Post(URL);
  finally
    SS.Free;
    Requirer.Free;
    //SetVariable(PureParams[0], Return);
  end;
  Result := Return;
end;

procedure TTemplate.InitializeVars(var Params: TStringList; SelfAssign:boolean=False);
var
  s:string;
begin
  if SelfAssign then
    for s in Params do
      SetVariable(s, s, False)
  else
    for s in Params do
      SetVariable(s, StringsFunctions.CreateRandomHash, False);
end;

function TTemplate.RequestRest(var Params: TStringList; var PureParams: TStringList):string;
var
  Requirer: TFPHttpClient;
  Return: string = '';
  URL, AQuery:string;
begin
  //ParseTokens([], Params);
  InitSSLInterface;
  Requirer := TFPHttpClient.Create(nil);
  Requirer.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
  try
    Requirer.AllowRedirect := True;
    URL := Params[0];
    Params.Delete(0);
    Params.SkipLastLineBreak := True;
    Params.LineBreak := '&';
    if Params.Count > 0 then
    begin
      URL := URL + '?';
      URL := URL + Params.Text;
    end;
    try
      Return := Requirer.Get(URL);

    except
      WriteLn('Couldn''t load ssl lib');
    end;
  finally
    Requirer.Free;
    //SetVariable(PureParams[0], Return);
  end;
  Result := Return;
end;

procedure TTemplate.CreateQueue(var Params:TStringList; var PureParams:TStringList);
var
  Errorlocation:TErrorLocation;
  begin
   ParseTokens([0,1], Params);
   with ErrorLocation do
   begin
     LineNumber := FLineNumber;
     TempName := FFullName;
     Line := FLines[FLineNumber];
   end;
  EAliasError.Create(E_FORBIDDEN_ALIAS_NAME,ErrorLocation,Params[0]);
  if Params.Count = 1 then
    GlobalQueue.AddQueue(Params[0],1, False)
  else if Params.Count = 2 then
    GlobalQueue.AddQueue(Params[0],StrToInt(Params[1]),False)
  else if Params.Count = 3 then
    GlobalQueue.AddQueue(Params[0],StrToInt(Params[1]), StrToBoolean(PureParams[2]));
end;

procedure TTemplate.DestroyQueue(var Params:TStringList);
begin
  ParseTokens([], Params);
  GlobalQueue.StopQueue(Params[0]);
end;

procedure TTemplate.ActivateQueue(var Params:TStringList);
begin
  ParseTokens([], Params);
  GlobalQueue.ActivateQueue(Params[0]);
end;

procedure TTemplate.QueueTask(var Params:TStringList);
var
  AFuncName:String;
  AQueue:string;
  i:integer;
  AFunc:TUserFunction;
  AParams:TStringList;
begin
  ParseTokens([], Params);
  AFuncName := Params[1];
  AQueue := Params[0];
  Params.Delete(0);
  Params.Delete(0);
  AParams := TSTringList.Create;
  AParams.AddStrings(Params);
  i := FindFunction(AFuncName);
  if i > -1 then
  begin
    AFunc := FUserFunctions[i];
    GlobalQueue.AddTask(Self.FullName,AQueue,i,AParams,FGenFileSet);
  end;
end;

procedure TTemplate.LogText(var Params:TStringList);
var
  AText:TStringList;
begin
  ParseTokens([], Params);
  AText := TStringList.Create;
  AText.SkipLastLineBreak := True;
  if FileExists(Params[1]) then
    AText.LoadFromFile(Params[1])
  else
    CreateDirTree(Params[1]);
  if (Params.Count = 3) and (StrToBoolean(Params[2]) = True) then
    AText.Clear;
  AText.Add(Params[0]);
  AText.SaveToFile(Params[1]);
  AText.Free;
end;

function TTemplate.HasKey(AKey:string; AnAlias:string=''):boolean;
var
  i :integer;
  Return:boolean=False;
begin
  SetErrorLocation;
  if AnAlias = '' then
    Return := FGenFileSet.HasKey(AKey)
  else
  begin
    i := FGenFileSet.IndexOf(AnAlias);
    if i > -1 then
    begin
      Return := FGenFileSet.GenFiles[i].GenFile.HasKey(AKey);
    end
    else
      EGenError.Create(E_GEN_NOT_EXIST,FErrorLocation,AKey,AnAlias,-1).ERaise(True);
  end;
  Result := Return;
end;

procedure TTemplate.CreateGen(var Params: TStringList);
var
  AGenFile: TGenFile;
  Errorlocation:TErrorLocation;
begin
  //Params:
  //0: alias name
  //?1: gen file
  //?2: gen sep
  //?3: gen default
  ParseTokens([], Params);
  with ErrorLocation do
  begin
    LineNumber := FLineNumber;
    TempName := FFullName;
    Line := FLines[FLineNumber];
  end;
  EGenError.Create(E_GEN_ALREADY_EXISTS,ErrorLocation,'',Params[0],-1).TestAliasExists(FGenFileSet).ERaise(False);
  EAliasError.Create(E_FORBIDDEN_ALIAS_NAME,ErrorLocation,Params[0]).TestValidAliasName.ERaise(False);
  AGenFile := TGenFile.Create;
  if Params.Count > 1 then
  begin
    EFileError.Create(E_FILE_NOT_FOUND,ErrorLocation,Params[1]).TestFileExists.ERaise(False);
    AGenFile.FullName := Params[1];
    if FileExists(Params[1]) then
    begin
      if Params.Count > 2 then
      begin
        AGenFile.GenSeparator := Params[2][1];
        if Params.Count = 4 then
          AGenFile.IfNotFound := Params[3];
      end;
      AGenFile.Load(Params[1]);
    end;
  end;
  FGenFileSet.Add(AGenFile, Params[0]);
end;

procedure TTemplate.UnloadGen(var Params: TStringList);
var
  AnAlias: string;
  Errorlocation:TErrorLocation;
begin
   ParseTokens([], Params);
   with ErrorLocation do
   begin
     LineNumber := FLineNumber;
     TempName := FFullName;
     Line := FLines[FLineNumber];
   end;
  EGenError.Create(E_GEN_NOT_EXIST,ErrorLocation,'',Params[0],-1).TestAliasNotExists(FGenFileSet).ERaise(False);
  AnAlias := Params[0];
  FGenFileSet.Drop(AnAlias);
end;

procedure TTemplate.ParseAbort(var Params: TStringList);
var
  msg: string;
begin
  msg := Params.Text;
  msg := Copy(msg, 1, Length(msg) - 1);
  WriteLn(msg);
  FParsed.Add(msg);
  FAbort := True;
end;

procedure TTemplate.ProcessTemplate(var Params: TStringList; DoPrint: boolean = False);
var
  ATemplate: TTemplate;
  AGenSet: TGenFileSet;
  AGenList: TStringList;
  TempPath, AGen: string;
  i: integer;
  Errorlocation:TErrorLocation;
begin

  with ErrorLocation do
  begin
    LineNumber := FLineNumber;
    TempName := FFullName;
    Line := FLines[FLineNumber];
  end;
  ParseTokens([], Params);
  TempPath := Params[0];
  EFileError.Create(E_FILE_NOT_FOUND,ErrorLocation,TempPath).TestFileExists.ERaise(False);
  ATemplate := TTemplate.Create(TempPath);
  AGenSet := TGenFileSet.Create;
  if Params[1] <> '' then
  begin
    AGenList := TStringList.Create;
    AGenList.Delimiter := GEN_FILES_CALL_SEP;     
    AGenList.StrictDelimiter := True;
    AGenList.DelimitedText := Params[1];
    //GenList is a listable delimited by pipe |
    for AGen in AGenList do
    begin
      EFileError.Create(E_FILE_NOT_FOUND,ErrorLocation,AGen).TestFileExists.ERaise(False);
      AGenSet.Add(AGen);
    end;
    AGenList.Free;
  end;
  if Params.Count > 2 then
  begin
    for i := 2 to Params.Count - 1 do
      ATemplate.SetVariable('param[' + IntToStr(i - 2) + ']', Params[i]);
  end;
  ATemplate.ParseTemplate(AGenSet);
  if DoPrint then
    ATemplate.PrintParsed
  else
    ATemplate.Save;
  ATemplate.Free;
end;

procedure TTemplate.Move(var Params: TStringList);
var
  ANewName: string;
  DestFile: string;
  Errorlocation:TErrorLocation;
 begin
   ParseTokens([], Params);
   with ErrorLocation do
   begin
     LineNumber := FLineNumber;
     TempName := FFullName;
     Line := FLines[FLineNumber];
   end;
  EFileError.Create(E_FILE_NOT_FOUND,ErrorLocation,Params[0]).TestFileExists.ERaise(False);
  if FileExists(Params[0]) then
  begin
    ANewName := GetFileName(Params[0]);
    CreateDirTree(Params[1], False);
    if Params.Count = 3 then
      ANewName := Params[2];
    DestFile := Params[1] + DirectorySeparator + ANewName;
    CreateDirTree(DestFile);
    if FileExists(DestFile) then
      SysUtils.DeleteFile(DestFile);
    RenameFile(Params[0], RemoveLastBackslash(Params[1]) + DirectorySeparator + ANewName);
  end;
end;

procedure TTemplate.InputValue(var Params: TStringList; DoParse: boolean);
var
  AValue: string;
  Errorlocation:TErrorLocation;
begin
  ParseTokens([1], Params);

  with ErrorLocation do
  begin
    LineNumber := FLineNumber;
    TempName := FFullName;
    Line := FLines[FLineNumber];
  end;
  if Params.Count = 2 then
    Write(Params[1] + ' ');
  ReadLn(AValue);
  EVariableError.Create(E_FORBBIDEN_VAR_NAME,ErrorLocation,Params[0]).TestValidName.ERaise(False);
  SetVariable(Params[0], AValue, DoParse);
end;

procedure TTemplate.SetGenValue(var Params: TStringList);
var
  GenAlias, AKey, AValue, FullGenName: string;
  AGen: TGenFile;
  i: integer;
  Errorlocation:TErrorLocation;
begin
  ParseTokens([], Params);
  with ErrorLocation do
  begin
    LineNumber := FLineNumber;
    TempName := FFullName;
    Line := FLines[FLineNumber];
  end;
  GenAlias := Params[0];
  AKey := Params[1];
  AValue := Params[2];
  i := FGenFileSet.IndexOf(GenAlias);
  if i = -1 then
  begin
    EGenError.Create(E_GEN_NOT_EXIST,ErrorLocation,Akey,GenAlias,i).ERaise();
  end;
  FGenFileSet.GenFiles[i].GenFile.SetValue(AKey, AValue);
end;

procedure TTemplate.ChangeGenAlias(var Params:TStringList);
var
  GenAlias: string;
  i: integer;

begin
  ParseTokens([], Params);
  SetErrorLocation;
  GenAlias := Params[0];
  i := FGenFileSet.IndexOf(GenAlias);
  if i = -1 then
  begin
    EGenError.Create(E_GEN_NOT_EXIST,FErrorLocation,'',GenAlias,i).ERaise();
  end;
  FGenFileSet.GenFiles[i].GenAlias := Params[1];
end;

procedure TTemplate.SaveGen(var Params: TStringList);
var
  GenAlias, a: string;
  i: integer;
  Errorlocation:TErrorLocation;
begin
    ParseTokens([], Params);
    with ErrorLocation do
    begin
      LineNumber := FLineNumber;
      TempName := FFullName;
      Line := FLines[FLineNumber];
    end;
  GenAlias := Params[0];
  i := FGenFileSet.IndexOf(GenAlias);
  if i > -1 then
  begin
    if Params.Count = 1 then
    begin
      EGenError.Create(E_EMPTY_GEN_FULLNAME,ErrorLocation,'',GenAlias,-1).TestFullNameEmpty(FGenFileSet.GenFiles[i].GenFile.FullName).ERaise(False);
      FGenFileSet.GenFiles[i].GenFile.Save
    end
    else if Params.Count = 2 then
      FGenFileSet.GenFiles[i].GenFile.Save(Params[1]);
  end
  else
    EGenError.Create(E_GEN_NOT_EXIST,ErrorLocation,'',GenAlias,-1).ERaise(True);
end;

procedure TTemplate.GroupKeys(var Params: TStringList);
var
  i:integer;
  APair: TKVPair;
  AGenFile:TGenFile;
  //groupKeys:'genAlias','prefix','newAlias'
  //groupKeys+mapGenKeys: 'genAlias', 'prefix', 'newAlias', 'varPrefix', 'destroyAfter'
   Errorlocation:TErrorLocation;
begin
  ParseTokens([], Params);
  with ErrorLocation do
  begin
    LineNumber := FLineNumber;
    TempName := FFullName;
    Line := FLines[FLineNumber];
  end;
  i := FGenFileSet.IndexOf(Params[0]);
  if i > -1 then
  begin
    AGenFile := TGenFile.Create;
    for APair in FGenFileSet.GenFiles[i].GenFile.Pairs do
    begin
      if Copy(APair.Key,1,Length(Params[1])) = Params[1] then
        AGenFile.SetValue(ReplaceStr(APair.Key,Params[1],''),APair.Value);
    end;
    EGenError.Create(E_GEN_ALREADY_EXISTS,ErrorLocation,'',Params[2],-1).TestAliasExists(FGenFileSet).ERaise(False);
    FGenFileSet.Add(AGenFile,Params[2]);
  end
  else
    EGenError.Create(E_GEN_NOT_EXIST,FErrorLocation,'',Params[0],-1).ERaise(True);
  // pass a 4th parameter do a gen/variables mapping using the prefix given in 4th parameter
  if Params.Count = 4 then
  begin
    Params.Delete(0);
    Params.Delete(0);
    MapGenKeys(Params, True);
    if Params.Count = 3 then
    begin
      if not (Params[2] = LANG_FALSE) then
        FGenFileSet.Drop(Params[0]);
    end
    else
      FGenFileSet.Drop(Params[0]);
  end;
end;

procedure TTemplate.CaptureGen(var Params: TStringList);
var
  i,j:integer;
  ResetMode:boolean = True;
  Errorlocation:TErrorLocation;
begin
 ParseTokens([], Params);
 with ErrorLocation do
 begin
   LineNumber := FLineNumber;
   TempName := FFullName;
   Line := FLines[FLineNumber];
 end;
  //captureGen:'dest','src'
  i := FGenFileSet.IndexOf(Params[0]);
  if i < 0 then
    EGenError.Create(E_GEN_NOT_EXIST,ErrorLocation,'',Params[0],-1).ERaise;

  j := FGenFileSet.IndexOf(Params[1]);
  if j < 0 then
    EGenError.Create(E_GEN_NOT_EXIST,ErrorLocation,'',Params[1],-1).ERaise;

  if Params.Count = 3 then
  begin
    ResetMode := StrToBoolean(Params[2]);
  end;

  if (j > -1) and (i > -1) then
    FGenFileSet.GenFiles[j].GenFile.CaptureGen(FGenFileSet.GenFiles[i].GenFile, ResetMode);
end;

procedure TTemplate.MapGenKeys(var Params: TStringList; DoMap: boolean);
var
  i: integer;
  APair: TKVPair;
  VarAlias, VarName: string;
  Errorlocation:TErrorLocation;
begin
 ParseTokens([], Params);
 with ErrorLocation do
 begin
   LineNumber := FLineNumber;
   TempName := FFullName;
   Line := FLines[FLineNumber];
 end;
  //mapGenKeys:aGenAlias,aVarPrefix
  i := FGenFileSet.IndexOf(Params[0]);
  if i > -1 then
  begin
    VarAlias := Params[0];
    if VarAlias <> '' then
    begin
      EAliasError.Create(E_INCLUDED_ALIAS_ALREADY_EXISTS,ErrorLocation,VarAlias).TestIncludedAliasExists(FIncludedAliases).ERaise(False);
      EAliasError.Create(E_FORBIDDEN_ALIAS_NAME,ErrorLocation,VarAlias).TestValidAliasName.ERaise(False);

    end;
    VarAlias := Params[0] + ATTR_ACCESSOR;
    if Params.Count = 2 then
    begin
      VarAlias := Params[1];
      if VarAlias <> '' then
      begin

        EAliasError.Create(E_FORBIDDEN_ALIAS_NAME,ErrorLocation,VarAlias).TestValidAliasName.ERaise(False);
        EAliasError.Create(E_INCLUDED_ALIAS_ALREADY_EXISTS,ErrorLocation,VarAlias).TestIncludedAliasExists(FIncludedAliases).ERaise(False);
      end;
      if VarAlias <> '' then
        VarAlias := VarAlias + ATTR_ACCESSOR;
    end;
    for APair in FGenFileSet.GenFiles[i].GenFile.Pairs do
    begin
      VarName := VarAlias + APair.Key;
      if Pos(VarName[1], VARS_ALLOWED) = 0 then
        VarName := '_' + VarName;
      if DoMap then
        SetVariable(VarName, APair.Value)
      else
        DropVariable(VarName + APair.Key);
    end;
  end
  else
    EGenError.Create(E_GEN_NOT_EXIST,ErrorLocation,'',Params[0],-1).ERaise();
end;

procedure TTemplate.LoadGenFolder(var Params: TStringList);
var
  AGenList: TStringList;
  s: string;
begin
  ParseTokens([], Params);
  AGenList := TStringList.Create;
  FindAllFiles(AGenList, Params[1], '*.gen;*.GEN', False);
  AGenList.Sort;
  FGenFileSet.Enlist(AGenList, Params[0]);
  AGenList.Free;
end;

procedure TTemplate.TempFileCopy(var Params: TStringList);
var
  ANewName: string;
begin
  ParseTokens([], Params);
  if FileExists(Params[0]) then
  begin
    ANewName := GetFileName(Params[0]);
    CreateDirTree(Params[1]);
    if Params.Count = 3 then
      ANewName := Params[2];
    CreateDirTree(Params[1] + DirectorySeparator + ANewName);
    FileUtil.CopyFile(Params[0], RemoveLastBackslash(Params[1]) +
      DirectorySeparator + ANewName, [cffOverwriteFile, cffPreserveTime]);
  end;
end;

procedure TTemplate.Execute(var Params: TStringList; ASync:boolean; silent:boolean=False);
var
  AProcess: TProcess;
  i: integer;
begin
  ParseTokens([], Params);
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := Params[0];
    if not ASync then
      AProcess.Options := AProcess.Options + [poWaitOnExit];
    if Silent then
      AProcess.Options := AProcess.Options + [ponoConsole];
    if Params.Count > 1 then
    begin
      for i := 1 to Params.Count - 1 do
        AProcess.Parameters.Add(Params[i]);
    end;
    AProcess.Execute;
  except
    WriteLn('Process '+Params[0]+' not found!');
  end;
  AProcess.Free;
end;


procedure TTemplate.PrintLine(var Params: TStringList; ToConsole, ToOutput: boolean; SameLine:boolean = False);
var
  Return: string = '';
  P: string;
  i:integer;
begin
  ParseTokens([], Params);
  for P in Params do
    Return := Return + P;
  if ToOutput then
    FParsed.Add(Return);
  if ToConsole then
  begin
    if SameLine then
      Write(Return)
    else
      Write(Return+sLineBreak);
  end;
end;

procedure TTemplate.ImportModule(var Params: TStringList);
var
  Files, IParams:TStringList;
  APath, AMod, F: string;
begin
  ParseTokens([], Params);
  Files := TStringList.Create;
  IParams := TStringList.Create;
  AMod := Params[0];
  if Params.Count > 1 then
    APath := Params[1] + DirectorySeparator + AMod
  else
    APath := U_HOME + DirectorySeparator + BUILTIN_MODULES + DirectorySeparator + AMod;

  if FileExists(APath + DirectorySeparator + INIT_MODULE) then
  begin
    IParams.Add(APath + DirectorySeparator + INIT_MODULE);
    IParams.Add(GetFileName(APath, False));
    IncludeTemplate(IParams);
  end
  else
  begin
    FindAllFiles(Files, APath, '*.ultra.*;*.ultra', False);
    for F in Files do
    begin
    {{ TODO: implement module import }}
      IParams.Clear;
      IParams.Add(F);
      IncludeTemplate(IParams);
    {{ parei aqui }}
    end;
  end;
  Files.Free;
  IParams.Free;
end;

procedure TTemplate.IncludeTemplate(var Params: TStringList);
var
  ATemp: TTemplate;
  APair: TKVPair;
  IncName, TempAlias, ALine: string;
  i: integer;
  Dump: TGenFile;
  F: TUserFunction;
  Errorlocation:TErrorLocation;
begin
  ParseTokens([], Params);
  with ErrorLocation do
  begin
    LineNumber := FLineNumber;
    TempName := FFullName;
    Line := FLines[FLineNumber];
  end;
  if FileExists(Params[0]) then
    IncName := Params[0]
  else
    IncName := U_HOME+DirectorySeparator+BUILTIN_MODULES+DirectorySeparator+Params[0];
  //EFileError.Create(E_FILE_NOT_FOUND,ErrorLocation,IncName).TestFileExists.ERaise(False);
  if (Params.Count > 1) and (Params[1] <> '') then
    TempAlias := Params[1]
  else
    TempAlias := GetFileName(GetFileName(IncName, False), False);


  EAliasError.Create(E_INCLUDED_ALIAS_ALREADY_EXISTS,ErrorLocation,TempAlias).TestIncludedAliasExists(FIncludedAliases).ERaise(False);
  EAliasError.Create(E_FORBIDDEN_ALIAS_NAME,ErrorLocation,TempAlias).TestValidAliasName.ERaise(False);
  FIncludedAliases.Add(TempAlias);
  ATemp := TTemplate.Create(IncName, FExpLocation);
  ATemp.SetWebVars(FWebVars);
  if Params.Count > 2 then
  begin
    Params.Delete(0);
    Params.Delete(0);
    for i:=0 to Params.Count - 1 do
      ATemp.SetVariable('param['+IntToStr(i)+']',Params[i]);
  end;
  ATemp.ParseTemplate(FGenFileSet);
  for ALine in ATemp.ParsedLines do
    FParsed.Add(ALine);
  for APair in ATemp.Variables do
    SetVariable(TempAlias + ATTR_ACCESSOR + APair.Key, APair.Value);
  for F in ATemp.UserFunctions do
  begin
    i := Length(FUserFunctions);
    SetLength(FUserFunctions, i + 1);
    FUserFunctions[i].FunctionName := TempAlias + ATTR_ACCESSOR + F.FunctionName;
    FUserFunctions[i].HasReturn := F.HasReturn;
    FUserFunctions[i].Args := TStringList.Create;
    for ALine in F.Args do
      FUserFunctions[i].Args.Add(ALine);
    FUserFunctions[i].Lines := TStringList.Create;
    for ALine in F.Lines do
      FUserFunctions[i].Lines.Add(ALine);
  end;
  ATemp.Free;
end;

function TTemplate.Name: string;
begin
  Result := GetFileName(GetFileName(FFullName, False), False);
end;

procedure TTemplate.ExplodeStr(var Params: TStringList; var PureParams: TStringList);
var
  Explode: TStringList;
  i: integer;
  t: string;
begin
  Explode := TStringList.Create;
  Explode.SkipLastLineBreak := True;
  Explode.StrictDelimiter := True;
  ParseTokens([0, 2], Params);
  if Params.Count > 2 then
    Explode.Delimiter := Params[2][1]
  else
    Explode.Delimiter := PARAM_SEP;
  Explode.DelimitedText := Params[0];
  t := Params[0];
  if Params.Count = 4 then
  begin
    if PureParams[3] = ASC then
      Explode.Sort
    else if PureParams[3] = DESC then
    begin
      Explode.Sort;
      ReverseList(Explode);
    end;
  end;
  SetVariable(PureParams[1],Explode.Text);
  if Explode.Count > 0 then
  begin
    for i := 0 to Explode.Count - 1 do
    begin
      SetVariable(PureParams[1] + '[' + IntToStr(i) + ']', Trim(Explode[i]));
    end;
  end;
  Explode.Free;
end;

procedure TTemplate.ListFiles(var Params: TStringList; var PureParams: TStringList);
var
  APath: string;
  AVarName: string;
  AFilter: string;
  LookSub: boolean;
  ADelimiter: string;
  Files: TStringList;
  AParser: TTempParser;
  Sort: boolean = False;
  i: integer;
begin
  AFilter := '';
  LookSub := False;
  //ADelimiter := FILES_SECURE_SEP;
  //listFiles:path,vari,filter,sub,order
  ParseTokens([0, 2], Params);
  APath := Params[0];
  AVarName := PureParams[1];
  if Params.Count > 2 then
    AFilter := Params[2];
  if Params.Count > 3 then
    LookSub := StrToBoolean(PureParams[3])             ;
  Files := TStringList.Create;
  Files.SkipLastLineBreak := True;
  FindAllFiles(Files, APath, AFilter, LookSub);
  if Params.Count > 4 then
  begin
    if PureParams[4] = ASC then
      Files.Sort
    else if PureParams[4] = DESC then
    begin
      Files.Sort;
      ReverseList(Files);
    end;
  end;
  {$IFDEF Windows}
    Files.LineBreak := #13;
  {$ENDIF}
  SetVariable(AVarName, Files.Text);
  if Files.Count > 0 then
  begin
    for i := 0 to Files.Count - 1 do
      SetVariable(AVarName + '[' + IntToStr(i) + ']', Trim(Files[i]));
  end;
  Files.Free;
end;

procedure TTemplate.ListDirs(var Params: TStringList; var PureParams: TStringList);
var
  APath: string;
  AVarName: string;
  AFilter: string;
  LookSub: boolean;
  ADelimiter: string;
  Files: TStringList;
  AParser: TTempParser;
  Sort: boolean = False;
  i: integer;
begin
  AFilter := '';
  LookSub := False;
  //ADelimiter := FILES_SECURE_SEP;
  //listFiles:path,vari,filter,sub,order
  ParseTokens([0, 2], Params);
  APath := Params[0];
  AVarName := PureParams[1];
  if Params.Count > 2 then
    AFilter := Params[2];
  if Params.Count > 3 then
    LookSub := StrToBoolean(PureParams[3])             ;
  Files := TStringList.Create;
  Files.SkipLastLineBreak := True;
  //FindAllFiles(Files, APath, AFilter, LookSub);
  FindAllDirectories(Files, Apath, LookSub);
  if (Files.Count > 0) and (AFilter <> '') then
  begin
    for i:=Files.Count-1 downto 0 do
    begin
      if Pos(AFilter,Files[i]) = 0 then
        Files.Delete(i);
    end;
  end;
  if Params.Count > 4 then
  begin
    if PureParams[4] = ASC then
      Files.Sort
    else if PureParams[4] = DESC then
    begin
      Files.Sort;
      ReverseList(Files);
    end;
  end;
  {$IFDEF Windows}
    Files.LineBreak := #13;
  {$ENDIF}
  SetVariable(AVarName, Files.Text);
  if Files.Count > 0 then
  begin
    for i := 0 to Files.Count - 1 do
      SetVariable(AVarName + '[' + IntToStr(i) + ']', Files[i]);
  end;
  Files.Free;
end;

procedure TTemplate.LimitedListFiles(var Params: TStringList; var PureParams: TStringList);
var
  APath: string;
  AVarName: string;
  AFilter: string;
  LookSub: boolean;
  ADelimiter: string;
  Files: TStringList;
  Dump: string;
  AParser: TTempParser;
  Sort: boolean = False;
  i: integer;
  Start, Limit: integer;
begin
  AFilter := '';
  LookSub := False;
  //ADelimiter := FILES_SECURE_SEP;
  //listFiles:start,limit,path,vari,filter,sub,order
  Start := StrToInt(PureParams[0]);
  Limit := StrToInt(PureParams[1]);
  APath := Params[2];
  AVarName := PureParams[3];
  if Params.Count > 4 then
    AFilter := Params[4];
  if Params.Count > 5 then
    LookSub := StrToBoolean(PureParams[5]);
  Files := TStringList.Create;
  Files.SkipLastLineBreak := True;
  FindAllFiles(Files, APath, AFilter, LookSub);
  if Params.Count > 6 then
  begin
    if PureParams[6] = ASC then
      Files.Sort
    else if PureParams[6] = DESC then
    begin
      Files.Sort;
      ReverseList(Files);
    end;
  end;
  if Files.Count > 0 then
  begin
    for i := 0 to Files.Count - 1 do
      SetVariable(AVarName + '[' + IntToStr(i) + ']', Files[i]);
  end;
  SetVariable(AVarName, Files.Text);
  Files.Free;
end;

procedure TTemplate.LoadText(var Params: TStringList);
var
  AText: TStringList;
  i: integer;
begin
  AText := TStringList.Create;
  AText.SkipLastLineBreak := True;
  ParseTokens([0], Params);
  if FileExists(Params[0]) then
  begin
    AText.LoadFromFile(Params[0]);
    if AText.Count > 0 then
    begin
      SetVariable(Params[1], AText.Text);
      for i := 0 to AText.Count - 1 do
        SetVariable(Params[1] + '[' + IntToStr(i) + ']', AText[i]);
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
      {if (Pos(OVER_STATE, Trim(Line)) = 1) then
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
      end;   }
      FLines.Add(Part);
    end;
  end;
  Vars.Free;
  Temp.Free;
end;


procedure TTemplate.ForPrepare(var Params: TStringList; ForLoop: boolean = True);
var
  Values: TStringList;
  ParamAsStr, Iterated, j: string;
  len, i, x: int64;
  EmptyList: boolean;
begin
  ParseTokens([0, 2], Params);
  FLoopTypeLast := FLoopType;
  FLoopType := FORDEV;
  FForLevel := FForLevel + 1;
  SetLength(FForLoops, FForLevel + 1);
  FForLoops[FForLevel].ControlVar := Params[1];
  FForLoops[FForLevel].GoToLine := FLineNumber + 2;

  FForLoops[FForLevel].List := TStringList.Create;
  FForLoops[FForLevel].List.SkipLastLineBreak := True;
  if Params.Count < 3 then
  begin
    FForLoops[FForLevel].List.Delimiter := PARAM_SEP;
    FForLoops[FForLevel].List.StrictDelimiter := True;
    FForLoops[FForLevel].List.DelimitedText := Params[0];
  end
  else
  begin
    if Params[2] <> LINE_BREAK then
    begin
      FForLoops[FForLevel].List.Delimiter := Params[2][1];
      FForLoops[FForLevel].List.StrictDelimiter := True;
      Iterated := Params[0];
      FForLoops[FForLevel].List.DelimitedText := Iterated;

    end
    else if Params[2] = LINE_BREAK then
    begin
      //Params[0] := DropLastLineBreak(Params[0]);
      {$IFDEF WINDOWS}
      Params[0] := ReplaceStr(Params[0], #13#10, #10);
      FForLoops[FForLevel].List.Delimiter := #10;
      {$ENDIF}
      {$IFDEF UNIX}
      FForLoops[FForLevel].List.Delimiter := sLineBreak;
      {$ENDIF}
      FForLoops[FForLevel].List.StrictDelimiter := True;
      FForLoops[FForLevel].List.DelimitedText := Params[0];
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

  if (FForLoops[FForLevel].List.Count > 0) then
  begin
    FForLoops[FForLevel].Times := FForLoops[FForLevel].List.Count;
    SetVariable(FForLoops[FForLevel].ControlVar,
      FForLoops[FForLevel].List[FForLoops[FForLevel].List.Count -
      FForLoops[FForLevel].Times]);
    j := FForLoops[FForLevel].ControlVar + '.i';
    SetVariable(j, IntToStr(FForLoops[FForLevel].List.Count -
      FForLoops[FForLevel].Times));
    j := GetVariable(j);
    j := j;

  end
  else
  begin
    FForLoops[FForLevel].Times := 0;
    FForSkip := True;
  end;
  Rewind := False;
end;

procedure TTemplate.EndFor;
begin
  FLoopType := NODEV;
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
      FForLoops[FForLevel].List[FForLoops[FForLevel].List.Count -
      FForLoops[FForLevel].Times]);
    SetVariable(FForLoops[FForLevel].ControlVar + '.i', IntToStr(
      FForLoops[FForLevel].List.Count - FForLoops[FForLevel].Times));
  end;
  FForSkip := False;
end;

procedure TTemplate.BreakFor;
begin
  FForLoops[FForLevel].Times := 0;
  FForLoops[FForLevel].Limit := 1;
  FForLoops[FForLevel].Infinite := False;
  FForSkip := True;
end;

procedure TTemplate.ContinueFor;
begin
  FForSkip := True;
end;

procedure TTemplate.LoopPrepare(var Params: TStringList;
  var PureParams: TStringList; ForLoop: boolean = True);
var
  Values: TStringList;
  ParamAsStr, Iterated: string;
  len, i, Times, Pause: integer;
begin
  //loop:[times],[control var],[pause (ms)]
  //loop: 0 -- no control var
  //loop: 0, i -- no pause
  //loop: 0, i, 500 (infinite loop) -- valid call
  FLoopTypeLast := FLoopType;
  FLoopType := LOOPDEV;
  ParseTokens([0, 2], Params);
  Times := StrToInt(Params[0]);
  FForLevel := FForLevel + 1;
  SetLength(FForLoops, FForLevel + 1);
  FForLoops[FForLevel].ControlVar := '';
  FForLoops[FForLevel].PauseTime := 0;
  if Times = 0 then
    FForLoops[FForLevel].Infinite := True;
  if Params.Count = 2 then
    FForLoops[FForLevel].ControlVar := PureParams[1];
  if Params.Count = 3 then
  begin
    Pause := StrToInt(Params[2]);
    FForLoops[FForLevel].PauseTime := Pause;
    FForLoops[FForLevel].ControlVar := PureParams[1];
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
  FLoopType := NODEV;
  if FForLevel > -1 then
  begin
    FForLoops[FForLevel].Times := FForLoops[FForLevel].Times + 1;
    if FForLoops[FForLevel].Infinite then
      FForLoops[FForLevel].Limit :=
        FForLoops[FForLevel].Limit + FForLoops[FForLevel].Times + 1;
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
        SetVariable(FForLoops[FForLevel].ControlVar,
          IntToStr(FForLoops[FForLevel].Times));
      FRewind := True;
    end;

  end;
end;


procedure TTemplate.IfPrepare(var Params: TStringList; var PureParams: TStringList;
  IfNot: boolean);
var
  a, b, c: string;
  Logic: boolean;
  d: integer;

begin
  FLoopTypeLast := FLoopType;
  FLoopType := IFDEV;
  if IfLevel = Length(FIfTests) then
  begin
    ParseTokens([0], Params);
    a := Params[0];
    Logic := StrToBoolean(a);
    //FSkip := not ((Logic and (not IfNot)) or ((not Logic) and IfNot));   //teste passou
    FSkip := not Logic;
    d := FIfLevel + 1;
    SetLength(FIfTests, d);
    FIfTests[FIfLevel] := Logic; //test is true
  end;
end;

procedure TTemplate.ElseDecision;
begin
  //FSkip := not FSkip;
end;

procedure TTemplate.EndIf;
var
  len:integer;
begin
  len := FIfLevel + 1;
  SetLength(FIfTests, len);
  //FSkip := False;
end;

function TTemplate.Load(ATempName: string): TTemplate;
var
  Temp: TStringList;
  SectionLines: TStringList;
  Line, Part, WillAdd: string;
  SectionOpen: boolean = False;
  IsMulti:boolean;
begin
  Temp := TStringList.Create;
  Temp.SkipLastLineBreak := True;
  try
    Temp.LoadFromFile(ATempName);
    FLines.Clear;
    Clear;
    FFullname := ATempName;
    FOverrides.Extension := Copy(FFullName, RPos(EXT_SEP, FFullName), Length(FFullName));
    if FOverrides.Extension = '.ultra' then
      FScriptMode := True;
    WillAdd := '';
    // PreParse must be at this point
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
      else if (Pos(OVER_STATE + 'endSection', Trim(Line)) = 1) and
        (Length(Trim(Line)) = Length(OVER_STATE + 'endSection')) then
        SectionOpen := False
      else if SectionOpen then
        TStringList(FSections.Objects[FSections.IndexOf(Part)]).Add(Line)
      else
      begin
        IsMulti := Copy(Trim(Line),Length(Trim(Line))-2,3) = MULTI_LINE;
        if IsMulti then
          WillAdd := WillAdd + Copy(Line,1,Length(Line)-3)
        else
        begin
          WillAdd := WillAdd + Line;
          FLines.Add(WillAdd);
          WillAdd := '';
        end;
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
      else if (Pos(OVER_STATE + 'endSection', Trim(Line)) = 1) and
        (Length(Trim(Line)) = Length(OVER_STATE + 'endSection')) then
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
        Fileutil.CopyFile(FOutFilePath, FullOutFilePath);
      until FileExists(FullOutFilePath) or (Tries > MAX_TRIES);
    end;
  end;
  Result := Self;
end;

function TTemplate.SetPredefined(AKey, AValue: string): boolean;
var
  Return: boolean = True;
  Res:boolean;
  AParser: TTempParser;
  Params, PureParams: TStringList;
  i, PosMod: integer;
  a, b: string;
  Errorlocation:TErrorLocation;
  AExtension:TExtensionCaller;
begin

  with ErrorLocation do
  begin
    LineNumber := FLineNumber;
    TempName := FFullName;
    Line := FLines[FLineNumber];
  end;
  Params := TStringList.Create;
  Params.SkipLastLineBreak := True;
  AParser := TTempParser.Create(Self);
  AParser.ParseParams(AValue, Params);
  PureParams := TStringList.Create;
  PureParams.SkipLastLineBreak := True;
  PureParams.AddStrings(Params);
  {if Params.Count > 0 then
  begin
    for i := 0 to Params.Count - 1 do
    begin
      a := AParser.ParseToken(Params[i]);
      Params[i] := a;
    end;
  end;}
  AKey := Trim(AKey);
  PosMod := Pos(EXTENSION_CALL, AKey);
  if PosMod = 0 then
  begin

    case (AKey) of
      'outFileName': FOverrides.OutFileName := AParser.ParseToken(Params[0]);
      'copyTo': FOverrides.CopyTo.Add(RemoveLastBackslash(AParser.ParseToken(Params[0])));
      'exportTo': FExpLocation := AParser.ParseToken(Params[0]);
      'extension': FOverrides.Extension := AParser.ParseToken(Params[0]);
      'overwrite': FOverrides.Overwrite := True;
      'exportAtRoot': FOverrides.ExpAtRoot := True;
      //'import': ImportGenFile(Params);
      'dontSave' : FCanSave := False;
      'include': IncludeTemplate(Params);
      'import':ImportModule(Params);
      'for': ForPrepare(Params);
      'endFor': EndFor;
      'break': BreakFor;
      'continue': ContinueFor;
      'loop': LoopPrepare(Params, PureParams);
      'endLoop': EndLoop;
      'if': IfPrepare(Params, PureParams, False);
      'ifNot': IfPrepare(Params, PureParams, True);
      'elseIf': IfPrepare(Params, PureParams, False);
      'elseIfNot': IfPrepare(Params, PureParams, True);
      'else': ElseDecision;
      'endIf': EndIf;
      'explode': ExplodeStr(Params, PureParams);
      'print': PrintLine(Params, True, False);
      'inline': PrintLine(Params, True, False, True);
      'tee': PrintLine(Params, True, True);
      'live',
      'livePrint': PrintLine(Params, False, True);
      'processTemplate': ProcessTemplate(Params);
      'clear': clrscr;
      'tokenEnclosers':
      begin
        FTokenOpen := Params[0][1];
        FTokenClose := Params[0][2];
      end;
      'input': InputValue(Params, False);
      //'live': FCanSave := False;
      'parsedInput': InputValue(Params, True);
      'execute': Execute(Params, False);
      'execSilent': Execute(Params, False,True);
      'trigger': Execute(Params, True);
      'drop': DropVariable(PureParams[0]);
      'loadText': LoadText(Params);
      //fileHandling
      'listFiles': ListFiles(Params, PureParams);
      'listDirs': ListDirs(Params, PureParams);
      'limitedListFiles': LimitedListFiles(Params, PureParams);
      'move': Move(Params);
      'copy': TempFileCopy(Params);
      'del' :
      begin
        EFileError.Create(E_FILE_NOT_FOUND,ErrorLocation,Params[0]).TestFileExists.ERaise(False);
        SysUtils.DeleteFile(Params[0]);
      end;

      'mkdir':
      begin
        if Params.Count = 1 then
          CreateDirTree(Params[0], False)
        else if (Params.Count = 2) then
        begin
          if (PureParams[1] = LANG_TRUE) then
            CreateDirTree(Params[0], True)
          else
            CreateDirTree(Params[0], True);
        end;
      end;
      'rmdir':
      begin
        if Params.Count = 1 then
          RemoveDir(Params[0])
        else if (Params.Count = 2) then
        begin
          if (PureParams[1] = LANG_TRUE) then
            Res := DeleteDirectory(Params[0], False)
          else
            RemoveDir(Params[0]);
        end;
      end;
      //end filehandling
      'renderBlank': FOverrides.RenderBlank := True;
      'pause': DoPause(Params);
      // Gen operations
      'setGenDefaultSeparator' : begin
        ParseTokens([], Params);
        VariablesGlobals.GEN_SEP := Params[0][1]
      end;
      'setGenAlias' : ChangeGenAlias(Params);
      'setValue': SetGenValue(Params);
      'saveGen': SaveGen(Params);
      'setGenName': SetGenName(Params);
      'createGen': CreateGen(Params);
      'captureGen': CaptureGen(Params);
      'unloadGen': UnloadGen(Params);
      'loadGenFolder': LoadGenFolder(Params);
      'mapGenKeys': MapGenKeys(Params, True);
      'dropGenMap': MapGenKeys(Params, False);
      'groupKeys': GroupKeys(Params);
      'lockGenPairs' :
      begin
        ParseTokens([], Params);
        FGenFileSet.GenFiles[FGenFileSet.IndexOf(Params[0])].GenFile.PairsLocked := StrToBoolean(Params[1]);
      end;
      'lockGenKeys' : FGenFileSet.GenFiles[FGenFileSet.IndexOf(Params[0])].GenFile.KeysLocked := StrToBoolean(PureParams[1]);
      // End of Gen operations
      //textsave functions
      'log': LogText(Params);
      //end textsave
      //start web operations
      'abort': ParseAbort(Params);
      'redirect',
      'goTo': RedirectTo(Params);
      'createSession': CreateSession(Params);
      'destroySession': DestroySession(Params);
      'setSessionVar': SetSessionVar(Params);
      'dropSessionVar': DropSessionVar(Params);
      'setCookie': SetCookie(Params);
      'setGenCookie': SetRawCookie(Params);
      'dropCookie': DropCookie(Params);
      'parseJson': ParseJson(Params);
      'sendGet': RequestRest(Params, PureParams);
      'responseType': FWebVars.Response.ContentType := Params[0];
      'responseCode':
      begin
        FWebVars.Response.Code := StrToInt(Params[0]);
        if Params.Count > 1 then
          FWebVars.Response.CodeText := Params[1];
      end;
      // end web operations
      //queues opers
      'createQueue': CreateQueue(Params, PureParams);
      'queue':QueueTask(Params);
      'stopQueue': DestroyQueue(Params);
      'startQueue': ActivateQueue(Params);
      //end queues
      //user functions
      'function': StartFunction(Params, PureParams, True);
      'endFunction': EndFunction;
      'proc': StartFunction(Params, PureParams, False);
      '_', 'return': FunctionReturn(Params);
      'endProc': EndFunction;
      //initializers
      'initRand' : InitializeVars(Params);
      'init' : InitializeVars(Params, True);
      //end init
      'POC' : POC(PureParams, Params);
      'callProc':
      begin
        ParseTokens([0], Params);
        a := Params[0];
        PureParams.Delete(0);
        PureParams.LineBreak := ',';
        b := PureParams.Text;
        Result := SetPredefined(a, b);
      end
      else
      begin
        if True then
        begin
          if Length(FUserFunctions) > 0 then
            a := FUserFunctions[0].FunctionName;
          ExecuteFunction(AKey, False, Params);
        end
        else
          Return := False;
      end;
        //end user functions
    end;

  end
  else
  begin
    //is calling from a Extension
    AExtension := TExtensionCaller.Create(Copy(AKey, 1, PosMod-1), Copy(AKey, PosMod+1, Length(AKey)), PureParams, Params, Self);
    AExtension.ExecProc;
    AExtension.Free;
  end;
  PureParams.Free;
  Params.Free;
  Result := Return;
end;

procedure TTemplate.StartFunction(var Params: TStringList;
  var PureParams: TStringList; HasRet: boolean);
//Params[0] = [funcname]
//Params[n] ... variables
var
  len: integer;
begin
  len := Length(FUserFunctions);
  SetLength(FUserFunctions, len + 1);
  FUserFunctions[len].FunctionName := PureParams[0];
  FUserFunctions[len].Args := TStringList.Create;
  FUserFunctions[len].Lines := TStringList.Create;
  FUserFunctions[len].Lines.SkipLastLineBreak := True;
  FUserFunctions[len].Args.AddStrings(PureParams);
  FUserFunctions[len].Args.Delete(0);
  FUserFunctions[len].HasReturn := HasRet;
  FAddToFunction := True;
end;

procedure TTemplate.MakeFunctionsRoom;
begin
  SetLength(FUserFunctions,Length(FUserFunctions)+1);
end;

procedure TTemplate.AddLineToFunction(ALine: string);
begin
  if (ReplaceStr(ALine, OVER_STATE, '') = 'endFunction') or
    (ReplaceStr(ALine, OVER_STATE, '') = 'endProc') then
  begin
    FAddToFunction := False;
  end
  else
    FUserFunctions[Length(FUserFunctions) - 1].Lines.Add(ALine);
end;

procedure TTemplate.FunctionReturn(var Params: TStringList);
begin
  FOrderReturn := True;
  ParseTokens([0], Params);
  FReturnValue := Params[0];
end;

procedure TTemplate.EndFunction;
begin
  FUserFunctions[Length(FUserFunctions) - 1].Lines.Delete(
    FuserFunctions[Length(FUserFunctions) - 1].Lines.Count - 1
    );
  FAddToFunction := False;
end;

function TTemplate.MapElem(var Params:TStringList; var PureParams:TStringList):string;
var
  Ret:string='';
  AListable:string;
  ASep, ARetSep:string;
  AFunc,z:string;
  Mapped:TStringList;
  i:integer;
  AParser:TTempParser;
begin
  //Params[0] = a listable
  //Params[1] = a separator
  //Params[2] = ret separator
  //PureParams[3] = an arrow function

  AListable := Params[0];
  Params.Delete(0);
  if Params.Count = 3 then
  begin
    ASep := Params[0];
    Params.Delete(0);
    ARetSep := Params[0];
    if ARetSep = LINE_BREAK then
      ARetSep := sLineBreak;
    AFunc := PureParams[3];
  end
  else if Params.Count = 2 then
  begin
    ASep := Params[0];
    ARetSep := Params[0];
    if ARetSep = LINE_BREAK then
      ARetSep := sLineBreak;
    AFunc := PureParams[2];
  end
  else
  begin
    ASep := PARAM_SEP;
    ARetSep := PARAM_SEP;
    AFunc := PureParams[1];
  end;



  Mapped := TStringList.Create;
  Mapped.SkipLastLineBreak := True;
  if ASep <> LINE_BREAK then
  begin
    Mapped.StrictDelimiter := True;      
    Mapped.Delimiter := ASep[1];
    Mapped.DelimitedText := AListable;
  end
  else
  begin
    Mapped.Text := AListable;
  end;
  if Mapped.Count > 0 then
  begin
    AParser := TTempParser.Create(Self);
    for i:=0 to Mapped.Count - 1 do
    begin
      SetVariable('elem',Mapped[i]);
      SetVariable('elem.i',IntToStr(i));
      //z := AParser.ParseFunction(AFuncName,Params,Params);
      z := AParser.ParseToken(AFunc);
      Ret := Ret + z;
      //Params.Delete(Params.Count-1);
      if i < Mapped.Count-1 then
        Ret := Ret + ARetSep;
    end;
    AParser.Free;
    DropVariable('elem');
    DropVariable('elem.i');
  end;
  Result := Ret;
end;

function TTemplate.ExecuteFunction(FuncName: string; HasRet: boolean;
  var Params: TStringList): string;
var
  ATemplate: TTemplate;
  Len, i, j, DotPos: integer;
  Return, Line, a, b: string;
  AGenSet:TGenFileSet;
begin
  if not HasRet then
    ParseTokens([], Params);
  Len := Length(FUserFunctions);
  if Len > 0 then
  begin
    for i := 0 to Len - 1 do
    begin
      if (FUserFunctions[i].FunctionName = FuncName) and
        (FUserFunctions[i].HasReturn = HasRet) then
      begin
        ATemplate := TTemplate.Create;
        ATemplate.FullName := FuncName;
        ATemplate.TempLines.AddStrings(FUserFunctions[i].Lines);
        if FUserFunctions[i].Args.Count > 0 then
        begin
          for j := 0 to Params.Count - 1 do
          begin
            try
              ATemplate.SetVariable(FUserFunctions[i].Args[j], Params[j]);
            except

            end;
          end;
        end;
        ATemplate.SetFunctionsLength(Len);
        for j := 0 to Len - 1 do
        begin
          DotPos := Pos(ATTR_ACCESSOR, FuncName);
          if (DotPos > 0) then
          begin
            a := Copy(FUserFunctions[j].FunctionName, 1, DotPos);
            b := Copy(FuncName, 1, DotPos);
            if a = b then
              ATemplate.UserFunctions[j].FunctionName :=
                Copy(FUserFunctions[j].FunctionName, DotPos + 1, Length(FUserFunctions[j].FunctionName))
            else
              ATemplate.UserFunctions[j].FunctionName := FUserFunctions[j].FunctionName;

          end
          else
            ATemplate.UserFunctions[j].FunctionName := FUserFunctions[j].FunctionName;
          ATemplate.UserFunctions[j].HasReturn := FUserFunctions[j].HasReturn;
          ATemplate.UserFunctions[j].Lines := TStringList.Create;
          ATemplate.UserFunctions[j].Args := TStringList.Create;
          ATemplate.UserFunctions[j].Lines.AddStrings(FUserFunctions[j].Lines);
          Line := ATemplate.UserFunctions[j].Lines.Text;
          ATemplate.UserFunctions[j].Args.AddStrings(FUserFunctions[j].Args);
          Line := ATemplate.UserFunctions[j].Args.Text;
        end;
        ATemplate.ScriptMode := True;
        ATemplate.SetWebVars(FWebVars);
        // AGenSet := TGenFileSet.Create;
        // FGenFileSet.CopyGenSet(AGenSet);

        Return := ATemplate.ParseTemplate(FGenFileSet, FParsed);
        FParsed.AddStrings(ATemplate.ParsedLines);
        ATemplate.ScriptMode := False;
        ATemplate.Free;
        break;
      end;
    end;

  end;
  Result := Return;
end;

procedure TTemplate.DoPause(var Params: TStringList);
var
  i: longint;
begin
  ParseTokens([], Params);
  if Params[0] = '' then
    ReadLn
  else
  begin
    i := StrToInt(Params[0]);
    if Params.Count = 2 then
    begin
      if Params[1] = 'h' then
        i := i * 1000 * 60 * 60
      else if Params[1] = 'm' then
        i := i * 1000 * 60
      else if Params[1] = 's' then
        i := i * 1000;
    end;
    Sleep(i);
  end;
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
    WriteLn(Line);
  end;
end;

function TTemplate.GetVariable(AVarName: string): string;
var
  Line: TKVPair;
  Return: string = '';
  Found:boolean = False;
  Errorlocation:TErrorLocation;
begin

  with ErrorLocation do
  begin
    LineNumber := FLineNumber;
    TempName := FFullName;
    Line := FLines[FLineNumber];
  end;
  for Line in FVariables do
  begin
    if Line.Key = AVarName then
    begin
      Found := True;
      Return := Line.Value;
      Break;
    end;
  end;
  if not Found then
  begin
    EVariableError.Create(E_VAR_NOT_EXIST,ErrorLocation,AVarName).ERaise;
  end;
  Result := Return;
end;

function TTemplate.GetWild(ASearch, AnAlias: string): string;
var
  i: integer;
  ADefault: string = '';
begin
  i := FGenFileSet.IndexOf(AnAlias);
  if i > -1 then
    ADefault := FGenFileSet.GenFiles[i].GenFile.IfNotFound;
  Result := GetWild(ASearch, AnAlias, ADefault);
end;

function TTemplate.RouteMatch(ASearch, AnAlias: string; ADefault: string = ''): string;
var
  i, j: integer;
  Exp, Inp: TStringList;
  APair: TKVPair;
  Match: boolean = False;
  Return: string;
begin
  // :int - for match with integers
  // :str - for match with strings
  // :any - for any word
  // :bool - for match with "true" or "false" (:str will not match it. true and false will be treated as reserved words)
  // :strbool - match any strings and does not treat true and false as reserved words

  i := FGenFileSet.IndexOf(AnAlias);
  Return := ADefault;
  if i > -1 then
  begin
    Inp := TStringList.Create;
    Inp.SkipLastLineBreak := True;
    Inp.Delimiter := '/';
    Inp.StrictDelimiter := True;
    Inp.DelimitedText := ASearch;
    Exp := TStringList.Create;
    Exp.SkipLastLineBreak := True;
    Exp.Delimiter := '/';
    Exp.StrictDelimiter := True;
    for APair in FGenFileSet.GenFiles[i].GenFile.Pairs do
    begin
      Exp.DelimitedText := APair.Key;
      if Exp.Count = Inp.Count then
      begin
        for j := 0 to Exp.Count - 1 do
        begin
          Match := False;
          if Exp[j] = Inp[j] then
          begin
            Match := True;
          end
          else if Exp[j] = ':int' then
          begin
            try
              StrToInt(Inp[j]);
              Match := True
            except
              Match := False;
            end;
          end
          else if Exp[j] = ':str' then
          begin
            try
              StrToInt(Inp[j]);
              Match := False
            except
              Match := True;
            end;
          end
          else if Exp[j] = ':any' then
            Match := True;
          if Match = False then
            break;
        end;
      end;
      if Match = True then
      begin
        Return := APair.Value;
        break;
      end;
    end;
    Exp.Free;
    Inp.Free;
    Result := Return;
  end;
end;

function TTemplate.GetWild(ASearch, AnAlias, ADefault: string): string;
var
  Jump, i, j: integer;
  Pairs: array of TKVPair;
  Temp, Right, AWild, Ret: string;
  WildLeft, WildRight, Match: boolean;
begin
  Ret := ADefault;

  i := FGenFileSet.IndexOf(AnAlias);
  if i > -1 then
  begin
    Pairs := FGenFileSet.GenFiles[i].GenFile.Pairs;
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
    end;
  end;
  Result := Ret;
end;

procedure TTemplate.SetGenName(var Params: TStringList);
var
  i: integer;
begin
  ParseTokens([], Params);
  i := FGenFileSet.IndexOf(Params[0]);
  if i > -1 then
    FGenFileSet.GenFiles[i].GenFile.FullName := Params[1];
end;

function TTemplate.SetVariable(AKey, AValue: string; Parse: boolean = False; IsConst:boolean = False): TTemplate;
var
  i, len, posDot: integer;
  AParser: TTempParser;
  GenValue, ValidVar: boolean;
  GenAliasPos, AttrAccPos: integer;
  AnAlias: string;
  Errorlocation:TErrorLocation;
begin

  with ErrorLocation do
  begin
    LineNumber := FLineNumber;
    TempName := FFullName;
    Line := FLines[FLineNumber];
  end;
  AParser := TTempParser.Create(Self);
  if Parse then
    AValue := AParser.ParseToken(AValue);
  GenAliasPos := Pos(FROM_GEN_SET, AKey);
  AttrAccPos := Pos(ATTR_ACCESSOR, AKey);
  GenValue := (GenAliasPos = 1) and (AttrAccPos > GenAliasPos + 1);
  ValidVar := (GenAliasPos = 0) and (AttrAccPos <> 1);
  len := Length(FVariables);
  if ValidVar then
  begin
    if Parse then
    begin
      if AttrAccPos = 0 then
        EVariableError.Create(E_FORBBIDEN_VAR_NAME,ErrorLocation,AKey).TestValidName.ERaise(False)
      else
        EVariableError.Create(E_FORBBIDEN_VAR_NAME,ErrorLocation,Copy(AKey,RPos('.',AKey)+1,Length(Akey))).TestValidName.ERaise(False);
    end;
    if len > 0 then
    begin
      for i := 0 to len - 1 do
      begin
        if FVariables[i].Key = AKey then
        begin
          FVariables[i].Value := AValue;
          Result := Self;
          Exit;
        end;
      end;
    end;
    SetLength(FVariables, len + 1);
    FVariables[len].Constant := IsConst;
    FVariables[len].Key := AKey;
    FVariables[len].Value := AValue;
  end
  else if GenValue then
  begin
    AnAlias := Copy(AKey, 2, AttrAccPos - 2);
    EAliasError.Create(E_FORBIDDEN_ALIAS_NAME,ErrorLocation,AnAlias).TestValidAliasName.ERaise(False);
    AKey := Copy(AKey, AttrAccPos + 1, Length(AKey));
    EGenError.Create(E_FORBIDDEN_KEY_NAME,ErrorLocation,AKey,AnAlias,-1).TestValidKeyName.ERaise(False);
    i := FGenFileSet.IndexOf(AnAlias);
    if i = -1 then
      i := FGenFileSet.Add(True, AnAlias);
    FGenFileSet.GenFiles[i].GenFile.SetValue(AKey, AValue);
  end
  else
    EVariableError.Create(E_FORBBIDEN_VAR_NAME,ErrorLocation,AKey).TestValidName.ERaise(False);

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
  var OutputParsed: TStringList): string;
var
  AParser: TTempParser;
  Return: string;
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
    SetLength(FIfTests, 0);
    FIfLevel := -1;
    FSkip := False;
    FRewind := False;

    AParser := TTempParser.Create(Self);
    Return := AParser.ParseTemplate(OutputParsed);
    AParser.Free;
    Result := Return;
  end
  else
  begin
    WriteLn('ERROR: No template path was provided');
    Exit;
  end;
end;

function TTemplate.ParseTemplate(var AGen: TGenFileSet): string;
begin
  FParsed.Clear;
  Result := ParseTemplate(AGen, FParsed);
end;

function TTemplate.ParseTemplate(var AGen: TGenFile;
  var OutputParsed: TStringList): string;
var
  AParser: TTempParser;
  Return: string;
begin
  if FFullName <> '' then
  begin
    FGenFile := AGen;
    FOverrides.OutFileName := AGen.OnlyName;
    FForGoto := -1;
    FForTimes := 0;
    SetLength(FForLoops, 0);
    FForLevel := -1;
    FIfLevel := -1;
    FRewind := False;
    FOrderReturn := False;
    AParser := TTempParser.Create(Self);
    Return := AParser.ParseTemplate(OutputParsed);
    AParser.Free;
    Result := Return;
  end
  else
  begin
    WriteLn('ERROR: No template path was provided');
    Exit;
  end;
end;

function TTemplate.ParseTemplate(var AGen: TGenFile): string;
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
      AKey := Copy(FOverrides.Filters[i],1,Pos(OVER_PARAM,FOverrides.Filters[i])-1);
      ATarget := Copy(FOverrides.Filters[i],Pos(OVER_PARAM,FOverrides.Filters[i])+1,Length(FOverrides.Filters[i]));
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

function TTemplate.EvalStrict: boolean;
var
  i: integer;
  AKey, AValue, ATarget: string;
  Return: boolean = True;
begin
  if FOverrides.Stricts.Count > 0 then
  begin
    Return := False;
    for i := 0 to FOverrides.Stricts.Count - 1 do
    begin
      AKey := FOverrides.Stricts.Names[i];
      ATarget := FOverrides.Stricts.Values[AKey];
      AKey := Copy(FOverrides.Stricts[i],1,Pos(OVER_PARAM,FOverrides.Stricts[i])-1);
      ATarget := Copy(FOverrides.Stricts[i],Pos(OVER_PARAM,FOverrides.Stricts[i])+1,Length(FOverrides.Stricts[i]));
      AValue := FGenFileSet.GetValue(AKey).Value;
      if Pos(ATarget, AValue) > 0 then
      begin
        Return := True;
      end
      else
      begin
        Return := False;
        break;
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
      AKey := Copy(FOverrides.Bypasses[i],1,Pos(OVER_PARAM,FOverrides.Bypasses[i])-1);
      ATarget := Copy(FOverrides.Bypasses[i],Pos(OVER_PARAM,FOverrides.Bypasses[i])+1,Length(FOverrides.Bypasses[i]));
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
  FOverrides.Stricts.Free;
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
  //FWebVars.Request.Free;
  FIncludedAliases.Free;
  FSections.Free;
  FImported.Free;
  FLines.Free;
  FParsed.Free;
end;

end.
