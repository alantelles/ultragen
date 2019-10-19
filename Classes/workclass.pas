unit WorkClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, FileUtil,

  { Globals }
  ConstantsGlobals, VariablesGlobals,

  { Classes }
  TemplateClass, GenFileClass, GenFileSetClass;

const
  PLUGIN = 'plugin';
  TEMPLATE = 'template';
  RELOAD = 'reload';
  GENLIST = 'GENLIST';

  { works modes }
  GENSET_TEMPLATE = 'genset_against_one_or_many_templates';

type
  TWork = class
  private
    FGenPath, FWorkType, FParam: string;
    FGensLoaded: TStringList;
    FGen: TGenFile;
    FGenSet: TGenFileSet;
    FLive: boolean;
    FExpLoc: string;
    FParams: TStringList;
    FDefault:string;
  public
    constructor Create(AGenPath: string; AExpLoc: string = '.');
    constructor Create(IsGenFile: boolean; AGenDelimited: string; AExpLoc: string = '.');
    //gen set mode work creator
    constructor Create(var AGenSet:TStringList; AExpLoc:string = '.');
    property Live:boolean read FLive write FLive;
    property GenPath: string read FGenPath write FGenPath;
    property GenSet: TGenFileSet read FGenSet write FGenSet;
    property GensLoaded: TStringList read FGensLoaded write FGensLoaded;
    property Params: TStringList read FParams;
    property DefaultValue:string read FDefault write FDefault;
    procedure Log(AText:string);
    procedure SetWork(ALine: string);
    procedure DoWork(AType:string;var AGenSet:TStringList; var ATemplates:TStringList; GenDefault:string);
    procedure Execute;
    procedure RunPlugin;
    procedure RunLoad;
    procedure RunLoad(AGenDelimited: string);
    procedure RunTemplate;
    procedure ListParams(AParamStr: string);
    destructor Destroy; override;
  end;

implementation

//gen path mode creator
constructor TWork.Create(AGenPath: string; AExpLoc: string = '.');
begin
  FGensLoaded := TStringList.Create;
  FParams := TStringList.Create;
  FGenPath := AGenPath;
  FExpLoc := AExpLoc;
  FGen := TGenFile.Create;
  RunLoad;
end;

//Gen Set mode work creator
constructor TWork.Create(var AGenSet:TStringList; AExpLoc:string = '.');
begin
  FGensLoaded := TStringList.Create;
  FParams := TStringList.Create;
  FExpLoc := AExpLoc;
  FGen := TGenFile.Create;
  FGenSet := TGenFileSet.Create;
  FDefault := DEF_IF_NOT;
end;

constructor TWork.Create(IsGenFile: boolean; AGenDelimited: string; AExpLoc: string = '.');
begin
  FGensLoaded := TStringList.Create;
  FParams := TStringList.Create;
  FExpLoc := AExpLoc;
  FGen := TGenFile.Create;
  RunLoad(AGenDelimited);
end;

procedure TWork.Log(AText:string);
begin
  if not FLive then
    WriteLn(AText);
end;

procedure TWork.RunLoad;
var
  F: string;
  Files, GenLoad: TStringList;
  i: integer;
begin
  //Log('Loading gens...');
  if FGensLoaded.Count > 0 then
  begin
    for i := 0 to FGensLoaded.Count - 1 do
    begin
      FGensLoaded.Objects[i].Free;
    end;
    FGensLoaded.Clear;
  end;
  Files := TStringList.Create;
  FindAllFiles(Files, FGenPath, '*.gen;*.GEN');
  for F in Files do
  begin
    GenLoad := TStringList.Create;
    //Log('Loading gen: '+F);
    GenLoad.LoadFromFile(F);
    FGensLoaded.AddObject(F, GenLoad);
  end;
  Files.Free;
end;

procedure TWork.RunLoad(AGenDelimited: string);
var
  GenLoad,GenList:TStringList;
  AGenName:string;
  i:integer;
begin
  //Log('Loading gens...');
  if FGensLoaded.Count > 0 then
  begin
    for i := 0 to FGensLoaded.Count - 1 do
    begin
      FGensLoaded.Objects[i].Free;
    end;
    FGensLoaded.Clear;
  end;
  GenList :=TStringList.Create;
  GenList.Delimiter:=GEN_FILES_CALL_SEP;
  GenList.StrictDelimiter := True;
  GenList.DelimitedText := AGenDelimited;
  for AGenName in GenList do
  begin
    GenLoad := TStringList.Create;
    //Log('Loading gen: '+Trim(AGenName));
    GenLoad.LoadFromFile(Trim(AGenName));
    FGensLoaded.AddObject(Trim(AGenName), GenLoad);
  end;
  GenList.Free;
end;

procedure TWork.ListParams(AParamStr: string);
const
  SP = ' ';
  STR = '"';
  ANY = '';
var
  i: integer;
  Part: string;
  StrOpen: boolean = False;
begin
  AParamStr := Trim(AParamStr);
  FParams.Clear;
  Part := ANY;
  for i := 1 to Length(AParamStr) do
  begin
    //zika "teste\zika thomas reyahi" Jones
    if (not StrOpen) and (AParamStr[i] = SP) then
    begin
      FParams.Add(Part);
      Part := '';
    end
    else if AParamStr[i] = STR then
      StrOpen := not StrOpen
    else
      Part := Part + AParamStr[i];
  end;
  FParams.Add(Part);
end;

procedure TWork.SetWork(ALine: string);
var
  SPos: integer;
  AParams: string;
begin
  ALine := Trim(ALine);
  SPos := Pos(' ', ALine);
  if SPos > 0 then
  begin
    FWorkType := Copy(ALine, 1, SPos - 1);
    AParams := Copy(ALine, SPos + 1, Length(ALine));
    ListParams(AParams);
  end
  else
    FWorkType := ALine;
  Execute;
end;

procedure TWork.DoWork(AType:string; var AGenSet:TStringList; var ATemplates:TStringList; GenDefault:string);
var
  AGenSetObj:TGenFileSet;
  ATemp:TTemplate;
  S:string;
begin
  AGenSetObj := TGenFileSet.Create;
  AGenSetObj.Enlist(AGenSet);
  AGenSetObj.SetDefault(GenDefault);
  ATemp := TTemplate.Create;
  for S in ATemplates do
  begin
    ATemp.Clear;
    ATemp.ExportLocation := FExpLoc;
    ATemp.Load(S);
    ATemp.ParseTemplate(AGenSetObj);
    if not Live then
      ATemp.Save
    else
      ATemp.PrintParsed;
  end;
  ATemp.Free;
  AGenSetObj.Free;
end;

procedure TWork.Execute;
begin
  if FWorkType = PLUGIN then
    RunPlugin
  else if FWorkType = RELOAD then
    RunLoad
  else if FWorkType = TEMPLATE then
    RunTemplate
  else
    Log('Unknown process');
end;

procedure TWork.RunTemplate;
var
  ATemplate: TTemplate;
  i: integer;
begin
  Log('Processing template '+ FParams[0]);
  ATemplate := TTemplate.Create(FParams[0], FExpLoc);
  if FGensLoaded.Count > 0 then
  begin
    for i := 0 to FGensLoaded.Count - 1 do
    begin
      FGen.Load(TStringList(FGensLoaded.Objects[i]), FGensLoaded[i]);
      ATemplate.ParseTemplate(FGen);
      if Live then
      begin
        ATemplate.PrintParsed
      end
      else
        ATemplate.Save;
      ATemplate.Clear;
    end;
  end;
  ATemplate.Free;
end;

procedure TWork.RunPlugin;
var
  AProcess: TProcess;
begin
  Log('Processing plugin '+ FParam);
  AProcess := TProcess.Create(nil);
  AProcess.Executable := 'Plugins' + DirectorySeparator + FParam;
  Aprocess.Parameters.Add(FGenPath);
  AProcess.Options := AProcess.Options + [poNoConsole, poWaitOnExit];
  AProcess.Execute;
  AProcess.Free;
end;

destructor TWork.Destroy;
var
  i: integer;
begin
  if FGensLoaded.Count > 0 then
  begin
    for i := 0 to FGensLoaded.Count - 1 do
    begin
      FGensLoaded.Objects[i].Free;
    end;
  end;
  FGensLoaded.Free;
  FParams.Free;
  FGen.Free;
  FGenSet.Free;
end;

end.
