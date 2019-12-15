unit GenFileSetClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,StrUtils,

  { Classes }
  GenFileClass,

  { Utils }
  FileHandlingUtils,

  { Globals }
  VariablesGlobals, ConstantsGlobals, TypesGlobals;

type
  TGenFileRecord = record
    GenFile:TGenFile;
    GenAlias:string;
  end;

  TGenFileArray = array of TGenFileRecord;

  TGenFileSet = class
    private
      FGenFiles:TGenFileArray;
      FDefault:string;
    public
      property GenFiles:TGenFileArray read FGenFiles write FGenFiles;
      property IfNotFound:string read FDefault write FDefault;
      constructor Create;
      procedure Enlist(AGenFiles:array of string);
      procedure Enlist(var AGenFiles:TStringList);
      procedure Add(AGenFile:string);
      procedure Add(AGenFile, ASeparator:string);
      procedure Add(AGenFile, ASeparator, ADefault:string);
      procedure Add(var AGenFile:TGenFile; AnAlias:string);
      procedure SetDefault(AValue:String);
      function GetValue(AKey:string):TParseResult;
      function GetValue(AKey,ADefault:string):TParseResult;
      function IndexOf(AnAlias:string):integer;
      destructor Destroy; override;
  end;

implementation

constructor TGenFileSet.Create;
begin
  SetLength(FGenFiles,0);
end;

procedure TGenFileSet.Enlist(AGenFiles:array of string);
var
  iter:string;
begin
  for iter in AGenFiles do
    Add(iter);
end;
procedure TGenFileSet.SetDefault(AValue:String);
var
  i:integer;
begin
  if Length(FGenFiles) > 0 then
    for i:=0 to Length(FGenFiles)-1 do
      FGenFiles[i].GenFile.IfNotFound := AValue;
  FDefault := AValue;
end;

procedure TGenFileSet.Enlist(var AGenFiles:TStringList);
var
  iter:string;
  PosSep,PosDef:integer;
  Temp,Def,Sep:string;
begin
  for iter in AGenFiles do
  begin
    Sep := GEN_SEP;
    Temp := iter;
    posSep := Pos(SEPARATOR_CHANGE,Temp);
    if PosSep > 0 then
    begin
      Sep := Temp[PosSep+Length(SEPARATOR_CHANGE)];
      Temp := ReplaceStr(Temp,SEPARATOR_CHANGE+Sep,'');
    end;
    Add(Trim(Temp),Sep,Def);
  end;
end;

function TGenFileSet.IndexOf(AnAlias:string):integer;
var
  i:integer=-1;
  Return:integer=-1;
  ARec:TGenFileRecord;
begin
  for ARec in FGenFiles do
  begin
    i := i+1;
    if ARec.GenAlias = AnAlias then
    begin
      Return := i;
      Break;
    end;
  end;
  Result := Return;
end;

function TGenFileSet.GetValue(AKey:string):TParseResult;
var
  DotPos,SetPos,i:integer;
  AnAlias:string;
  Return:TParseResult;
  ARec:TGenFileRecord;
  test:boolean;
begin
  AKey := Trim(AKey);
  if Pos(FROM_GEN_SET,AKey) = 1 then
    AKey := Copy(AKey,Length(FROM_GEN_SET)+1,Length(AKey));
  DotPos := Pos(ATTR_ACCESSOR,AKey);
  AnAlias := '';
  Return.Value := FDefault;
  Return.ParseOk := True;
  if DotPos = 1 then
    AKey := Copy(AKey,2,Length(AKey))
  else if DotPos > 1 then
  begin
    AnAlias := Copy(AKey,1,DotPos-1);
    AKey := Copy(AKey,DotPos+1,length(AKey));
  end;
  if AnAlias <> '' then
  begin
    //TODO: iterate through genfiles to find alias
    i := IndexOf(AnAlias);
    if i > -1 then
      Return := FGenFiles[i].GenFile.GetValue(Akey);
  end
  else
  begin
    //seek in all gen files, the first match is returned, otherwise, return default
    for ARec in FGenFiles do
    begin
      if ARec.GenFile.HasKey(AKey) then
      begin
        Return := ARec.GenFile.GetValue(AKey);
        Break;
      end;
    end;
  end;
  Result := Return;
end;

function TGenFileSet.GetValue(AKey,ADefault:string):TParseResult;
var
  DotPos,i:integer;
  AnAlias:string;
  Return:TParseResult;
  ARec:TGenFileRecord;
begin
  AKey := Trim(AKey);
  AKey := Copy(AKey,Length(FROM_GEN_SET)+1,Length(AKey));
  DotPos := Pos(ATTR_ACCESSOR,AKey);
  AnAlias := '';
  Return.Value := ADefault;
  Return.ParseOk := True;
  if DotPos = 1 then
    AKey := Copy(AKey,2,Length(AKey))
  else if DotPos > 1 then
  begin
    AnAlias := Copy(AKey,1,DotPos-1);
    AKey := Copy(AKey,DotPos+1,length(AKey));
  end;
  if AnAlias <> '' then
  begin
    //TODO: iterate through genfiles to find alias
    i := IndexOf(AnAlias);
    if i > -1 then
      Return := FGenFiles[i].GenFile.GetValue(Akey,ADefault);
  end
  else
  begin
    //seek in all gen files, the first match is returned, otherwise, return default
    for ARec in FGenFiles do
    begin
      if ARec.GenFile.HasKey(AKey) then
      begin
        Return := ARec.GenFile.GetValue(AKey,ADefault);
        Break;
      end;
    end;
  end;
  Result := Return;
end;

procedure TGenFileSet.Add(AGenFile, ASeparator:string);
begin
  Add(AGenFile,ASeparator,DEF_IF_NOT);
end;

procedure TGenFileSet.Add(AGenFile:string);
begin
  Add(AGenFile,GEN_SEP,DEF_IF_NOT);
end;

procedure TGenFileSet.Add(AGenFile, ASeparator, ADefault:string);
var
  len:integer;
  AGenObj:TGenFile;
begin
  len := Length(FGenFiles);
  SetLength(FGenFiles,len+1);
  AGenObj := TGenFile.Create;
  AGenObj.GenSeparator := ASeparator[1];
  AGenObj.IfNotFound := ADefault;
  AGenObj.Load(AGenFile);
  FGenFiles[len].GenFile := AGenObj;
  FGenFiles[len].GenAlias := AGenObj.OnlyName;
end;

procedure TGenFileSet.Add(var AGenFile: TGenFile; AnAlias:string);
var
  len: integer;
begin
  len := Length(FGenFiles);
  SetLength(FGenFiles, len+1);
  FGenFiles[len].GenFile := AGenFile;
  FGenFiles[len].GenAlias := AnAlias;
end;

destructor TGenFileSet.Destroy;
var
  AGen:TGenFileRecord;
begin
  for AGen in FGenFiles do
    AGen.GenFile.Free;
  SetLength(FGenFiles,0);
end;

end.

