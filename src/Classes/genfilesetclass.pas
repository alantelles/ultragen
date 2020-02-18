unit GenFileSetClass;

{$mode objfpc}{$H+}
{$LongStrings ON}

interface

uses
  Classes, SysUtils,StrUtils,

  { Classes }
  GenFileClass,

  { Utils }
  FileHandlingUtils,

  { Globals }
   ConstantsGlobals, TypesGlobals;

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
      procedure ClearSet;
      procedure Enlist(AGenFiles:array of string);
      procedure Enlist(var AGenFiles:TStringList);
      procedure Enlist(var AGenFiles:TStringList; AnAliasRule:string);
      procedure Add(WithAlias:boolean; AGenFile:string; AnAlias:string);
      procedure CopyGenSet(var OutGenSet:TGenFileSet);
      function Add(WithAlias:boolean; AnAlias:string):integer;
      procedure Add(AGenFile:string);
      procedure Add(AGenFile, ASeparator:string);
      procedure Add(AGenFile, ASeparator, ADefault:string);
      procedure Add(var AGenFile:TGenFile; AnAlias:string);
      procedure Drop(var AnAlias:string);
      procedure SetDefault(AValue:String);
      function GetValue(AKey:string):TParseResult;
      function GetValue(AKey,ADefault:string):TParseResult;
      function IndexOf(AnAlias:string):integer;
      function GenKeyByIndex(AnAlias:string;AnIndex:integer):string;
      function GenValueByIndex(AnAlias:string;AnIndex:integer):string;
      function KeysCount(AnAlias:string):string;
      destructor Destroy; override;
  end;

implementation
uses
  VariablesGlobals;

constructor TGenFileSet.Create;
begin
  SetLength(FGenFiles,0);
end;

procedure TGenFileSet.ClearSet;
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

procedure TGenFileSet.CopyGenSet(var OutGenSet:TGenFileSet);
var
  AGenFile:TGenFile;
  i,len:integer;
begin
  len := Length(FGenFiles);
  if len > 0 then
  begin
    for i:=0 to len-1 do
    begin
      AGenFile := TGenFile.Create;
      FGenFiles[i].GenFile.CopyGen(AGenFile);
      OutGenSet.Add(AGenFile,FGenFiles[i].GenAlias);
    end;
  end;
end;

function TGenFileSet.KeysCount(AnAlias:string):string;
var
  i:integer;
  Return :integer = -1;
begin
  i := IndexOf(AnAlias);
  if i > -1 then
    Return := Length(FGenFiles[i].GenFile.Pairs);
  Result := IntToStr(Return);
end;

function TGenFileSet.GenKeyByIndex(AnAlias:string;AnIndex:integer):string;
var
  Return:string = '';
  i:integer;
begin
  i := IndexOf(AnAlias);
  Return := FGenFiles[i].GenFile.Pairs[AnIndex].Key;
  Result := Return;
end;

function TGenFileSet.GenValueByIndex(AnAlias:string;AnIndex:integer):string;
var
  Return:string = '';
  i:integer;
begin
  i := IndexOf(AnAlias);
  Return := FGenFiles[i].GenFile.Pairs[AnIndex].Value;
  Result := Return;
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

procedure TGenFileSet.Enlist(var AGenFiles:TStringList; AnAliasRule:string);
var
  i:integer;
begin
  i := Add(True,AnAliasRule);
  FGenFiles[i].GenFile.SetValue('count',IntToStr(AGenFiles.Count));
  if AGenFiles.Count > 0 then
  begin
    Add(True,AnAliasRule);
	  for i:=0 to AGenFiles.Count-1 do
	    Add(True,AGenFiles[i],AnAliasRule+GEN_SUB_LEVEL+IntToStr(i));
	end;
end;

function TGenFileSet.IndexOf(AnAlias:string):integer;
var
  i:integer=-1;
  Return:integer=-1;
  ARec:TGenFileRecord;
begin
  try
    Return := StrToInt(AnAlias);
	except
  	for ARec in FGenFiles do
    begin
      i := i+1;
      if ARec.GenAlias = AnAlias then
      begin
        Return := i;
        Break;
      end;
    end;
	end;
	Result := Return;
end;

procedure TGenFileSet.Drop(var AnAlias:string);
var i: integer;
begin
  i := IndexOf(AnAlias);
  if i > -1 then
  begin
    FGenFiles[i] := FGenFiles[Length(FGenFiles)-1];
    SetLength(FGenFiles,Length(FGenFiles)-1);
	end;
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

procedure TGenFileSet.Add(WithAlias:boolean; AGenFile:string; AnAlias:string);
var
  len:integer;
  AGenObj: TGenFile;
begin
  len := Length(FGenFiles);
  SetLength(FGenFiles,len+1);
  AGenObj := TGenFile.Create;
  AGenObj.Load(AGenFile);
  FGenFiles[len].GenFile := AGenObj;
  if WithAlias then
    FGenFiles[len].GenAlias := AnAlias
  else
    FGenFiles[len].GenAlias := AGenObj.OnlyName;
end;

function TGenFileSet.Add(WithAlias:boolean; AnAlias:string):integer;
var
  len:integer;
  AGenObj: TGenFile;
begin
  len := Length(FGenFiles);
  SetLength(FGenFiles,len+1);
  AGenObj := TGenFile.Create;
  FGenFiles[len].GenFile := AGenObj;
  FGenFiles[len].GenAlias := AnAlias;
  Result := len;
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

