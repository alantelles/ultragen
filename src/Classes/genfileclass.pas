unit GenFileClass;

{$mode objfpc}{$H+}
{$LongStrings ON}

interface

uses
    Classes, SysUtils, StrUtils,

    { Globals }
    ConstantsGlobals,
    TypesGlobals;

type

  TGenFile = class
  private
    FFullName, FDefault:string;
    FPairs:TDict;
    FGenSep:string;
  public
    property FullName:string read FFullName write FFullName;
    property IfNotFound:string read FDefault write FDefault;
    property GenSeparator:string read FGenSep write FGenSep;
    property Pairs:TDict read FPairs write FPairs;
    constructor Create;
    function Name:string;
    function OnlyName:string;
    function Load(AFullName:string):boolean;
    function Load(AGenFile:TStringList; AFullName:string):boolean;
    function Save(AName:string=''):boolean;
    function HasKey(AKey:string):boolean;
    function GetValue(AKey:string):TParseResult;
    function GetValue(AKey:string; AIfNotFound:string):TParseResult;
    procedure SetValue(AKey, AValue:string);
    procedure DropKey(AKey:string);
    procedure Append(AKey,AValue:string);
    procedure Prepend(AKey,AValue:string);
    procedure Print;
    procedure ClearValues;
    procedure CaptureGen(var OutGenFile:TGenFile);
    procedure CopyGen(var OutGenFile:TGenFile);
    //sort

  end;

implementation
uses
    VariablesGlobals, FileHandlingUtils;

constructor TGenFile.Create;
begin
  FDefault := DEF_IF_NOT;
  FGenSep := GEN_SEP;
end;

procedure TGenFile.ClearValues;
begin
  SetLength(FPairs,0);
end;

function TGenFile.Name:string;
begin
  Result:=GetFileName(FFullName);
end;

function TGenFile.OnlyName:string;
begin
  Result:=GetFileName(FFullName,False);
end;

function TGenFile.HasKey(AKey:string):boolean;
var
  APair:TKVPair;
  Return:boolean;
begin
  Return:=False;
  for APair in FPairs do
  begin

    if APair.Key = AKey then
    begin
      Return := True;
      Break;
    end;
  end;
  Result := Return;
end;

procedure TGenFile.CopyGen(var OutGenFile:TGenFile);
var
  APair:TKVPair;
begin
  OutGenFile.ClearValues;
  for APair in FPairs do
    OutGenFile.SetValue(APair.Key, APair.Value);
  OutGenFile.IfNotFound := FDefault;
  OutGenFile.GenSeparator := FGenSep;
end;

procedure TGenFile.CaptureGen(var OutGenFile:TGenFile);
var
  i, len:integer;
  AKey:string;
begin
  len := Length(OutGenFile.Pairs);
  if len > 0 then
  begin
    for i:=0 to len-1 do
    begin
      AKey := OutGenFile.Pairs[i].Key;
      OutGenFile.SetValue(AKey, GetValue(AKey).Value);
    end;
  end;
end;

function TGenFile.Load(AFullName:string):boolean;
var
  Temp: TStringList;
  Line, Key, Value: string;
  APos:integer;
  i:integer=0;
begin
  SetLength(FPairs,0);
  FFullName := ExpandFileName(AFullName);
  Temp := TStringList.Create;
  Temp.SkipLastLineBreak := True;
  try
    Temp.LoadFromFile(FFullName);
    Key := '';
    for Line in Temp do
    begin
      Inc(i);
      APos:= Pos(FGenSep,Line);
      if APos > 0 then
      begin
        Key := Copy(Line,1,APos-1);
        Value := Copy(Line,APos+Length(FGenSep),Length(Line));
      end
      else
      begin
        if Key = '' then
        begin
          WriteLn('ERROR: GEN file parse error at line: ',i,' in file ',FFullName);
          Exit;
        end
        else
        begin
          Value := Value + sLineBreak + Line;
        end;
      end;
      SetValue(Key,Value);
    end;
    Temp.Free;
    Result := True;
  except
    Temp.Free;
    Result := False;
  end;
end;

function TGenFile.Load(AGenFile:TStringList; AFullName:string):boolean;
var
  Line, Key, Value: string;
  APos:integer;
  i:integer=0;
begin
  SetLength(FPairs,0);
  FFullName := ExpandFileName(AFullName);
  for Line in AGenFile do
  begin
    Inc(i);
    APos:= Pos(FGenSep,Line);
    if (APos > 1) and (Line[APos-1] <> ESCAPER) then
    begin
      Key := Copy(Line,1,APos-1);
      Value := Copy(Line,APos+Length(FGenSep),Length(Line));
    end
    else
    begin
      if Key = '' then
      begin
        WriteLn('ERROR: GEN file parse error at line: ',i);
        Exit;
      end
      else
      begin
        Value := Value + sLineBreak + Line;
      end;
    end;
    SetValue(Key,Value);
  end;
  Result := True;
end;

function TGenFile.GetValue(AKey:string):TParseResult;
var
  APair:TKVPair;
  Return: TParseResult;
begin
  Return.Value := FDefault;
  Return.ParseOk := False;
  for APair in FPairs do
  begin
    if AKey = APair.Key then
    begin
       Return.Value := APair.Value;
       Return.ParseOk := True;
       Break;
    end;
  end;
  Result := Return;
end;

function TGenFile.GetValue(AKey:string; AIfNotFound:string):TParseResult;
var
  APair:TKVPair;
  Return: TParseResult;
begin
  Return.Value := AIfnotFound;
  Return.ParseOk := True;
  for APair in FPairs do
  begin
    if AKey = APair.Key then
    begin
       Return.Value := APair.Value;
       Return.ParseOk := True;
       Break;
    end;
  end;
  Result := Return;
end;

procedure TGenFile.SetValue(AKey, AValue:string);
var
  i,len:integer;
begin
  len := Length(FPairs);
  if len > 0 then
    for i:=0 to len-1 do
    begin
      if Trim(AKey) = FPairs[i].Key then
      begin
        FPairs[i].Value := AValue;
        Exit;
      end;
    end;
  SetLength(FPairs,len+1);
  FPairs[len].Key := AKey;
  FPairs[len].Value := ReplaceStr(AValue,'\n',sLineBreak);
end;

procedure TGenFile.DropKey(AKey:string);
var
  i:integer;
  Aux:TKVPair;
begin
  if Length(FPairs) > 0 then
    for i in [0..Length(FPairs)-1] do
    begin
      if Trim(AKey) = FPairs[i].Key then
      begin
        FPairs[i] := FPairs[Length(FPairs)-1];
        SetLength(FPairs,Length(FPairs)-1);
        Break;
      end;
    end;
end;

function TGenFile.Save(AName:string=''):boolean;
var
  Temp:TStringList;
  APair:TKVPair;
  OutFile:string;
begin
  Temp := TStringList.Create;
  Temp.SkipLastLineBreak := True;
  for APair in FPairs do
    Temp.Add(APair.Key+FGenSep+APair.Value);
  if AName = '' then
    OutFile := FFullName
  else if Pos(DirectorySeparator,AName) = 0 then
  begin
    AName := '.'+DirectorySeparator+AName;
    OutFile := AName;
	end
  else
    OutFile := AName;
	try
    CreateDirTree(GetFilePath(OutFile));
    Temp.SaveToFile(OutFile);
    Temp.Free;
    Result := True;
  except
    Temp.Free;
    Result := False;
  end;
end;

procedure TGenFile.Print;
var
  APair:TKVPair;
begin
  for APair in FPairs do
    WriteLn(APair.Key+FGenSep+APair.Value);
end;

procedure TGenFile.Append(AKey, AValue:string);
var
  Temp:String;
begin
  Temp:= GetValue(AKey).Value;
  Temp := Temp+AValue;
  SetValue(AKey, Temp);
end;

procedure TGenFile.Prepend(AKey, AValue:string);
var
  Temp:string;
begin
  Temp:=GetValue(AKey).Value;
  Temp:=AValue+Temp;
  SetValue(AKey, Temp);
end;



end.

