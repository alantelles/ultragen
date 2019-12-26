unit GenFileClass;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, StrUtils,

    { Globals }
    VariablesGlobals,
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
    property Pairs:TDict read FPairs;
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
  end;

implementation
uses FileHandlingUtils;

constructor TGenFile.Create;
begin
  FDefault := DEF_IF_NOT;
  FGenSep := GEN_SEP;
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
  for APair in FPairs do
  begin
    Return:=False;
    if APair.Key = AKey then
    begin
      Return := True;
      Break;
    end;
  end;
  Result := Return;
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
    if (APos > 0) and (Line[APos-1] <> ESCAPER) then
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
  i:integer;
begin
  if Length(FPairs) > 0 then
    for i in [0..Length(FPairs)-1] do
    begin
      if Trim(AKey) = FPairs[i].Key then
      begin
        FPairs[i].Value := AValue;
        Exit;
      end;
    end;
  SetLength(FPairs,Length(FPairs)+1);
  FPairs[Length(FPairs)-1].Key := AKey;
  FPairs[Length(FPairs)-1].Value := ReplaceStr(AValue,'\n',sLineBreak);
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

