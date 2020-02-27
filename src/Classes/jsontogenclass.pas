unit JsonToGenClass;

{$mode objfpc}{$H+}

interface

uses
  GenFileClass;

const
  OBJ_O = '{';
  OBJ_C = '}';
  LIST_O = '[';
  LIST_C = ']';
  SEP = ',';
  STR_ENC = '"';
  ATTR = ':';


type
  TListLevels = array of integer;

type
  TJson2Gen = class
  private
    FGenFile: TGenFile;
    FJson: string;
    Fkey, FPrefix: string;
    FListIndex: integer;
    FListLevels:TListLevels;
  public
    constructor Create(AStr: string; var AGenFile: TGenFile; Prefix:string='');
    procedure ParseJson;
    procedure ParseValue(AStr: string);
    procedure ParseList(AStr: string);
    procedure ParseObj(Astr: string);
    function IsObj(AStr: string): boolean;
    function IsList(AStr: string): boolean;
    function Conform(AStr: string): string;
  end;



implementation

uses
  Classes, SysUtils, ConstantsGlobals, StrUtils;

constructor TJson2Gen.Create(AStr: string; var AGenFile: TGenFile; Prefix:string='');
begin
  FGenFile := AGenFile;
  FJson := Trim(AStr);
  FKey := '';
  FPrefix := Prefix;
  FListIndex := 0;
  SetLength(FListLevels,0);
end;

function TJson2Gen.Conform(AStr: string): string;
begin
  AStr := Trim(AStr);
  if (AStr[1] = STR_ENC) and (AStr[Length(AStr)] = STR_ENC) then
    Result := Copy(AStr, 2, Length(AStr) - 2)
  else
    Result := AStr;
end;

procedure TJson2Gen.ParseJson;
begin
  //FGenFile.SetValue(FKey,FJson);
  ParseValue(FJson);
end;

procedure TJson2Gen.ParseValue(AStr: string);
var
  Prefix:string='';
begin
  if IsList(AStr) then
    ParseList(Copy(AStr, 2, Length(AStr) - 2))
  else if IsObj(AStr) then
    ParseObj(Copy(AStr, 2, Length(AStr) - 2))
  else
  begin
    if FPrefix <> '' then
      Prefix := FPrefix + GEN_SUB_LEVEL;
    if AStr = 'null' then
      AStr := '';
    FGenFile.SetValue(Prefix+FKey, AStr);
  end;
end;

procedure TJson2Gen.ParseList(AStr: string);
var
  c: char;
  inStr: boolean = False;
  inVal: boolean = False;
  objLevel, listLevel, AttrPos, len: integer;
  part: string = '';

begin
  len := Length(FListLevels);
  SetLength(FListLevels,len+1);
  FListLevels[len] := 0;
  objLevel := 0;
  listLevel := 0;
  if Length(AStr) > 0 then
  begin
    for c in AStr do
    begin
      if ((c = #10) or (c = #13) or (c = ' ')) and (not inStr) then
        continue;
      if (c = OBJ_O) then
        objLevel := objLevel + 1;
      if (c = OBJ_C) then
        objLevel := objLevel - 1;
      if (c = LIST_O) then
        listLevel := listLevel + 1;
      if (c = LIST_C) then
        listLevel := listLevel - 1;
      if (c = STR_ENC) then
        inStr := not inStr;
      if (c = SEP) and (not inStr) and (listLevel = 0) and (objLevel = 0) then
      begin
        AttrPos := RPos(GEN_SUB_LEVEL, FKey);
        if FKey <> '' then
          FKey := FKey + GEN_SUB_LEVEL + IntToStr(FListLevels[len])
        else
          FKey := IntToStr(FListLevels[len]);
        ParseValue(Conform(part));
        //FListIndex := FListIndex + 1;
        FListLevels[len] := FListLevels[len] + 1;
        AttrPos := RPos(GEN_SUB_LEVEL, FKey);
        if AttrPos > 0 then
          FKey := Copy(FKey, 1, AttrPos - 1)
        else
          FKey := '';
        part := '';
        inVal := False;
      end
      else
        part := part + c;
    end;
  end;

  if FKey <> '' then
    FKey := FKey + GEN_SUB_LEVEL + IntToStr(FListLevels[len])
  else
    FKey := IntToStr(FListLevels[len]);
  if Length(Part) > 0 then
  begin
    ParseValue(Conform(Part));
    FListIndex := 0;
    AttrPos := RPos(GEN_SUB_LEVEL, FKey);
    if AttrPos > 0 then
      FKey := Copy(FKey, 1, AttrPos - 1)
    else
      FKey := '';
  end;
  SetLength(FListLevels,len);
end;

procedure TJson2Gen.ParseObj(Astr: string);
var
  c: char;
  inStr: boolean = False;
  inVal: boolean = False;
  objLevel, listLevel, AttrPos: integer;
  part: string = '';
begin
  objLevel := 0;
  listLevel := 0;
  if Length(AStr) > 0 then
  begin
    for c in AStr do
    begin
      if ((c = #10) or (c = #13) or (c = ' ')) and (not inStr) then
        continue;
      if inVal and (c = OBJ_O) then
        objLevel := objLevel + 1;
      if inVal and (c = OBJ_C) then
        objLevel := objLevel - 1;
      if inVal and (c = LIST_O) then
        listLevel := listLevel + 1;
      if inVal and (c = LIST_C) then
        listLevel := listLevel - 1;
      if (c = STR_ENC) then
        inStr := not inStr;
      if (c = ATTR) and (not inVal) and (not inStr) then
      begin
        inVal := True;
        if FKey = '' then
          FKey := Conform(ReplaceStr(Part,':',''))
        else
          FKey := FKey + GEN_SUB_LEVEL + Conform(ReplaceStr(Part,':',''));
        part := '';
      end
      else if (c = SEP) and (not inStr) and (listLevel = 0) and (objLevel = 0) then
      begin
        ParseValue(Conform(part));
        AttrPos := RPos(GEN_SUB_LEVEL, FKey);
        if AttrPos > 0 then
          FKey := Copy(FKey, 1, AttrPos - 1)
        else
          FKey := '';
        part := '';
        inVal := False;
      end
      else
        part := part + c;
    end;
  end;
  if Length(AStr) > 0 then
  begin
    ParseValue(Conform(Part));
    AttrPos := RPos(GEN_SUB_LEVEL, FKey);
    if AttrPos > 0 then
      FKey := Copy(FKey, 1, AttrPos - 1)
    else
      FKey := '';
  end;
end;

function TJson2Gen.IsObj(AStr: string): boolean;
begin
  if Length(AStr) = 0 then
    Result := False
  else if (AStr[1] = OBJ_O) and (AStr[Length(AStr)] = OBJ_C) then
    Result := True
  else
    Result := False;
end;

function TJson2Gen.IsList(AStr: string): boolean;
begin
  if Length(AStr) = 0 then
    Result := False
  else if (AStr[1] = LIST_O) and (AStr[Length(AStr)] = LIST_C) then
    Result := True
  else
    Result := False;
end;

end.

