unit ARClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, InstanceOfClass, StrUtils, ListInstanceClass, StringInstanceClass;

const
  AR_PROGRAM = 'AR_PROGRAM';
  AR_FUNCTION = 'AR_FUNCTION';
  AR_NAMESPACE = 'AR_NAMESPACE';
  AR_DICT = 'AR_DICT';

type


  TActivationRecord = class
    private
      FName:string;
      FType:string;
      FNestingLevel: integer;
      FMembers: TFPHashObjectList;
      FReturnValue: TInstanceOf;
    public
      property PName:string read FName;
      property PType:string read FType;
      property PNestingLevel:integer read FNestingLevel;
      property PReturnValue: TInstanceOf read FReturnValue write FReturnValue;
      property PMembers: TFPHashObjectList read FMembers write FMembers;
      procedure CopyActRec(var AReceiver: TActivationRecord);
      constructor Create(AName:string; AType: string; ALevel: integer);
      function AddMember(AKey:string; AObj:TInstanceOf): boolean;
      function GetMember(AKey:string):TInstanceOf;
      function GetFunction(AKey: string; ASrc: TInstanceOf = nil): TFunctionInstance;
      function AsString:string;
      procedure GetKeys(var AList: TListInstance);                                
      function DropItem(AKey: string): TInstanceOf;
      destructor Destroy; override;

  end;

  TDictionaryInstance = class (TInstanceOf)
    private
      FValue: TActivationRecord;
      FDefault: TInstanceOf;
      FAddLocked: boolean;
      FChangeLocked: boolean;

    public
      property PAddLocked: boolean read FAddLocked write FAddLocked;
      property PChangeLocked: boolean read FChangeLocked write FChangeLocked;
      property PValue: TActivationRecord read FValue write FValue;
      property PDefault: TInstanceOf read FDefault write FDefault;
      function AsString:string; override;
      constructor Create(AnActRec: TActivationRecord; ADefault:TInstanceOf = nil);
      procedure CopyInstance(var AReceiver: TInstanceOf); override;
  end;

implementation
uses ExceptionsClasses;

constructor TActivationRecord.Create(AName:string; AType: string; ALevel: integer);
begin
  FName := AName;
  FType := AType;
  FNestingLevel := ALevel;
  FMembers := TFPHashObjectList.Create(False);
end;

destructor TActivationRecord.Destroy;
var
  i, selfPos: integer;
begin
  selfPos := FMembers.FindIndexOf('self');
  if FMembers.Count > 0 then
  begin
    for i := FMembers.Count - 1 downto 0 do
    begin
      if FMembers[i] <> nil then
      begin
        if (TInstanceOf(FMembers[i]).PCoreType) and (i <> selfPos) then
          FMembers[i].Free;
      end;
      FMembers.Delete(i);
    end;
  end;
  inherited
end;

procedure TActivationRecord.GetKeys(var Alist: TListInstance);
var
  i: integer;
begin
  if FMembers.Count > 0 then
  begin
    for i:=0 to FMembers.Count-1 do
      AList.Add(TStringInstance.Create(FMembers.NameOfIndex(i)));
  end;
end;

function TActivationRecord.DropItem(AKey: string): TInstanceOf;
var
  AFind: TInstanceOf;
  AIndex: integer;
begin
  AIndex := FMembers.FindIndexOf(AKey);
  AFind := TInstanceOf(FMembers[AIndex]);
  FMembers.Delete(AIndex);
  Result := AFind;
end;

function TDictionaryInstance.AsString:string;
var
  Ret:string = '{ ';
  len, i: integer;
begin
  len := FValue.FMembers.Count;
  if len > 0 then
  begin
    for i:=0 to len - 1 do
    begin
      ret := Ret + FValue.FMembers.NameOfIndex(i) + ': ';
      if FValue.FMembers[i].ClassNameIs('TStringInstance') then
        Ret := Ret + '''' + TInstanceOf(FValue.FMembers[i]).AsString + ''''
      else
        Ret := Ret + TInstanceOf(FValue.FMembers[i]).AsString;
      if i < len-1 then
        ret := Ret + ', '
    end;
  end;
  Ret := Ret + ' }';
  Result := Ret;
end;

procedure TDictionaryInstance.CopyInstance(var AReceiver: TInstanceOf);
var
  Cast: TDictionaryInstance;
  ADef: TInstanceOf;
  AActRec: TActivationRecord;
begin
  Cast := TDictionaryInstance.Create(FValue);
  Cast.PAddLocked := FAddLocked;
  Cast.PChangeLocked := FChangeLocked;
  FValue.CopyActRec(AActRec);
  if FDefault <> nil then
  begin
    ADef := TInstanceOf.Create;
    FDefault.CopyInstance(ADef);
    Cast.PDefault := ADef;
  end;
  AReceiver := Cast;
end;

constructor TDictionaryInstance.Create(AnActRec: TActivationRecord; ADefault:TInstanceOf = nil);
var
  ADef: TInstanceOf;
begin
  FValue := AnActRec;
  FAddLocked := False;
  FChangeLocked := False;
  if ADefault = nil then
    FDefault := nil
  else
  begin
    ADefault.CopyInstance(ADef);
    FDefault := ADef;
  end;
end;

function TActivationRecord.GetFunction(AKey: string; ASrc: TInstanceOf = nil): TFunctionInstance;
var
  Ret: TFunctionInstance;
begin
  Ret := TFunctionInstance(GetMember(AKey));
  if Ret <> nil then
    Ret.PName := AKey;
  Result := Ret;
end;

function TActivationRecord.AddMember(AKey:string; AObj:TInstanceOf):boolean;
var
  i: integer;
  test: TInstanceOf;
begin
  i := FMembers.FindIndexOf(AKey);
  if (i > -1) and (AKey[1] = '$') then
  begin
    Result := False;
    exit;
	end;
	if (i > -1) then
  begin
    {if FMembers[i].ClassNameIs('TIntegerInstance') or
       FMembers[i].ClassNameIs('TBooleanInstance') or
       FMembers[i].ClassNameIs('TStringInstance') or
       FMembers[i].ClassNameIs('TFloatInstance') or
       FMembers[i].ClassNameIs('TNullInstance') then
      FMembers[i].Free;}
    FMembers.Delete(i);
  end;
  FMembers.Add(AKey, AObj);
  Result := True;
end;

function TActivationRecord.GetMember(AKey:string):TInstanceOf;
var
  Ret:TInstanceOf;
begin
  Ret := TInstanceOf(FMembers.Find(AKey));
  if Ret <> nil then
  begin
    if Ret.ClassNameIs('TIntegerInstance') then
      Ret := TIntegerInstance.Create(Ret.PIntValue)
    else if Ret.ClassNameIs('TBooleanInstance') then
      Ret := TBooleanInstance.Create(Ret.PBoolValue)
    else if Ret.ClassNameIs('TStringInstance') then
      Ret := TStringInstance.Create(Ret.PStrValue)
    else if Ret.ClassNameIs('TFloatInstance') then
      Ret := TFloatInstance.Create(Ret.PFloatValue)
    else if Ret.ClassNameIs('TNullInstance') then
      Ret := TNullInstance.Create;
  end;
  Result := Ret;
end;

procedure TActivationRecord.CopyActRec(var AReceiver: TActivationrecord);
var
  i: integer;
  APtr: Pointer;
  ARec: TActivationRecord;
begin
  ARec := TActivationRecord.Create(Fname, FType, FNestinglevel);
  if FMembers.Count > 0 then
  begin
    for i:=0 to FMembers.Count - 1 do
      ARec.AddMember(FMembers.NameOfIndex(i), TInstanceOf(FMembers[i]));
  end;
  AReceiver := ARec;
end;

function TActivationRecord.AsString:string;
var
  Ret: string = '';
begin
  WriteLn('Activation record named '+FName+' of type '+FType);
  Result := Ret;

end;

end.

