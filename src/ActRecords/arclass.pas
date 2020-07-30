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
      //FMembers: TStringList;
      FNowType: TInstanceOf;
      FNowKey: string;
      FFound: boolean;
      FReceiver: TActivationrecord;
      FList: TListInstance;
    public
      property PName:string read FName;
      property PType:string read FType;
      property PNestingLevel:integer read FNestingLevel;

      procedure MemberAsString(AItem:  TObject;const AName:string; var Cont:boolean);
      procedure SearchFunction(AItem: TObject; const AName:string; var Cont: boolean);
      procedure CopyItems(AItem: TObject; const Aname:string; var Cont: boolean);

      procedure FreeMembers(AItem: TObject; const Aname:string; var Cont: boolean);

      procedure FreeAllMembers;
      procedure CopyActRec(var AReceiver: TActivationRecord);
      property PMembers: TFPHashObjectList read FMembers write FMembers;
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
  i: integer;
begin
  if FMembers.Count > 0 then
  begin
    for i := FMembers.Count - 1 downto 0 do
    begin
      if FMembers[i] <> nil then
      begin
        if TInstanceOf(FMembers[i]).PCoreType then
          FMembers[i].Free;
      end;
      FMembers.Delete(i);
    end;
  end;
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

constructor TDictionaryInstance.Create(AnActRec: TActivationRecord; ADefault:TInstanceOf = nil);
begin
  FValue := AnActRec;
  FAddLocked := False;
  FChangeLocked := False;
  if ADefault = nil then
    FDefault := nil
  else
    FDefault := ADefault;
end;

procedure TActivationRecord.FreeAllMembers;
begin
  //FMembers.Iterate(@FreeMembers);
  FNowType.Free;
end;

function TActivationRecord.GetFunction(AKey: string; ASrc: TInstanceOf = nil): TFunctionInstance;
var
  Ret: TFunctionInstance;
begin
  FFound := False;
  FNowKey := AKey;
  FNowType := ASrc;
  //FMembers.Iterate(@SearchFunction);
  FNowKey := '';
  FNowType := nil;
  Ret := TFunctionInstance(GetMember(AKey));
  if Ret <> nil then
    Ret.PName := AKey;
  Result := Ret;
end;

procedure TActivationRecord.MemberAsString(AItem:  TObject;const AName:string; var Cont:boolean);
begin
  WriteLn('Record member '+Aname+' of type '+AItem.ToString);
  if AItem.ClassNameIs('TDictionaryInstance') then
    WriteLn(' ---- ' + TDictionaryInstance(AItem).PValue.AsString);
end;

procedure TActivationRecord.FreeMembers(AItem:  TObject;const AName:string; var Cont:boolean);
begin
  //FNowType := TInstanceOf(AItem);
  //writeln(AItem.ToString);
end;

procedure TActivationRecord.SearchFunction(AItem:  TObject;const AName:string; var Cont:boolean);
const
  ST_ACCESS = ':';
begin
  if (AName = FNowKey) then
  begin
    if FNowType = nil then
    begin
      exit;
      //WriteLn('achei: ', AName, ' -- ', TFunctionInstance(AItem).PType)
    end
    else
    begin
      if TFunctionInstance(AItem).PType = FNowType.ClassName then
      begin
        exit;
        //WriteLn('achei: ', AName, ' -- ', TFunctionInstance(AItem).PType)
      end
      else
      begin
        exit;
        //WriteLn('achei: ', AName, ST_ACCESS, TFunctionInstance(AItem).PType, ' de outro tipo');
      end;
    end;
  end;
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
    if FMembers[i].ClassNameIs('TIntegerInstance') or
       FMembers[i].ClassNameIs('TBooleanInstance') or
       FMembers[i].ClassNameIs('TStringInstance') or
       FMembers[i].ClassNameIs('TFloatInstance') or
       FMembers[i].ClassNameIs('TNullInstance') then
      FMembers[i].Free;
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
begin
  FReceiver := TActivationRecord.Create(Fname, FType, FNestinglevel);
  if FMembers.Count > 0 then
  begin
    for i:=0 to FMembers.Count - 1 do
      FReceiver.AddMember(FMembers.NameOfIndex(i), TInstanceOf(FMembers[i]));
  end;
  AReceiver := FReceiver;
end;

procedure TActivationRecord.CopyItems(AItem:  TObject;const AName:string; var Cont:boolean);
begin
  FReceiver.AddMember(AName, TInstanceOf(AItem));
end;

function TActivationRecord.AsString:string;
var
  Ret: string = '';
begin
  WriteLn('Activation record named '+FName+' of type '+FType);
  //FMembers.Iterate(@MemberAsString);
  Result := Ret;

end;

end.

