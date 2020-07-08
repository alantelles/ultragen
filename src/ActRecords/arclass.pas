unit ARClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, InstanceOfClass, ListInstanceClass, StringInstanceClass;

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
      FMembers: TFPObjectHashTable;
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
      procedure IterGetkeys(AItem: TObject; const Aname:string; var Cont: boolean);

      procedure CopyActRec(var AReceiver: TActivationRecord);
      property PMembers: TFPObjectHashTable read FMembers write FMembers;
      constructor Create(AName:string; AType: string; ALevel: integer);

      procedure AddMember(AKey:string; AObj:TInstanceOf);
      function GetMember(AKey:string):TInstanceOf;
      function GetFunction(AKey: string; ASrc: TInstanceOf = nil): TFunctionInstance;
      function AsString:string;
      procedure GetKeys(var AList: TListInstance);


  end;

  TActRecInstance = class (TInstanceOf)
    private
      FValue: TActivationRecord;
    public
      property PValue: TActivationRecord read FValue write FValue;
      function AsString:string; override;
      constructor Create(AnActRec: TActivationRecord);
  end;

implementation
constructor TActivationRecord.Create(AName:string; AType: string; ALevel: integer);
begin
  FName := AName;
  FType := AType;
  FNestingLevel := ALevel;
  FMembers := TFPObjectHashTable.Create();
end;

procedure TActivationRecord.GetKeys(var Alist: TListInstance);
begin
  FList := Alist;
  FMembers.Iterate(@IterGetKeys);
end;

procedure TActivationRecord.IterGetkeys(AItem: TObject; const Aname:string; var Cont: boolean);
begin
  FList.Add(TStringInstance.Create(Aname));
end;

function TActRecInstance.AsString:string;
begin
  Result := 'Dictionary "'+ FValue.FName + '"';
end;

constructor TActRecInstance.Create(AnActRec: TActivationRecord);
begin
  FValue := AnActRec;
end;

function TActivationRecord.GetFunction(AKey: string; ASrc: TInstanceOf = nil): TFunctionInstance;
var
  Ret: TFunctionInstance;
begin
  FFound := False;
  FNowKey := AKey;
  FNowType := ASrc;
  FMembers.Iterate(@SearchFunction);
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
  if AItem.ClassNameIs('TActRecInstance') then
    WriteLn(' ---- ' + TActRecInstance(AItem).PValue.AsString);
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

procedure TActivationRecord.AddMember(AKey:string; AObj:TInstanceOf);
begin
  try
    FMembers.Add(AKey, AObj)
	except
    FMembers[Akey] := AObj
	end;
end;

function TActivationRecord.GetMember(AKey:string):TInstanceOf;
var
  Ret:TInstanceOf;
begin
  Ret := TInstanceOf(FMembers[AKey]);
  Result := Ret;
end;

procedure TActivationRecord.CopyActRec(var AReceiver: TActivationrecord);
begin
  FReceiver := TActivationRecord.Create(Fname, FType, FNestinglevel);
  FMembers.Iterate(@CopyItems);
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
  FMembers.Iterate(@MemberAsString);
  Result := Ret;

end;

end.

