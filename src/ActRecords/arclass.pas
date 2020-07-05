unit ARClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, InstanceOfClass;

const
  AR_PROGRAM = 'AR_PROGRAM';
  AR_FUNCTION = 'AR_FUNCTION';
  AR_NAMESPACE = 'AR_NAMESPACE';

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
    public
      property PName:string read FName;
      property PType:string read FType;
      property PNestingLevel:integer read FNestingLevel;

      procedure MemberAsString(AItem:  TObject;const AName:string; var Cont:boolean);
      procedure SearchFunction(AItem: TObject; const AName:string; var Cont: boolean);

      property PMembers: TFPObjectHashTable read FMembers write FMembers;
      constructor Create(AName:string; AType: string; ALevel: integer);

      procedure AddMember(AKey:string; AObj:TInstanceOf);
      function GetMember(AKey:string):TInstanceOf;
      function GetFunction(AKey: string; ASrc: TInstanceOf = nil): TFunctionInstance;
      function AsString:string;

  end;

  TActRecInstance = class (TInstanceOf)
    private
      FValue: TActivationRecord;
    public
      property PValue: TActivationRecord read FValue write FValue;
      function AsString:string;
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

function TActRecInstance.AsString:string;
begin
  Result := 'Namespace "'+ FValue.FName + '"';
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
  writeln('Record member '+Aname+' of type '+AItem.ToString);
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
      //writeln('achei: ', AName, ' -- ', TFunctionInstance(AItem).PType)
    end
    else
    begin
      if TFunctionInstance(AItem).PType = FNowType.ClassName then
      begin
        exit;
        //writeln('achei: ', AName, ' -- ', TFunctionInstance(AItem).PType)
      end
      else
      begin
        exit;
        //writeln('achei: ', AName, ST_ACCESS, TFunctionInstance(AItem).PType, ' de outro tipo');
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

function TActivationRecord.AsString:string;
var
  Ret: string = '';
begin
  writeln('Activation record named '+FName+' of type '+FType);
  FMembers.Iterate(@MemberAsString);
  Result := Ret;

end;

end.

