unit ARClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

const
  AR_PROGRAM = 'AR_PROGRAM';
  AR_FUNCTION = 'AR_FUNCTION';

type
  TActivationRecord = class
    private
      FName:string;
      FType:string;
      FNestingLevel: integer;
      FMembers: TFPObjectHashTable;
    public
      property PName:string read FName;
      property PType:string read FType;
      property PNestingLevel:integer read FNestingLevel;
      procedure MemberAsString(AItem:  TObject;const AName:string; var Cont:boolean);
      property PMembers: TFPObjectHashTable read FMembers write FMembers;
      constructor Create(AName:string; AType: string; ALevel: integer);
      procedure AddMember(AKey:string; AObj:TObject);
      function GetMember(AKey:string):TObject;
      function AsString:string;
  end;

implementation
constructor TActivationRecord.Create(AName:string; AType: string; ALevel: integer);
begin
  FName := AName;
  FType := AType;
  FNestingLevel := ALevel;
  FMembers := TFPObjectHashTable.Create();
end;

procedure TActivationRecord.MemberAsString(AItem:  TObject;const AName:string; var Cont:boolean);
begin
  writeln('Record member '+Aname+' of type '+AItem.ToString);
end;

procedure TActivationRecord.AddMember(AKey:string; AObj:TObject);
begin
  FMembers.Add(AKey, AObj);
end;

function TActivationRecord.GetMember(AKey:string):TObject;
begin
  Result := FMembers[AKey];
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

