unit ARClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, InstanceOfClass;

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

      // overloads
      procedure AddMember(AKey:string; AObj:TInstanceOf);
      {procedure AddMember(AKey:string; AObj:TStringInstance);
      procedure AddMember(AKey:string; AObj:TIntegerInstance);
      procedure AddMember(AKey:string; AObj:TFloatInstance);
      procedure AddMember(AKey:string; AObj:TBooleanInstance);
      procedure AddMember(AKey:string; AObj:TFunctionInstance);}
      // end overloads

      function GetMember(AKey:string):TInstanceOf;
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

procedure TActivationRecord.AddMember(AKey:string; AObj:TInstanceOf);
begin
  try
    FMembers.Add(AKey, AObj)
	except
    FMembers[Akey] := AObj
	end;
end;

{procedure TActivationRecord.AddMember(AKey:string; AObj:TStringInstance);
begin
  try
    FMembers.Add(AKey, AObj)
	except
    FMembers[Akey] := AObj
	end;
end;

procedure TActivationRecord.AddMember(AKey:string; AObj:TFloatInstance);
begin
  try
    FMembers.Add(AKey, AObj)
	except
    FMembers[Akey] := AObj
	end;
end;

procedure TActivationRecord.AddMember(AKey:string; AObj:TIntegerInstance);
begin
  try
    FMembers.Add(AKey, AObj)
	except
    FMembers[Akey] := AObj
	end;
end;

procedure TActivationRecord.AddMember(AKey:string; AObj:TFunctionInstance);
begin
  try
    FMembers.Add(AKey, AObj)
	except
    FMembers[Akey] := AObj
	end;
end;

procedure TActivationRecord.AddMember(AKey:string; AObj:TBooleanInstance);
begin
  try
    FMembers.Add(AKey, AObj)
	except
    FMembers[Akey] := AObj
	end;
end;
                                    }

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

