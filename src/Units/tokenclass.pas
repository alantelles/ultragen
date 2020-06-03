unit TokenClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TToken = class
  private
    FType: string;
    FValue: string;
  public
    property PType:string read FType write FType;
    property PValue:string read FValue write FValue;
    constructor Create(AType, AValue:String);
    constructor Create;
    procedure SetValue(AType, AValue:String);
    function AsString:string;
  end;

implementation

constructor TToken.Create;
begin

end;
constructor TToken.Create(AType, AValue:String);
begin
  FType := AType;
  FValue := AValue;

end;

procedure TToken.SetValue(AType, AValue:String);
begin
  FType := AType;
  FValue := AValue;
end;

function TToken.AsString:string;
begin
  Result := 'Token ('+ FType +', "'+ FValue +'")';
end;

end.

