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
    FLineNo: integer;
    FCharNo: integer;
    FScriptName: string;
  public
    property PScriptName:string read FSCriptName write FScriptNAme;
    property PType:string read FType write FType;
    property PValue:string read FValue write FValue;
    property PLineNo: integer read FLineNo write FLineNo;
    property PCharNo: integer read FCharNo write FCharNo;
    constructor Create(AType, AValue:String; ALineNo, ACharNo: integer; AScriptName: string);
    constructor Create(AType, AValue:String);
    constructor Create(AToken: TToken);
    constructor Create;
    procedure SetValue(AType, AValue:String; ALineNo, ACharNo: integer; AScriptName: string);
    procedure SetPosition(ALineNo, ACharNo: integer; AScriptName: string);
    function AsString:string;
  end;

implementation

constructor TToken.Create;
begin

end;
constructor TToken.Create(AType, AValue:String; ALineNo, ACharNo: integer; AScriptName: string);
begin
  FType := AType;
  FValue := AValue;
  FLineNo := ALineNo;
  FCharNo := ACharNo;
  FScriptName := AScriptName;
end;
constructor TToken.Create(AType, AValue:String);
begin
  FType := AType;
  FValue := AValue;
end;

constructor TToken.Create(AToken: TToken);
begin
  FType := AToken.PType;
  FValue := AToken.PValue;
  FLineNo := AToken.PLineNo;
  FCharNo := AToken.PCharNo;
  FScriptName := AToken.PScriptName;
end;

procedure TToken.SetPosition(ALineNo, ACharNo: integer; AScriptName: string);
begin
  FLineNo := ALineNo;
  FCharNo := ACharNo;
  FScriptName := AScriptName;
end;

procedure TToken.SetValue(AType, AValue:String; ALineNo, ACharNo: integer; AScriptName: string);
begin
  FType := AType;
  FValue := AValue;
  FLineNo := ALineNo;
  FCharNo := ACharNo;
  FScriptName := AScriptName;
end;

function TToken.AsString:string;
begin
  Result := 'Token ('+ FType +', "'+ FValue +'")';
end;

end.

