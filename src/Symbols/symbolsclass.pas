unit SymbolsClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ASTClass;

type
  TSymbol = class
    protected
      FName:string;
      FType: TSymbol;
    public
      property PName:string read FName write FName;
      property PType: TSymbol read FType write FType;
      constructor Create(AName:string; AType: TSymbol=nil);
      function AsString:string;
  end;


  TBuiltinSymbol = class(TSymbol)
    public
      constructor Create(AName:string);
  end;

  TVariableSymbol = class(TSymbol)

  end;

  TProcedureSymbol = class(TSymbol)
    protected
      FParams: TASTList;
    public
      property PParams:TASTList read FParams write FParams;
      constructor Create(AName: string; AParams: TASTList = nil);

  end;


implementation
constructor TSymbol.Create(AName:string; AType: TSymbol=nil);
begin
  FName := AName;
  FType := AType;
end;

constructor TProcedureSymbol.Create(AName: string; AParams: TASTList = nil);
var
  len: integer;
begin
  FName := AName;
  if AParams <> nil then
  begin
    len := Length(AParams);
    SetLength(FParams, len);
    FParams := AParams;
  end;
end;

function TSymbol.AsString:string;
var
  msg:string;
begin

  if FType = nil then
    msg := '<BuiltinType: ' + PName + '>'
  else
  begin
    msg := '<instance of ' + PType.PName + '> named: ' + FName;
  end;
  Result := msg;
end;

constructor TBuiltinSymbol.Create(AName:string);
begin
  FName := AName;
end;


end.

