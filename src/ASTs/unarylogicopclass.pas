unit UnaryLogicOpClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TokenClass, ASTClass;

type
  TUnaryLogicOp = class (TAST)
    protected
      FOper: TToken;
      FExpr: TAST;
    public
      property PExpr: TAST read FExpr write FExpr;
      property PToken: TToken read FToken write FToken;
      property POper: TToken read FOper write FOper;
      constructor Create(AToken: TToken; AExpr: TAST);
      destructor Destroy; override;
  end;

implementation
constructor TUnaryLogicOp.Create(AToken: TToken; AExpr: TAST);
begin
  FToken := AToken;
  FOper := AToken;
  FExpr := AExpr;
end;

destructor TUnaryLogicOp.Destroy;
begin
  FExpr.Free;
  inherited;
end;

end.

