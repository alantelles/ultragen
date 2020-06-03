unit UnaryOpClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TokenClass, ASTClass;

type
  TUnaryOp = class (TAST)
    protected
      FOper: TToken;
      FExpr: TAST;
    public
      property PExpr: TAST read FExpr write FExpr;
      property PToken: TToken read FToken write FToken;
      property POper: TToken read FOper write FOper;
      constructor Create(AToken: TToken; AExpr: TAST);

  end;

implementation
constructor TUnaryOp.Create(AToken: TToken; AExpr: TAST);
begin
  FToken := AToken;
  FOper := AToken;
  FExpr := AExpr;
end;

end.

