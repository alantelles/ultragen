unit BinLogicOpClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, ASTClass, TokenClass, Tokens, LoggingClass;
type
  TBinLogicOp = class (TAST)
    protected
      FLeft: TAST;
      FOper: TToken;
      FRight: TAST;
    public
      property PLeft: TAST read FLeft write FLeft;
      property POper: TToken read FOper write FOper;
      property PRight: TAST read FRight write FRight;
      constructor Create(ALeft, ARight: TAST; AToken: TToken);
      destructor Destroy; override;
	end;

implementation

constructor TBinLogicOp.Create(ALeft, ARight: TAST; AToken: TToken);
begin
  FLeft := ALeft;
  FOper := AToken;
  FToken := AToken;
  FRight := ARight;
  LogText(DEBUG, 'BinLogicOp', 'Creating a BinLogicOp between '+ ALeft.PToken.AsString + ' and ' + ARight.PToken.AsString);
end;

destructor TBinLogicOp.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  inherited;
end;

end.

