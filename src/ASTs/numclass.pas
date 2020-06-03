unit NumClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, ASTClass, TokenClass, Tokens;
type
  TNum = class (TAST)
    protected
      FValue: string;
    public
      property PValue: string read FValue write FValue;
      constructor Create(AToken: TToken);
	end;

implementation

constructor TNum.Create(AToken: TToken);
begin
  FToken := AToken;
  FValue := AToken.PValue;
end;

end.
