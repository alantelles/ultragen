unit NumClass;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, ASTClass, TokenClass, Tokens;
type
  TNumInt = class (TAST)
    protected
      FValue: string;
    public
      property PValue: string read FValue write FValue;
      constructor Create(AToken: TToken);
	end;

  TNumFloat = class (TAST)
    protected
      FValue: string;
    public
      property PValue: string read FValue write FValue;
      constructor Create(AToken: TToken);
	end;

  TByte = class(TAST)
     protected
      FValue: string;
    public
      property PValue: string read FValue write FValue;
      constructor Create(AToken: TToken);
  end;

implementation

constructor TNumInt.Create(AToken: TToken);
begin
  FToken := AToken;
  FValue := AToken.PValue;
end;

constructor TByte.Create(AToken: TToken);
begin
  FToken := AToken;
  FValue := AToken.PValue;
end;

constructor TNumFloat.Create(AToken: TToken);
begin
  FToken := AToken;
  FValue := AToken.PValue;
end;

end.
