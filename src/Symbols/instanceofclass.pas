unit InstanceOfClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SymbolsClass, SymbolTableClass;

type
  TInstanceOf = class
    protected
      FSymbol: TSymbol;
    public
  end;

  TIntegerInstance = class (TInstanceOf)
    protected
      FValue:integer;
    public
      property PValue:integer read FValue write FValue;
      constructor Create(AValue: integer);
  end;

  TStringInstance = class (TInstanceOf)
    protected
      FValue:string;
    public
      property PValue:string read FValue write FValue;
      constructor Create(AValue: string);
  end;

  TFloatInstance = class (TInstanceOf)
    protected
      FValue:extended;
    public
      property PValue:Extended read FValue write FValue;
      constructor Create(AValue: extended);
  end;

  TBooleanInstance = class (TInstanceOf)
    protected
      FValue: boolean;
    public
      property PValue: boolean read FValue write FValue;
      constructor Create(AValue: boolean);
  end;

implementation
constructor TIntegerInstance.Create(AValue: integer);
begin
  FValue := AValue;
end;

constructor TFloatInstance.Create(AValue: extended);
begin
  FValue := AValue;
end;

constructor TStringInstance.Create(AValue: string);
begin
  FValue := AValue;
end;

constructor TBooleanInstance.Create(AValue: boolean);
begin
  FValue := AValue;
end;

end.

