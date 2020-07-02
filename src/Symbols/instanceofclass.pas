unit InstanceOfClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SymbolsClass, SymbolTableClass, ASTClass, ExceptionsClasses;

type
  TInstanceOf = class
    protected
      FSymbol: TSymbol;

    public
      property PSymbol: TSymbol read FSymbol;
  end;

  TInstanceList = array of TInstanceOf;



  TNullInstance = class (TInstanceOf)
    protected
      FValue:string;
    public
      property PValue:string read FValue;
      constructor Create;
	end;

  TIntegerInstance = class (TInstanceOf)
    protected
      FValue:integer;
    public
      property PValue:integer read FValue write FValue;
      constructor Create(AValue: integer);
  end;

  TParamList = array of string;

  TFunctionInstance = class (TInstanceOf)
    protected
      FName:string;
      FParamsName:TParamList;
      FBlock: TASTList;
    public
      property PName:string read FName;
      property PParams: TParamList read FParamsName;
      property PBlock: TASTList read FBlock write FBlock;
      constructor Create(AName:string);
      procedure AddParam(AName:string);
      procedure AddBlock(ABlock: TASTList);
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
uses
  Tokens;

constructor TNullInstance.Create;
begin
  FValue := '';
end;

constructor TFunctionInstance.Create(AName:string);
begin
  Fname := AName;
  SetLength(FParamsName, 0);
end;

procedure TFunctionInstance.AddParam(AName:string);
var
  i,len:integer;
begin
  len :=  Length(FParamsName);
  len := len + 1;
  SetLength(FParamsName, len);
  FParamsName[len - 1] := Aname;
end;

procedure TFunctionInstance.AddBlock(ABlock:TASTList);
var
  i,len:integer;
begin
  FBlock := ABlock;
end;


constructor TIntegerInstance.Create(AValue: integer);
begin
  FValue := AValue;
end;

constructor TFloatInstance.Create(AValue: extended);
begin
  FValue := AValue;
end;

constructor TBooleanInstance.Create(AValue: boolean);
begin
  FValue := AValue;
end;

end.

