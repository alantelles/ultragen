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
      function AsString: string; virtual;
  end;

  TInstanceList = array of TInstanceOf;



  TNullInstance = class (TInstanceOf)
    protected
      FValue:string;
    public
      property PValue:string read FValue;
      constructor Create;
      function AsString: string;  override;
	end;

  TIntegerInstance = class (TInstanceOf)
    protected
      FValue:integer;
    public
      property PValue:integer read FValue write FValue;
      constructor Create(AValue: integer);
      function AsString: string;  override;
  end;

  TParamList = array of string;

  TFunctionInstance = class (TInstanceOf)
    protected
      FName:string;
      FParamsName:TParamList;
      FParams: TASTList;
      FBlock: TASTList;
      FType: string;
    public
      property PName:string read FName;
      property PType:string read FType;
      //property PParams: TParamList read FParamsName;
      property PParams: TASTList read FParams;
      property PBlock: TASTList read FBlock write FBlock;
      function AsString: string;  override;
      //constructor Create(AName:string);
      constructor Create(AName: string; AParams, ABlock: TASTList; AType:string);
      procedure AddParam(AName:string);
      procedure AddBlock(ABlock: TASTList);
	end;


  TFloatInstance = class (TInstanceOf)
    protected
      FValue:extended;
    public
      property PValue:Extended read FValue write FValue;
      constructor Create(AValue: extended);
      function AsString: string;  override;
  end;

  TBooleanInstance = class (TInstanceOf)
    protected
      FValue: boolean;
    public
      property PValue: boolean read FValue write FValue;
      constructor Create(AValue: boolean);
      function AsString: string;  override;
  end;

implementation
uses
  Tokens;

function TInstanceOf.AsString: string;
begin
  Result := '';
end;

constructor TNullInstance.Create;
begin
  FValue := T_LANG_NULL;
end;

function TNullInstance.AsString: string;
begin
  Result := FValue;
end;

constructor TFunctionInstance.Create(AName:string; AParams, ABlock: TASTList; AType: string);
begin
  //Fname := AName;
  //SetLength(FParamsName, 0);
  FName := AName;
  FParams := AParams;
  FBlock := ABlock;
  FType := AType;
end;

function TFunctionInstance.AsString: string;
begin
  Result := 'function ' + FName;
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

function TIntegerInstance.AsString:string;
begin
  Result := IntToStr(FValue);
end;

constructor TFloatInstance.Create(AValue: extended);
begin
  FValue := AValue;
end;

function TFloatInstance.AsString: string;
var
  AFStr :string;
begin
  AFstr := FloatToStrF(FValue, ffFixed, 30, 30);
  while AFStr[Length(AFStr)] = '0' do
    AFStr := Copy(AFStr, 1, Length(AFStr) - 1);
  Result := AFstr;
end;

constructor TBooleanInstance.Create(AValue: boolean);
begin
  FValue := AValue;
end;

function TBooleanInstance.AsString: string;
var
  Ret: string;
begin
  if FValue then
    Ret := T_LANG_TRUE
  else
    Ret := T_LANG_FALSE;
  Result := Ret;
end;

end.

