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
      constructor Create;
      function AsString: string;  override;
  end;

  TBuiltInType = class(TInstanceOf)
    protected
      FValue: string;
    public
      property PValue: string read FValue write FValue;
      function AsString: string; override;
      constructor Create(ANAme: string);
  end;


  TParamList = array of string;

  TFunctionInstance = class (TInstanceOf)
    protected
      FName:string;
      FParams: TASTList;
      FBlock: TASTList;
      FType: string;
      FIsBuiltin: boolean;
      FFromNamespace: boolean;
    public
      property PFromNamespace:boolean read FFromNamespace write FFromNamespace;
      property PName:string read FName write FName;
      property PType:string read FType;
      property PIsBuiltin: boolean read FIsBuiltin;
      property PParams: TASTList read FParams;
      property PBlock: TASTList read FBlock write FBlock;
      function AsString: string;  override;
      constructor Create(AName: string; AParams, ABlock: TASTList; AType:string; IsBuiltin:boolean; FromNamespace: boolean=False);
      //procedure AddParam(AName:string);
      //procedure AddBlock(ABlock: TASTList);
	end;




  TFloatInstance = class (TInstanceOf)
    protected
      FValue:extended;
      FValueInt: integer;
    public
      property PValueInt: integer read FValueInt write FValueInt;
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

constructor TBuiltInType.Create(AName: string);
begin
  FValue := AName;
end;

function TBuiltInType.AsString: string;
begin
  Result := FValue;
end;

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

constructor TFunctionInstance.Create(AName:string; AParams, ABlock: TASTList; AType: string; IsBuiltin:boolean; FromNamespace: boolean = False);
begin
  //Fname := AName;
  //SetLength(FParamsName, 0);
  FFromNamespace := FromNamespace;
  FName := AName;
  FParams := AParams;
  FBlock := ABlock;
  FType := AType;
  FIsBuiltin := IsBuiltin;
end;

function TFunctionInstance.AsString: string;
begin
  Result := 'function ' + FName + ' from type ' + FType;
end;

constructor TIntegerInstance.Create(AValue: integer);
begin
  FValue := AValue;
end;

constructor TIntegerInstance.Create;
begin
end;

function TIntegerInstance.AsString:string;
begin
  Result := IntToStr(FValue);
end;

constructor TFloatInstance.Create(AValue: extended);
begin
  try
    FValue := AValue;

  finally
    FValue := AValue * 1.0;
  end;
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

