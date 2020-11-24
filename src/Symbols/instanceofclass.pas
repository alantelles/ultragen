unit InstanceOfClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SymbolsClass, SymbolTableClass, ASTClass, ExceptionsClasses,Variants, Contnrs;

type
  TInstanceOf = class
    protected

      FMembers: TFPHashObjectList;
      FSymbol: TSymbol;
      FIntValue: integer;
      FStrValue: string;
      FBoolValue: boolean;
      FFloatValue: extended;
      FCoreType: boolean;
      //FPtrVal: PtrInst;

    public
      //property PPtrVal: PtrInst read FPtrVal write FPtrVal;
      property PMembers: TFPHashObjectList read FMembers write FMembers;
      property PCoreType: boolean read FCoreType write FCoreType;
      property PSymbol: TSymbol read FSymbol;
      property PIntValue: integer read FIntValue write FIntValue;
      property PStrValue: string read FStrValue write FStrValue;
      property PBoolValue: boolean read FBoolValue write FBoolValue;
      property PFloatValue: extended read FFloatValue write FFloatValue;

      constructor Create;
      function AsString: string; virtual;
      procedure CopyInstance(var AReceiver: TInstanceOf); virtual;
  end;

  TInstanceList = array of TInstanceOf;



  TNullInstance = class (TInstanceOf)
    protected
      FValue:string;
    public
      property PValue:string read FValue;
      function AsString: string;  override;
      constructor Create;
      procedure CopyInstance(var AReceiver: TInstanceOf); override;
	end;


  TIntegerInstance = class (TInstanceOf)
    protected
      FValue:integer;
    public
      property PValue:integer read FValue write FValue;
      constructor Create(AValue: integer);
      constructor Create;
      function AsString: string;  override;
      procedure CopyInstance(var AReceiver: TInstanceOf); override;
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

  TDataType = class(TInstanceOf)
    protected
      FValue: string;
      FFrontName: string;
      FConst: TFunctionInstance;
      FuserDef: boolean;
    public
      property PUserDef: boolean read FUserDef;
      property PConst: TFunctionInstance read FConst write FConst;
      property PFrontName: string read FFrontName;
      property PValue: string read FValue write FValue;
      function AsString: string; override;
      constructor Create(AName, AFront: string; UserDefined: boolean=False);
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
      procedure CopyInstance(var AReceiver: TInstanceOf); override;
  end;

  TBooleanInstance = class (TInstanceOf)
    protected
      FValue: boolean;
    public
      property PValue: boolean read FValue write FValue;
      constructor Create(AValue: boolean);
      function AsString: string;  override;
      procedure CopyInstance(var AReceiver: TInstanceOf); override;
  end;

  TClassInstance = class (TInstanceOf)
    protected
      FValue: string;
      FConst: TFunctionInstance;
    public
      property PValue: string read FValue;
      constructor Create(AName: string);
      function AsString: string;  override;

	end;

implementation
uses
  Tokens, StringInstanceClass;

constructor TInstanceOf.Create;
begin
  // FCoreType := True;
  FMembers := TFPHashObjectList.Create(True);
end;

constructor TClassInstance.Create(AName: string);
begin
  inherited Create;
  FValue := AName;
end;

function TClassInstance.AsString: string;
begin
  Result := '<Instance of class '+FValue+'>';
end;

procedure TClassInstance.CopyInstance(var AReceiver: TInstanceOf);
begin
  AReceiver := TClassInstance.Create(FValue)
  inherited CopyInstance(AReceiver);
end;

procedure TInstanceOf.CopyInstance(var AReceiver: TInstanceOf);
var 
  i: integer;
  Aux: TInstanceOf;
begin
  if FMembers.Count > 0 then
  begin
    for i:=0 to FMembers.Count - 1 do
    begin
      Aux := TInstanceOf.Create;
      FMembers[i].CopyInstance(Aux);
      AReceiver.FMembers.Add(FMembers.NameOfIndex(i), Aux)
    end;
  end;  
  
end;

constructor TDataType.Create(AName: string; AFront: string; UserDefined: boolean=False);
begin
  inherited Create;
  Fuserdef:=UserDefined;
  FMembers.Add('internal', TStringInstance.Create(Aname));
  FValue := AName;
  FFrontName := AFront;
end;

function TDataType.AsString: string;
begin
  Result := '<DataType: '+FFrontName+', internal name: '+FValue+'>';
end;

function TInstanceOf.AsString: string;
begin
  Result := '';
end;

constructor TNullInstance.Create;
begin
  FCoreType := True;
end;

procedure TNullInstance.CopyInstance(var AReceiver: TInstanceOf);
begin
  AReceiver := TNullInstance.Create;
end;

function TNullInstance.AsString: string;
begin
  Result := T_LANG_NULL;
end;

constructor TFunctionInstance.Create(AName:string; AParams, ABlock: TASTList; AType: string; IsBuiltin:boolean; FromNamespace: boolean = False);
begin
  //Fname := AName;
  //SetLength(FParamsName, 0);
  inherited Create;
  FFromNamespace := FromNamespace;
  FName := AName;
  FParams := AParams;
  FBlock := ABlock;
  FType := AType;
  FIsBuiltin := IsBuiltin;
end;

function TFunctionInstance.AsString: string;
begin
  if FType <> '' then
    Result := '<function ' + FName + ' from type ' + FType + '>'
  else
    Result := '<function ' + FName + '>';
end;

constructor TIntegerInstance.Create(AValue: integer);
begin
  inherited Create;
  FValue := AValue;
  FIntValue := AValue;
  FCoreType := True;
end;

constructor TIntegerInstance.Create;
begin
  FCoreType := True;
end;

procedure TIntegerInstance.CopyInstance(var AReceiver: TInstanceOf);
var
  Cast: TIntegerInstance;
begin
  Cast := TIntegerInstance.Create(FValue);
  AReceiver := Cast;
end;

function TIntegerInstance.AsString:string;
begin
  Result := IntToStr(FValue);
end;

constructor TFloatInstance.Create(AValue: extended);
begin
  inherited Create;
  try
    FValue := AValue;
    FFloatValue := FValue;
  finally
    FValue := AValue * 1.0;
    FFloatValue := FValue;
  end;
  FCoreType := True;
end;

procedure TFloatInstance.CopyInstance(var AReceiver: TInstanceOf);
var
  Cast: TFloatInstance;
begin
  Cast := TFloatInstance.Create(FValue);
  AReceiver := Cast;
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
  inherited Create;
  FValue := AValue;
  FBoolValue := AValue;
  FCoreType := True;
end;

procedure TBooleanInstance.CopyInstance(var AReceiver: TInstanceOf);
var
  Cast: TBooleanInstance;
begin
  Cast := TBooleanInstance.Create(FValue);
  AReceiver := Cast;
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