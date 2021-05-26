unit InstanceOfClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SymbolsClass, SymbolTableClass, ASTClass, ExceptionsClasses,Variants, Contnrs, BrookHTTPUploads;

type

  TInstanceOf = class
    protected

      FMembers: TFPHashObjectList;
      FSymbol: TSymbol;
      FError: boolean;
      FErrorMsg: string;
      FIntValue: integer;
      FStrValue: string;
      FBoolValue: boolean;
      FFloatValue: extended;
      FCoreType: boolean;

      //FPtrVal: PtrInst;

    public
      //property PPtrVal: PtrInst read FPtrVal write FPtrVal;
      property PError: boolean read FError write FError;
      property PErrorMsg: string read FErrorMsg write FErrorMsg;
      property PMembers: TFPHashObjectList read FMembers write FMembers;
      property PCoreType: boolean read FCoreType write FCoreType;
      property PSymbol: TSymbol read FSymbol;
      property PIntValue: integer read FIntValue write FIntValue;
      property PStrValue: string read FStrValue write FStrValue;
      property PBoolValue: boolean read FBoolValue write FBoolValue;
      property PFloatValue: extended read FFloatValue write FFloatValue;

      constructor Create;
      function AsString: string; virtual;
      procedure ValueBuilder; virtual;
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

  TByteInstance = class (TInstanceOf)
      protected
      FValue:byte;
    public
      property PValue:byte read FValue write FValue;
      constructor Create(AValue: byte);
      constructor Create;
      function AsString: string;  override;
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
      FAccVarargs: boolean;
      FIsDecorator: boolean;
      FDecorParams: TASTList;
    public
      property PAccVarargs:boolean read FAccVarargs write FAccVarargs;
      property PIsDecorator: boolean read FIsDecorator;
      property PName:string read FName write FName;
      property PType:string read FType;
      property PIsBuiltin: boolean read FIsBuiltin;
      property PParams: TASTList read FParams;
      property PDecorParams: TASTList read FDecorParams write FDecorParams;
      property PBlock: TASTList read FBlock write FBlock;
      function AsString: string;  override;
      constructor Create(AName: string; AParams, ABlock: TASTList; AType:string; IsBuiltin:boolean; IsDecorator: boolean; AcceptVarargs: boolean);
      //procedure AddParam(AName:string);
      //procedure AddBlock(ABlock: TASTList);
	end;

  TDecoratorInstance = class(TFunctionInstance)

  end;



  TDataType = class(TInstanceOf)
    protected
      FValue: string;
      FFrontName: string;
      FConst: TFunctionInstance;
      FuserDef: boolean;
    public
      property PUserDef: boolean read FUserDef write FUserDef;
      property PConst: TFunctionInstance read FConst write FConst;
      property PFrontName: string read FFrontName write FFrontName;
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

  TBrookUploadedInstance = class(TInstanceOf)
    protected
      FValue: TBrookHTTPUpload;
    public
      constructor Create(AHandler: TBrookHTTPUpload);
      function SaveFile(FileName: string): TBooleanInstance;
      function AsString: string;  override;
  end;

  TClassInstance = class (TInstanceOf)
    protected
      FValue: string;
      FConst: TFunctionInstance;
    public
      property PValue: string read FValue;
      constructor Create(AName: string);
      function AsString: string;  override;
 //     procedure CopyInstance(var AReceiver: TInstanceOf);

	end;

implementation
uses
  Tokens, StringInstanceClass;

constructor TBrookUploadedInstance.Create(AHandler: TBrookHTTPUpload);
begin
  inherited Create;
  FValue := AHandler;
end;

function TBrookUploadedInstance.SaveFile(FileName: string): TBooleanInstance;
var
  e: string;
  res: boolean;
begin
  res := FValue.SaveAs(FileName, e);

  Result := TBooleanInstance.Create(res);
end;

function TBrookUploadedInstance.AsString: string;
begin
  Result := '<Uploaded file ' + FValue.Name + ' in field ' + FValue.Field + '>';
end;

constructor TInstanceOf.Create;
begin
  // FCoreType := True;
  FMembers := TFPHashObjectList.Create(True);
  FError := False;
  FErrorMsg := '';
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

{
procedure TClassInstance.CopyInstance(var AReceiver: TInstanceOf);
begin
  AReceiver := TClassInstance.Create(FValue);
  inherited CopyInstance(AReceiver);
end;
}
procedure TInstanceOf.CopyInstance(var AReceiver: TInstanceOf);
var 
  i: integer;
  Aux: TInstanceOf;
begin
  {if FMembers.Count > 0 then
  begin
    for i:=0 to FMembers.Count - 1 do
    begin
      Aux := TInstanceOf.Create;
      TInstanceOf(FMembers[i]).CopyInstance(Aux);
      AReceiver.FMembers.Add(FMembers.NameOfIndex(i), Aux)
    end;
  end;  }
  AReceiver := self;
  
end;

constructor TDataType.Create(AName: string; AFront: string; UserDefined: boolean=False);
begin
  inherited Create;
  Fuserdef:=UserDefined;
  FMembers.Add('$internal', TStringInstance.Create(Aname));
  FValue := AName;
  FFrontName := AFront;
end;

function TDataType.AsString: string;
begin
  Result := '<DataType: '+FFrontName+', internal name: '+FValue+'>';
end;

function TInstanceOf.AsString: string;
begin
  Result := '<Instance of ' + ClassName + '>';
end;

procedure TInstanceOf.ValueBuilder;
begin

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

constructor TFunctionInstance.Create(AName: string; AParams, ABlock: TASTList; AType:string; IsBuiltin:boolean; IsDecorator: boolean; AcceptVarargs: boolean);
begin
  //Fname := AName;
  //SetLength(FParamsName, 0);
  inherited Create;
  FAccVarargs := AcceptVarargs;
  FName := AName;
  FParams := AParams;
  FBlock := ABlock;
  FType := AType;
  FIsBuiltin := IsBuiltin;
  FIsDecorator := IsDecorator;
  FDecorParams := nil;
end;



function TFunctionInstance.AsString: string;
var
  AMode: string = 'function';
begin
  if FIsDecorator then
    AMode := 'decorator ' + AMode;
  if FType <> '' then
    Result := '<' + AMode + ' ' + FName + ' from type ' + FType + '>'
  else
    Result := '<' + AMode + ' ' + FName + '>';
end;


constructor TByteInstance.Create(AValue: byte);
begin
  inherited Create;
  FValue := AValue;
  FIntValue := AValue;
  FCoreType := True;
end;

constructor TByteInstance.Create;
begin
  FCoreType := True;
end;

procedure TByteInstance.CopyInstance(var AReceiver: TInstanceOf);
var
  Cast: TByteInstance;
begin
  Cast := TByteInstance.Create(FValue);
  AReceiver := Cast;
end;

function TByteInstance.AsString:string;
begin
  Result := IntToStr(FValue);
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
