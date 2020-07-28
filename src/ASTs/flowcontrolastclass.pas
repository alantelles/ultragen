unit FlowControlASTClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ASTClass, TokenClass;

type
  TIfConditionBlock = class (TAST)
    private
      FCondition: TAST;
      FBlock: TASTList;
    public
      property PCondition: TAST read FCondition write FCondition;
      property PBlock: TASTList read FBlock write FBlock;
      constructor Create(ABlock: TASTList; ACondition: TAST; AToken: TToken);
      destructor Destroy; override;
  end;


  TConditional = class (TAST)
    private
      FConditions: TASTList;
    public
      property PConditions: TASTList read FConditions write FConditions;
      constructor Create(AConditions: TASTList; AToken: TToken);
      destructor Destroy; override;
  end;

  TBreakLoop = class(TAST)
	end;

  TContinueLoop = class(TAST)
	end;

  TReturnFunction = class(TAST)
    private
      FValue: TAST;
    public
      property PValue: TAST read FValue write FValue;
      constructor Create(AValue: TAST; AToken: TToken);
      destructor Destroy; override;
  end;
                                                                             {todo}
  TWhileLoop = class (TAST)
    private
      FBlock: TASTList;
      FCondition: TAST;
    public
      property PCondition: TAST read FCondition write FCondition;
      property PBLock: TASTList read FBLock write FBLock;
      constructor Create(ABlock: TASTList; ACondition: TAST; AToken: TToken);
      destructor Destroy; override;
  end;

  TForLoop = class (TAST)
    private
      FBlock: TASTList;
      FList: TAST;
      FVar: string;
    public
      property PBlock: TASTList read FBlock write FBlock;
      property PList: TAST read FList write FList;
      property PVar: string read FVar write FVar;
      constructor Create(ABlock: TASTList; AList: TAST; AControlVar: string; AToken: TToken);
      destructor Destroy; override;
  end;

implementation

constructor TReturnFunction.Create(AValue: TAST; AToken: TToken);
begin
  FValue := AValue;
  FToken := AToken;
end;

destructor TReturnFunction.Destroy;
begin
  FVAlue.Free;
  inherited;
end;

constructor TWhileLoop.Create(ABlock: TASTList; ACondition: TAST; AToken: TToken);
begin
  FBlock := ABlock;
  FCondition := ACondition;
  FToken := AToken;
end;

destructor TWhileLoop.Destroy;
var
  len, i: integer;
begin
  len := Length(FBlock);
  if len > 0 then
  begin
    for i:=0 to len-1 do
    begin
      FBlock[i].Free;
    end;
  end;
  FCondition.Free;
  inherited;
end;

constructor TForLoop.Create(ABlock: TASTList; AList: TAST; AControlVar: string; AToken: TToken);
begin
  FBlock := ABlock;
  FList := AList;
  FVar := AControlVar;
  FToken := AToken;
end;

destructor TForLoop.Destroy;
var
  len, i: integer;
begin
  len := Length(FBlock);
  if len > 0 then
  begin
    for i:=0 to len-1 do
    begin
      FBlock[i].Free;
    end;
  end;
  FList.Free;
  inherited;
end;

constructor TIfConditionBlock.Create(ABlock: TASTList; ACondition: TAST; AToken: TToken);
begin
  FCondition := ACondition;
  FBlock := ABlock;
  FToken := AToken;
end;

destructor TIfConditionBlock.Destroy;
var
  len, i: integer;
begin
  len := Length(FBlock);
  if len > 0 then
  begin
    for i:=0 to len-1 do
    begin
      FBlock[i].Free;
    end;
  end;
  FCondition.Free;
  inherited;
end;

constructor TConditional.Create(AConditions: TASTList; AToken: TToken);
begin
  FConditions := AConditions;
  FToken := AToken;
end;

destructor TConditional.Destroy;
var
  len, i: integer;
begin
  len := Length(FConditions);
  if len > 0 then
  begin
    for i:=0 to len-1 do
    begin
      FConditions[i].Free;
    end;
  end;
  inherited;
end;

end.

