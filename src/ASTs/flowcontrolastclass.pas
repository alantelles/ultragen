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
  end;


  TConditional = class (TAST)
    private
      FConditions: TASTList;
    public
      property PConditions: TASTList read FConditions write FConditions;
      constructor Create(AConditions: TASTList; AToken: TToken);
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
  end;

  TForLoop = class (TAST)
    private
      FBlock: TASTList;
      FList: TAST;
      FVar: TVarAssign;
    public
      property PBlock: TASTList read FBlock write FBlock;
      property PList: TAST read FList write FList;
      property PVar: TVarAssign read FVar write FVar;
      constructor Create(ABlock: TASTList; AList: TAST; AVar: TVarAssign; AToken: TToken);
  end;

implementation

constructor TReturnFunction.Create(AValue: TAST; AToken: TToken);
begin
  FValue := AValue;
  FToken := AToken;
end;

constructor TWhileLoop.Create(ABlock: TASTList; ACondition: TAST; AToken: TToken);
begin
  FBlock := ABlock;
  FCondition := ACondition;
  FToken := AToken;
end;

constructor TForLoop.Create(ABlock: TASTList; AList: TAST; AVar: TVarAssign; AToken: TToken);
begin
  FBlock := ABlock;
  FList := AList;
  FVar := AVar;
  FToken := AToken;
end;

constructor TIfConditionBlock.Create(ABlock: TASTList; ACondition: TAST; AToken: TToken);
begin
  FCondition := ACondition;
  FBlock := ABlock;
  FToken := AToken;
end;

constructor TConditional.Create(AConditions: TASTList; AToken: TToken);
begin
  FConditions := AConditions;
  FToken := AToken;
end;

end.

