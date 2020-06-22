unit FlowControlASTClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ASTClass;

type
  TIfConditionBlock = class (TAST)
    private
      FCondition: TAST;
      FBlock: TASTList;
    public
      property PCondition: TAST read FCondition write FCondition;
      property PBlock: TASTList read FBlock write FBlock;
      constructor Create(ABlock: TASTList; ACondition: TAST = nil);
  end;


  TConditional = class (TAST)
    private
      FConditions: TASTList;
    public
      property PConditions: TASTList read FConditions write FConditions;
      constructor Create(AConditions: TASTList);
  end;

  TWhileLoop = class (TAST)
    private
      FBlock: TASTList;
      FCondition: TAST;
    public
      property PCondition: TAST read FCondition write FCondition;
      property PBLock: TASTList read FBLock write FBLock;
      constructor Create(ABlock: TASTList; ACondition: TAST);
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
      constructor Create(ABlock: TASTList; AList: TAST; AVar: TVarAssign);
  end;

implementation

constructor TWhileLoop.Create(ABlock: TASTList; ACondition: TAST);
begin
  FBlock := ABlock;
  FCondition := ACondition;
end;

constructor TForLoop.Create(ABlock: TASTList; AList: TAST; AVar: TVarAssign );
begin
  FBlock := ABlock;
  FList := AList;
  FVar := AVar;
end;

constructor TIfConditionBlock.Create(ABlock: TASTList; ACondition: TAST = nil);
begin
  FCondition := ACondition;
  FBlock := ABlock;
end;

constructor TConditional.Create(AConditions: TASTList);
begin
  FConditions := AConditions;
end;

end.

