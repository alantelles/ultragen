unit SymbolTableClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SymbolsClass, Contnrs, Tokens, LoggingClass;

type
  TSymbolHashTable = class (TFPObjectHashTable)
    protected
      AItem: TSymbol;
  end;

  TSymbolTable = class
    private
      FSymbols: TSymbolHashTable;
      FScope: string;
      FLevel: integer;
      FParentScope: TSymbolTable;
    public
      property PScope:string read FScope;
      property PLevel:integer read FLevel write FLevel;
      property PParentScope: TSymbolTable read FParentScope;
      constructor Create(AScopeName: string; AScopeLevel: integer; AParentScope: TSymbolTable = nil);
      procedure SetBuiltins;
      procedure AsString;
      procedure SymbolAsString(AItem:  TObject;const AName:string; var Cont:boolean);
      function Define(ASymbol: TSymbol):boolean;
      function Lookup(AName: string):TSymbol;

  end;

implementation
constructor TSymbolTable.Create(AScopeName: string; AScopeLevel: integer; AParentScope: TSymbolTable = nil);
begin
  FSymbols := TSymbolHashTable.Create;
  FScope := AScopeName;
  FLevel := AScopeLevel;
  FParentScope := AParentScope;
  if FParentScope = nil then
  begin
    SetBuiltins;
	end;
end;

function TSymbolTable.Define(ASymbol: TSymbol):boolean;
var
  len, i:integer;
  Ret: boolean = True;
begin
  if FSymbols[ASymbol.PName] <> nil then
  begin
    FSymbols[ASymbol.PName] := ASymbol;
    ret := False;
  end
  else
    FSymbols.Add(ASymbol.PName, ASymbol);
  Result := ret;
end;

procedure TSymbolTable.SetBuiltins;
begin
  FSymbols.Add(TYPE_integer, TBuiltinSymbol.Create(TYPE_integer));
  FSymbols.Add(TYPE_FLOAT, TBuiltinSymbol.Create(TYPE_FLOAT));
  FSymbols.Add(TYPE_STRING, TBuiltinSymbol.Create(TYPE_STRING));
  FSymbols.Add(TYPE_BOOLEAN, TBuiltinSymbol.Create(TYPE_BOOLEAN));
  FSymbols.Add(T_FUNC_DEF, TBuiltinSymbol.Create(T_FUNC_DEF));

end;


procedure TSymbolTable.SymbolAsString(AItem: TObject;const AName:string; var Cont:boolean);
var
  ACast, ATest: TSymbol;

begin
  ACast := TSymbol(AItem);
  writeLn('Item '+ACast.PName+ ' from type '+ACast.ClassName+' in scope '+ FScope);
end;

function TSymbolTable.Lookup(AName: string):TSymbol;
var
  Ret: TSymbol = nil;
begin
  Ret := TSymbol(FSymbols[AName]);
  if Ret <> nil then
  begin
    logdebug('Symbol '+ AName +' found at '+FScope, 'SymbolTable');
    Result := Ret
  end
  else if FParentScope <> nil then
    Result := FParentScope.Lookup(AName);
end;

procedure TSymbolTable.AsString;
var
  AIter: TSymbol;
begin
  FSymbols.Iterate(@SymbolAsString);
end;

end.

