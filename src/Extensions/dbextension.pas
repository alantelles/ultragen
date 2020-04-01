unit DBExtension;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtensionClass, StrUtils, TemplateClass;

const
  SQLITE3_DB = 'sqlite3';

type
  TDBExtension = class
    private
      FPureParams, FParams:TStringList;
      FName :string;
      FTemplate:TTemplate;
    public
      constructor Create(AProc:string; var APureParams:TStringList; var AParams:TStringList; var ATemplate:TTemplate);
      procedure CallProc;
      function CallFunc:string;

      { procedures }
      procedure CreateDatabase;
      procedure CreateTable;
      procedure DirectQuery;


      { functions }
  end;

implementation
uses
  ConstantsGlobals, VariablesGlobals, TypesGlobals, GenFileClass, FileUtil,
  { DB Units }
  SQLDB, sqlite3conn, db;


constructor TDBExtension.Create(AProc:string; var APureParams:TStringList; var AParams:TStringList; var ATemplate:TTemplate);
begin
  FPureParams := APureParams;
  FParams := AParams;
  FTemplate := ATemplate;
  FName := AProc;
end;

procedure TDBExtension.CallProc;
begin
  if FName = 'createTable' then
    CreateTable
  else if FName = 'query' then
    DirectQuery
  else if FName = 'create' then
    CreateDataBase;

end;

function TDBExtension.CallFunc:string;
begin
  Result := '';
end;

{ procedures }
procedure TDBExtension.CreateDatabase;
var
  AConn:TSQLite3Connection;
  ATrans:TSQLTransaction;
  AQuery: TSQLQuery;
  AGen:TGenFile;
  i:integer;
  AQText:string;
  TempText:string='';
begin
  i := FTemplate.GenFileSet.IndexOf(FParams[0]);
  if i > -1 then
  begin
    AGen := TGenFile.Create;
    AConn := TSQLite3Connection.Create(nil);
    ATrans := TSQLTransaction.Create(nil);

    FTemplate.GenFileSet.GenFiles[i].GenFile.CopyGen(AGen);
    AConn.DatabaseName := AGen.GetValue('path').Value;
    AConn.Transaction := ATrans;
    AConn.Connected := True;
    AQuery := TSQLQuery.Create(nil);
    AConn.StartTransaction;
    if AGen.GetValue('database').Value = SQLITE3_DB then
    begin
      AQuery.Transaction := ATrans;
      AQuery.SQL.Text := ';';
      AQuery.ExecSQL;
    end;

    ATrans.Commit;
    AConn.EndTransaction;
    AQuery.Free;
    AConn.Free;
    AGen.Free;
  end;
end;

procedure TDBExtension.CreateTable;
var
  AGen:TGenFile;
  i:integer;
  AQText:string;
  TempText:string='';
  //table settings
begin
  //'CREATE TABLE users (id INTEGER NOT NULL PRIMARY KEY, name VARCHAR(100))';
  i := FTemplate.GenFileSet.IndexOf(FParams[0]);
  AGen := TGenFile.Create;
  FTemplate.GenFileSet.GenFiles[i].GenFile.CopyGen(AGen);
  AGen.Free;
end;

procedure TDBExtension.DirectQuery;
var
  //AConn:TSQLite3Connection;
  AConn:TSQLConnection;
  ATrans:TSQLTransaction;
  AQuery: TSQLQuery;
  F: TField;
  AQ, Val, ADBName, ADBS:string;
  g, i, j:integer;
  Added:boolean=False;
begin
  g := FTemplate.GenFileSet.IndexOf(FParams[0]);
  ADBName := FTemplate.GenFileSet.GenFiles[g].GenFile.GetValue('path').Value;
  ADBS := FTemplate.GenFileSet.GenFiles[g].GenFile.GetValue('database').Value;
  if ADBS = SQLITE3_DB then
    AConn := TSQLite3Connection.Create(nil);
  ATrans := TSQLTransaction.Create(nil);
  AConn.DatabaseName := ADBName;
  AConn.Transaction := ATrans;
  AConn.Connected := True;
  AConn.StartTransaction;
  AQuery := TSQLQuery.Create(nil);
  AQ := Trim(FParams[1]);

  AQuery.DataBase := AConn;
  if FParams.Count > 2 then
  begin
    AQuery.SQL.Text := AQ;
    AQuery.Open;

    j := 0;
    while not AQuery.EOF do
    begin
      if not Added then
      begin
        i := FTemplate.GenFileSet.Add(True, FParams[2]);
        Added := True;
      end;
      for F in AQuery.Fields do
      begin
        if not F.IsNull then
          FTemplate.GenFileSet.GenFiles[i].GenFile.SetValue(IntToStr(j)+GEN_SUB_LEVEL+F.FieldName, F.Value)
        else
          FTemplate.GenFileSet.GenFiles[i].GenFile.SetValue(IntToStr(j)+GEN_SUB_LEVEL+F.FieldName, '');
      end;
      j := j+1;
      AQuery.Next;
    end;
    AQuery.Close;
  end
  else
  begin
    AQuery.SQL.Text := AQ;
    AQuery.ExecSQL;
    ATrans.Commit;
  end;
  AConn.EndTransaction;
  AQuery.Free;
  AConn.Free;
end;


end.

