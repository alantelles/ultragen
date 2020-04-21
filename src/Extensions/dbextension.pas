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
      procedure DirectParametrized;


      { functions }
  end;

implementation
uses
  ConstantsGlobals, VariablesGlobals, TypesGlobals, GenFileClass, FileUtil,
  Dateutils,
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
  else if FName = 'paramQuery' then
    DirectParametrized
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

begin
  FTemplate.ParseTokens([], FParams);
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

  TempText:string='';
  //table settings
begin
  //'CREATE TABLE users (id INTEGER NOT NULL PRIMARY KEY, name VARCHAR(100))';
  i := FTemplate.GenFileSet.IndexOf(FParams[0]);
  AGen := TGenFile.Create;
  FTemplate.GenFileSet.GenFiles[i].GenFile.CopyGen(AGen);
  AGen.Free;
end;

procedure TDBExtension.DirectParametrized;
var
  //AConn:TSQLite3Connection;
  AConn:TSQLConnection;
  ATrans:TSQLTransaction;
  AQuery: TSQLQuery;
  F: TField;
  AType, AField, AValue, AnAlias:string;
  AQ, Val, ADBName, ADBS:string;
  g, i, j:integer;
  Added:boolean=False;
  AGenFile:TGenFile;
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

  AQuery := TSQLQuery.Create(nil);
  AQuery.DataBase := AConn;
  AQ := Trim(FParams[1]);
  AQuery.SQL.Text := AQ;
  AQuery.Prepare;
  //set query params
  AGenFile := TGenFile.Create;
  for i:=0 to Length(FTemplate.GenFileSet.GenFiles)-1 do
  begin
    AnAlias := FTemplate.GenFileSet.GenFiles[i].GenAlias;
    if Pos(FParams[2], AnAlias) > 0 then
    begin
      AGenFile.ClearValues;
      FTemplate.GenFileSet.GenFiles[i].GenFile.CopyGen(AGenFile);
      AType := AGenFile.GetValue('type').Value;
      AValue := AGenFile.GetValue('value').Value;
      AField := AGenFile.GetValue('field').Value;
      if Lowercase(AType) = 'string' then
        AQuery.Params.ParamByName(AField).AsString := AValue
      else if Lowercase(AType) = 'integer' then
        AQuery.Params.ParamByName(AField).AsInteger := StrToInt(AValue)
      else if Lowercase(AType) = 'datetime' then
        AQuery.Params.ParamByName(AField).AsDateTime := ScanDateTime('yyyy-mm-dd hh:nn:ss', AValue)
      else if Lowercase(AType) = 'time' then
        AQuery.Params.ParamByName(AField).AsTime := ScanDateTime('hh:nn:ss', AValue)
      else if Lowercase(AType) = 'date' then
        AQuery.Params.ParamByName(AField).AsDate := ScanDateTime('yyyy-mm-dd', AValue);
    end;
  end;
  AGenFile.Free;
  if FParams.Count > 3 then
  begin

    AQuery.Open;

    j := 0;
    while not AQuery.EOF do
    begin
      if not Added then
      begin
        i := FTemplate.GenFileSet.Add(True, FParams[3]);
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
    AConn.StartTransaction;
    AQuery.ExecSQL;
    ATrans.Commit;
    AConn.EndTransaction;
  end;

  AQuery.Free;
  AConn.Free;
end;

procedure TDBExtension.DirectQuery;
var
  //AConn:TSQLite3Connection;
  AConn:TSQLConnection;
  ATrans:TSQLTransaction;
  AQuery: TSQLQuery;
  F: TField;
  AQ, Val, ADBName, ADBS, AnAlias:string;

  g, i, j:integer;
  Added:boolean=False;
  //[0] = connection
  //[1] = query
  //[2] = prefix
  //[3] = results
begin
  FTemplate.ParseTokens([], FParams);
  AnAlias := FParams[0];
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

  //end set query params

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

