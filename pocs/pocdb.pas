program pocdb;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, Sysutils,
  { you can add units after this }
  SQLDB, sqlite3conn, StrUtils, db;

var
  AConn:TSQLite3Connection;
  ATrans:TSQLTransaction;
  AQuery: TSQLQuery;
  AList: TStrings;
  F:TField;

begin
  AConn := TSQLite3Connection.Create(nil);
  ATrans := TSQLTransaction.Create(nil);
  AConn.DatabaseName := 'poc.db';
  AConn.Transaction := ATrans;
  AConn.Connected := True;
  AQuery := TSQLQuery.Create(nil);

  //AQuery.SQL.Text := 'CREATE TABLE users (id INTEGER NOT NULL PRIMARY KEY, name VARCHAR(100))';
  if ParamStr(1) = 'insert' then
  begin
    AConn.StartTransaction;
    AQuery.Transaction := ATrans;
    AQuery.SQL.Text := 'INSERT INTO users (id, name) VALUES (:id, :name)';
    AQuery.Params.ParamByName('id').AsInteger := StrToInt(ParamStr(2));
    AQuery.Params.ParamByName('name').AsString := ParamStr(3);
    AQuery.ExecSQL;
    ATrans.Commit;
  end
  else if ParamStr(1) = 'delete' then
  begin
    AConn.StartTransaction;
    AQuery.Transaction := ATrans;
    AQuery.SQL.Text := 'DELETE FROM users WHERE ('+ParamStr(2)+' = :'+ParamStr(2)+')';
    AQuery.Params.ParamByName('id').AsInteger := StrToInt(ParamStr(3));
    AQuery.ExecSQL;
    ATrans.Commit;
  end
  else if ParamStr(1) = 'update' then
  begin
    AConn.StartTransaction;
    AQuery.Transaction := ATrans;
    // update name old new
    AQuery.SQL.Text := 'UPDATE users SET '+ParamStr(2)+' = "'+ParamStr(4)+'" WHERE ('+ParamStr(2)+' = :'+ParamStr(2)+')';
    AQuery.Params.ParamByName('name').AsString := ParamStr(3);
    AQuery.ExecSQL;
    ATrans.Commit;
  end
  else if ParamStr(1) = 'select' then
  begin
    AQuery.DataBase := AConn;
    if ParamStr(2) = 'all' then
      AQuery.SQL.Text := 'SELECT * FROM users'
    else if ParamCount > 2 then
    begin
      AQuery.SQL.Text := 'SELECT * FROM users WHERE (' + ParamStr(2) + ' = :'+Paramstr(2)+' )';
      AQuery.Params.ParamByName(ParamStr(2)).AsString := ParamStr(3);
    end;
    AQuery.Open;
    while not AQuery.EOF do
    begin
      for F in AQuery.Fields do
      begin
        write(F.FieldName + ' = ');
        if F.IsNull then
          Write('NULL')
        else
          Write(F.Value);
          Write(', ');
      end;
      WriteLn;
      AQuery.Next;
    end;


    AQuery.Close;
  end;

  AConn.EndTransaction;
  AQuery.Free;
  AConn.Free;
end.
