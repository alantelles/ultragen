unit DBInstanceClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass,
  sqldb, db, pqconnection;

type
  TDBInstance = class (TInstanceOf)
    protected
      FPGConn: TPQConnection;
    public
      constructor CreatePgConn;
      procedure Connect;
      procedure Disconnect;
      procedure QueryDb;
  end;

implementation

constructor TDBInstance.CreatePgConn;
begin
  inherited Create;
  FPGConn := TPQConnection.Create(nil);
end;
procedure TDBInstance.Connect;
var
  host, database, username, password: string;
begin
  host := TInstanceOf(FMembers.Find('host')).PStrValue;
  database := TInstanceOf(FMembers.Find('name')).PStrValue;
  username := TInstanceOf(FMembers.Find('username')).PStrValue;
  password := TInstanceOf(FMembers.Find('password')).PStrValue;
  FPGConn.Transaction := TSQLTransaction.Create(FPGConn);
  FPGConn.HostName := host;
  FPGConn.UserName := username;
  FPGConn.Password := password;
  FPGConn.DatabaseName := database;
end;

procedure TDBInstance.QueryDb;
var
  Query: TSQLQuery;
  F: TField;
begin
  Query := TSQLQuery.Create(nil);
  Query.DataBase := FPGConn;
  Query.SQL.Text := 'select * from doc_entries';
  Query.Open;
  while not Query.EOF do
  begin
    for F in Query.Fields do
    begin
      write(F.FieldName, ' = ');
      if f.IsNull then
        write('NULL')
      else
        write(F.Value);
      writeln;
    end;
    Query.Next;
  end;
  Query.Close;
  Query.Free;
end;

procedure TDBInstance.Disconnect;
begin
  FPGConn.Free;
end;

end.

