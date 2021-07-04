unit DBInstanceClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass,
  sqldb, db, pqconnection, sqlite3conn;

type
  TDBInstance = class (TInstanceOf)
    protected
      FPGConn: TSQLConnection;

    public
      constructor CreatePgConn;
      constructor CreateSqLiteConn;
      procedure Connect;
      procedure Disconnect;
      procedure QueryDb(AQuery: string);
      class function CreateConnection(Atype: integer): TDBInstance;
  end;

implementation

class function TDBInstance.CreateConnection(AType: integer): TDBInstance;
var
  DBConn: TSQLConnection;
  ADBInstance: TDBInstance;
begin
  if AType = 0 then // postgres
    Result := TDBInstance.CreatePgConn
  else if AType = 1 then // sqlite
    Result := TDBInstance.CreateSqLiteConn
  else
    raise Exception.Create('Unknown database driver option');
end;

constructor TDBInstance.CreatePgConn;
begin
  inherited Create;
  FPGConn := TPQConnection.Create(nil);
end;

constructor TDBInstance.CreateSqLiteConn;
begin
  inherited Create;
  FPGConn := TSQLite3Connection.Create(nil);
end;

procedure TDBInstance.Connect;
var
  host, database, username, password: string;
  Port: TInstanceOf;
begin
  host := TInstanceOf(FMembers.Find('host')).PStrValue;
  database := TInstanceOf(FMembers.Find('name')).PStrValue;
  username := TInstanceOf(FMembers.Find('username')).PStrValue;
  password := TInstanceOf(FMembers.Find('password')).PStrValue;
  port := TInstanceOf(FMembers.Find('port'));

  FPGConn.Transaction := TSQLTransaction.Create(FPGConn);
  FPGConn.HostName := host;
  FPGConn.UserName := username;
  FPGConn.Password := password;
  FPGConn.DatabaseName := database;
end;

procedure TDBInstance.QueryDb(Aquery: string);
var
  Query: TSQLQuery;
  F: TField;
  conns: TStringList;
begin
  Conns := TSTringList.Create;
  GetConnectionList(Conns);
  writeln(conns.Text);
  Conns.Free;
  Query := TSQLQuery.Create(nil);
  Query.DataBase := FPGConn;
  Query.SQL.Text := Aquery;
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

