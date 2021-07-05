unit DBInstanceClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass, StringInstanceClass, DateTimeInstanceClass,
  sqldb, db, pqconnection, sqlite3conn, ARClass, ListInstanceClass;

type
  TDBInstance = class (TInstanceOf)
    protected
      FPGConn: TSQLConnection;

    public
      constructor CreatePgConn;
      constructor CreateSqLiteConn;
      procedure Connect;
      procedure Disconnect;
      function QueryDb(AQuery: string): TListInstance;
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

function TypeSelect(Value: TField): TInstanceOf;
begin
  write(Value.DataType, ': ');
  if not Value.IsNull then
  begin
    if Value.DataType = ftLargeInt then
      Result := TIntegerInstance.Create(Value.AsInteger)
    else if Value.DataType = ftString then
      Result := TStringInstance.Create(Value.AsString)
    else if VAlue.DataType = ftDateTime then
      Result := TDateTimeInstance.Create(Value.AsDateTime)
    else
      Result := TSTringInstance.Create(Value.AsString);
  end
  else
    Result := TNullInstance.Create;
end;

function TDBInstance.QueryDb(Aquery: string): TListInstance;
var
  Query: TSQLQuery;
  F: TField;
  conns: TStringList;
  AResult: TActivationRecord;
  ResultSet: TListInstance;
begin
  Conns := TSTringList.Create;
  Conns.Free;
  Query := TSQLQuery.Create(nil);
  Query.DataBase := FPGConn;
  Query.SQL.Text := Aquery;
  Query.Open;
  try

    try
      ResultSet := TListInstance.Create;
      while not Query.EOF do
      begin
        AResult := TActivationRecord.Create('ResultSet', AR_DICT, 0);
        for F in Query.Fields do
          AResult.AddMember(F.FieldName, TypeSelect(F));
        ResultSet.Add(TDictionaryInstance.Create(AResult));
        Query.Next;
      end;

    except
      ResultSet.Free;
    end;
    Result := ResultSet;
  finally

  end;
  Query.Close;
  Query.Free;
end;

procedure TDBInstance.Disconnect;
begin
  FPGConn.Free;
end;

end.

