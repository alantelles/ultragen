unit DBInstanceClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass, StringInstanceClass, DateTimeInstanceClass,
  sqldb, DB, pqconnection, sqlite3conn, ARClass, ListInstanceClass;

type
  TQueryResultInstance = class (TInstanceOf)
    public
      constructor Create;
  end;

  TDBInstance = class(TInstanceOf)
  protected
    FPGConn: TSQLConnection;
    FTrans: TSQLTransaction;

  public
    property DBConn: TSQLConnection read FPGConn;
    constructor CreatePgConn;
    constructor CreateSqLiteConn;
    procedure Connect;
    procedure Disconnect;
    function QueryDb(AQuery: string; Params: TInstanceOf = nil): TQueryResultInstance;
    class function CreateConnection(Atype: integer): TDBInstance;
  end;



implementation

class function TDBInstance.CreateConnection(AType: integer): TDBInstance;
var
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
  FTrans := TSQLTransaction.Create(FPGConn);
  FPGConn.Transaction := FTrans;
end;

constructor TQueryResultInstance.Create;
begin
  inherited Create;
  FMembers.Add('rowsAffected', TNullInstance.Create);
  FMembers.Add('results', TNullInstance.Create);
end;

constructor TDBInstance.CreateSqLiteConn;
begin
  inherited Create;
  FPGConn := TSQLite3Connection.Create(nil);
  FTrans := TSQLTransaction.Create(FPGConn);
  FPGConn.Transaction := FTrans;
end;

procedure TDBInstance.Connect;
var
  host: string = '';
  database: string = '';
  username: string = '';
  password: string = '';
  Port: integer = 5432;
  Hold: TObject;
begin
  Hold := FMembers.Find('host');
  if hold <> nil then
    host := TInstanceOf(Hold).PStrValue;

  Hold := FMembers.Find('name');
  if hold <> nil then
    database := TInstanceOf(Hold).PStrValue;

  hold := FMembers.Find('username');
  if Hold <> nil then
    username := TInstanceOf(Hold).PStrValue;

  hold := FMembers.Find('password');
  if hold <> nil then
    password := TInstanceOf(hold).PStrValue;

  hold := FMembers.Find('port');
  if hold <> nil then
    port := TInstanceOf(hold).PIntValue;

  FPGConn.Transaction := TSQLTransaction.Create(FPGConn);
  FPGConn.HostName := host;
  FPGConn.UserName := username;
  FPGConn.Password := password;
  FPGConn.DatabaseName := database;
end;

{ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate,  ftTime, ftDateTime,
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
    ftWideString, ftLargeint, ftADT, ftArray, ftReference,
    ftDataSet, ftOraBlob, ftOraClob, ftVariant, ftInterface,
    ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, ftFixedWideChar, ftWideMemo}

function IsFloatType(DataType: TfieldType): boolean;
begin
  Result := (DataType = ftFloat) or (DataType = ftCurrency) or (DataType = ftBCD);
end;

function IsTextualType(DataType: TFieldType): boolean;
begin
  Result := (DataType = ftString) or (DataType = ftMemo) or
    (DataType = ftFmtMemo) or (DataType = ftMemo) or
    (DataType = ftFixedChar) or (DataType = ftFixedWideChar) or
    (DataType = ftWideMemo);
end;

function IsIntegerType(DataType: TFieldType): boolean;
begin
  Result := (DataType = ftInteger) or (DataType = ftSmallInt) or
    (DataType = ftWord) or (DataType = ftBytes) or (DataType = ftVarBytes) or
    (DataType = ftLargeInt) or (DataType = ftTimestamp);
end;

function TypeSelect(Value: TField): TInstanceOf;
begin
  if not Value.IsNull then
  begin
    if IsIntegerType(Value.DataType) then
      Result := TIntegerInstance.Create(Value.AsInteger)

    else if IsTextualType(Value.DataType) then
      Result := TStringInstance.Create(Value.AsString)

    else if IsFloatType(Value.DataType) then
      Result := TFloatInstance.Create(Value.AsFloat)

    else if Value.DataType = ftDateTime then
      Result := TDateTimeInstance.Create(Value.AsDateTime)

    else if Value.DataType = ftBoolean then
      Result := TBooleanInstance.Create(Value.AsBoolean)

    else
      Result := TSTringInstance.Create(Value.AsString);
  end
  else
    Result := TNullInstance.Create;
end;

procedure TypeParamAdd(Params: TParams; ParamName: string; AInst: TInstanceOf);
var
  AParam: TParam;
begin
  AParam := Params.ParamByName(ParamName);
  if AInst.ClassNameIs('TStringInstance') then
    AParam.AsString := AInst.PStrValue
  else if AInst.ClassNameIs('TIntegerInstance') then
    AParam.AsInteger := AInst.PIntValue
  else if AInst.ClassNameIs('TFloatInstance') then
    AParam.AsFloat := AInst.PFloatValue
  else if AInst.ClassNameIs('TBooleanInstance') then
    AParam.AsBoolean := AInst.PBoolValue
  else if AInst.ClassNameIs('TNullInstance') then
    AParam.AsString := 'NULL'
  else if AInst.ClassNameIs('TDateTimeInstance') then
    AParam.AsDateTime := TDateTimeInstance(AInst).PValue
  else
    AParam.AsString := AInst.AsString;
end;

function TDBInstance.QueryDb(Aquery: string; Params: TInstanceOf = nil): TQueryResultInstance;
var
  Query: TSQLQuery;
  F: TField;
  conns: TStringList;
  AResult, AParams: TActivationRecord;
  ResultSet: TListInstance;
  AResInst: TQueryResultInstance;
  AInst: TInstanceOf;
  AFieldName: string;
  i, len: integer;
begin
  Conns := TStringList.Create;
  Conns.Free;
  Query := TSQLQuery.Create(nil);
  Query.DataBase := FPGConn;
  Query.SQL.Text := Aquery;
  if Params <> nil then
  begin
    if PArams.ClassNameIs('TDictionaryInstance') then
    begin
      AParams := TDictionaryInstance(Params).PValue;
      if AParams.PMembers.Count > 0 then
      begin
        ;
        for i:=0 to AParams.PMembers.Count - 1 do
        begin
          AInst := TInstanceOf(AParams.PMembers[i]);
          AFieldName := AParams.PMembers.NameOfIndex(i);
          TypeParamAdd(Query.Params, AFieldName, AInst);
        end;
      end;
    end;
  end;
  AResInst := TQueryResultInstance.Create;
  if AQuery.StartsWith('select', True) then
  begin
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
        AResinst.Free;
      end;
      AResInst.FMembers.Add('results', ResultSet);

    finally

    end;
    Query.Close;
  end
  else
  begin

    Query.ExecSQL;
    FPGConn.Transaction.Commit;

    AResInst.FMembers.Add('rowsAffected', TIntegerInstance.Create(Query.RowsAffected));
    Query.Close;

  end;
  Result := AResInst;
  Query.Free;
end;

procedure TDBInstance.Disconnect;
begin
  FPGConn.Free;
end;

end.









