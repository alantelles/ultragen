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
    FDBSystem: integer;
  public
    property DBConn: TSQLConnection read FPGConn;
    procedure StartPgConn;
    procedure StartSqLiteConn;
    constructor Create(ASystem: integer);
    procedure StartConnection;
    procedure Disconnect;
    function QueryDb(AQuery: string; Values: TInstanceOf = nil): TQueryResultInstance;
  end;



implementation

procedure TDBInstance.StartConnection;
var
  ADBInstance: TDBInstance;
begin
  if FDBSystem = 0 then // postgres
    StartPgConn
  else if FDBSystem = 1 then
    StartSqLiteConn
  else
    raise Exception.Create('Unknown database driver option');

end;

procedure TDBInstance.StartPgConn;
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
  // end

  FPGConn := TPQConnection.Create(nil);
  FPGConn.HostName := host;
  FPGConn.UserName := username;
  FPGConn.Password := password;
  FPGConn.DatabaseName := database;
  FPGConn.Params.Add('port=' + IntToStr(Port));
  FPGConn.Transaction := TSQLTransaction.Create(FPGConn);
end;

constructor TQueryResultInstance.Create;
begin
  inherited Create;
  FMembers.Add('rowsAffected', TNullInstance.Create);
  FMembers.Add('rows', TNullInstance.Create);

end;

procedure TDBInstance.StartSqLiteConn;
var
  database: string = '';
  Hold: TObject;
begin
  Hold := FMembers.Find('name');
  if hold <> nil then
    database := TInstanceOf(Hold).PStrValue;
  FPGConn := TSQLite3Connection.Create(nil);
  FPGConn.DatabaseName := database;
  FPGConn.Transaction := TSQLTransaction.Create(FPGConn);
end;

constructor TDBInstance.Create(ASystem: integer);
begin
  inherited Create;
  FDBSystem := ASystem;
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
    (DataType = ftLargeInt) or (DataType = ftTimestamp) or (DAtaType = ftAutoInc);
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

procedure ListParamsAdd(Params: TParams; AListInst: TListInstance);
var
  i: integer;
begin
  if Params.Count > 0 then
  begin
    if Params.Count = AListInst.Count then
    begin
      for i:=0 to Params.Count-1 do
        TypeParamAdd(Params, Params[i].Name, AListInst.PValue[i]);
    end
    else
      raise Exception.Create('Different count of parameters and values');

  end;
end;

function ParametrizeQuestionMarks(AQuery: string): string;
var
  len, count, i: integer;
  output: string = '';
begin
  count := 0;
  len := Length(AQuery);
  if len > 0 then
  begin
    for i:=1 to len do
    begin
      if AQuery[i] = '?' then
      begin
        output := output + ':' + IntToStr(count);
        count := count + 1;
      end
      else
        output := output + AQuery[i];
    end;
  end;
  Result := output;
end;

function TDBInstance.QueryDb(Aquery: string; Values: TInstanceOf = nil): TQueryResultInstance;
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

  Query := TSQLQuery.Create(nil);
  Query.DataBase := FPGConn;
  Query.Close;
  Query.SQL.Text := ParametrizeQuestionMarks(Aquery);
  if Values <> nil then
  begin

    if Values.ClassNameIs('TDictionaryInstance') then
    begin
      AParams := TDictionaryInstance(Values).PValue;
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
    end
    else if Values.ClassNameIs('TListInstance') then
    begin
      ListParamsAdd(Query.Params, TListInstance(Values));
    end
    else
      raise Exception.Create('Invalid type for query values');
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
      AResInst.FMembers.Add('rows', ResultSet);

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









