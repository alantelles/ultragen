procedure TCoreFunction.CopyStream;
var
  AStream: TMemoryStream;
  FileIn, FileOut: string;
begin
  {FileIn := TStringInstance(FParams[0]).Pvalue;
  FileOut := TStringInstance(FParams[1]).Pvalue;
  AStream := TMemoryStream.Create;
  AStream.LoadFromFile(FileIn);
  AStream.SaveToFile(FileOut);
  AStream.Free;}
  if Length(FParams) = 2 then
    CopyFile(FileIn, FileOut, [cffPreserveTime, cffOverwriteFile], True);
end;

function TCoreFunction.GetAllFiles: TListInstance;
var
  AList: TStringList;
  APath, F: string;
  AFilter: string = '*';
  ASub: boolean = False;
  ARet: TListInstance;
  len: integer;
begin
  len := Length(FParams);
  if (Len > 3) or (Len < 1) then
    raise EArgumentsError.Create(E_INVALID_ARGS);
  APath := TStringInstance(FParams[0]).PValue;
  if (len = 2) then
  begin
    if FParams[1].ClassNameIs('TBooleanInstance') then
      ASub := TBooleanInstance(FParams[1]).PValue
    else if FParams[1].ClassNameIs('TStringInstance') then
      AFilter := TStringInstance(FParams[1]).PValue
    else
      raise EArgumentsError.Create(E_INVALID_ARGS_TYPE);
  end;
  if (len = 3) then
  begin
    ASub := TBooleanInstance(FParams[2]).PValue;
    AFilter := TStringInstance(FParams[1]).PValue
  end;
  if (len > 2) then
    ASub := TBooleanInstance(FParams[2]).PValue;
  try
    AList := TStringList.Create;
    try
      FindAllFiles(AList, APath, AFilter, ASub);
      ARet := TListInstance.Create;
      for F in AList do
        ARet.Add(TStringInstance.Create(F));
      Result := ARet;
    except
      on E: Exception do raise ERunTimeError.Create(E.Message);
    end;
  finally
    AList.Free;
  end;
end;

function TCoreFunction.CreateDirOpt: TBooleanInstance;
var
  len :integer;
begin
  len := Length(FParams);
  if (len > 2) or (len < 1) then
    raise EArgumentsError.Create(E_INVALID_ARGS);
  if not FParams[0].ClassNameIs('TStringInstance') then
    raise EArgumentsError.Create(E_INVALID_ARGS_TYPE);
  if len = 2 then
  begin
    if FParams[1].ClassNameIs('TBooleanInstance') then
    begin
      if TBooleanInstance(FParams[1]).PValue then
        Result := TBooleanInstance.Create(ForceDirectories(TStringInstance(FParams[0]).PValue))
      else
        Result := TBooleanInstance.Create(CreateDir(TStringInstance(FParams[0]).PValue));
    end
    else
      raise EArgumentsError.Create(E_INVALID_ARGS_TYPE);
  end
  else
    Result := TBooleanInstance.Create(CreateDir(TStringInstance(FParams[0]).PValue));
end;
