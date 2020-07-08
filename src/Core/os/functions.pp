function TCoreFunction.GetAllFiles: TListInstance;
var
  AList: TStringList;
  APath, F: string;
  ARet: TListInstance;
begin
  if Length(FParams) <> 1 then
    raise EArgumentsError.Create(E_INVALID_ARGS);
  APath := TStringInstance(FParams[0]).PValue;
  try
    AList := TStringList.Create;
    try
      FindAllFiles(AList, APath);
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
