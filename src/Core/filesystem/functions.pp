procedure TCoreFunction.FSCopyFile;
var
  FileIn, FileOut: string;
begin
  checkArgCount([2,3]);
  checkArgTypes(['TStringInstance', 'TStringInstance', 'TBooleanInstance']);
  FileIn := TStringInstance(FParams[0]).Pvalue;
  FileOut := TStringInstance(FParams[1]).Pvalue;

  if Length(FParams) = 2 then
    CopyFile(FileIn, FileOut, [cffPreserveTime, cffOverwriteFile], True)
  else if Length(FParams) = 3 then
  begin
    if FParams[2].PBoolValue then
      CopyFile(FileIn, FileOut, [cffPreserveTime, cffOverwriteFile, cffCreateDestDirectory], True)
    else
      CopyFile(FileIn, FileOut, [cffPreserveTime, cffOverwriteFile], True);
  end;
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
  checkArgCount([1, 2, 3]);
  checkArgTypes(['TStringInstance', 'TBooleanInstance', 'TStringInstance']);
  len := Length(FParams);
  APath := TStringInstance(FParams[0]).PValue;
  if (len > 1) then
    ASub := TBooleanInstance(FParams[1]).PboolValue;

  if (len > 2) then
    AFilter := TStringInstance(FParams[2]).PStrValue;

  try
    AList := TStringList.Create;
    try
      FindAllFiles(AList, APath, AFilter, ASub);
      ARet := TListInstance.Create;
      for F in AList do
        ARet.Add(TStringInstance.Create(F));
      Result := ARet;
    except on E: Exception do
           FInter.RaiseException(E.Message, 'Internal')
    end;
  finally
    AList.Free;
  end;
end;

function TCoreFunction.CreateDirOpt: TBooleanInstance;
var
  len :integer;
begin
  checkArgCount([1, 2]);
  checkArgTypes(['TStringInstance', 'TBooleanInstance']);
  len := Length(FParams);
  if len = 2 then
  begin
    if TBooleanInstance(FParams[1]).PValue then
      Result := TBooleanInstance.Create(ForceDirectories(FParams[0].PStrValue))
    else
      Result := TBooleanInstance.Create(CreateDir(FParams[0]).PStrValue);
  end
  else
    Result := TBooleanInstance.Create(CreateDir(FParams[0].PStrValue));
end;
