function TCoreFunction.GetAllFiles(var AObj: TFileExplorer): TListInstance;
begin
  AObj.PPath := TStringInstance(FParams[0]).PValue;
  Result := AObj.GetAllFiles;
end;
