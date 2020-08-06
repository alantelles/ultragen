// teste
else if AType = 'TFileSystemInstance' then
begin
  if FName = 'getAllFiles' then
    Ret := GetAllFiles()
  else if FName = 'mkdir' then
    Ret := CreateDirOpt
  else if FName = 'isFile' then
    Ret := TBooleanInstance.Create(FileExists(TStringInstance(FParams[0]).PValue))
  else if FName = 'loadText' then
    Ret := TListInstance.LoadText(TStringInstance(FParams[0]).PValue)
  else if FName = 'isDir' then
    Ret := TBooleanInstance.Create(DirectoryExists(TStringInstance(FParams[0]).PValue));
end
