// teste
else if AType = 'TFileSystemInstance' then
begin
  if FName = 'getAllFiles' then
    Ret := GetAllFiles()
  else if FName = 'mkdir' then
    Ret := CreateDirOpt;
end
