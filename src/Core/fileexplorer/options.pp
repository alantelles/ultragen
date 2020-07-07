// teste
else if AType = 'TFileExplorer' then
begin
  if FName = 'getAllFiles' then
    Ret := GetAllFiles(TFileExplorer(AObj));
end
