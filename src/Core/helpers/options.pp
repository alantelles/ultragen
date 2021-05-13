else if AType = 'THelpersInstance' then
begin
  if FName = 'urlencode' then
    Ret := TStringInstance.Create(httpencode(FParams[0].AsString))
  else if FName = 'urldecode' then
    Ret := TStringInstance.Create(httpdecode(FParams[0].AsString));
end
