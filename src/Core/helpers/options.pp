else if AType = 'THelpersInstance' then
begin
  if FName = 'urlencode' then
    Ret := TStringInstance.Create(httpencode(FParams[0].AsString))
  else if FName = 'urldecode' then
    Ret := TStringInstance.Create(httpdecode(FParams[0].AsString))
  else if FName = 'randomToken' then
  begin
    AuxInt := Length(FParams);
    if AuxInt > 1 then
      Ret := HelpersGetRandomToken
    else
      Ret := HelpersGetRandomToken(True);
  end
end
