else if AType = 'TCookiesHandlerInstance' then
begin
  if FName = 'set' then
  begin
    ACookie := Finter.PResponse.Cookies.Add;
    ACookie.Name := TStringInstance(FParams[0]).PValue;
    ACookie.Value := FParams[1].AsString;
  end
  else if FNAme = 'get' then
    Ret := TStringInstance.Create(FInter.PRequest.CookieFields.Values[TStringInstance(FParams[0]).PValue]);
end
