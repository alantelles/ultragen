else if AType = 'TCookiesHandlerInstance' then
begin
  if FName = 'set' then
  begin
    ACookie := Finter.PResponse.Cookies.Add;
    ACookie.Path := '/';
    ACookie.Name := TStringInstance(FParams[0]).PValue;
    ACookie.Value := FParams[1].AsString;
  end
  else if FName = 'unset' then
  begin
    ACookie := Finter.PResponse.Cookies.Add;
    ACookie.Path := '/';
    ACookie.Name := TStringInstance(FParams[0]).PValue;
    try
      ACookie.Expires := StrToDateTime('1-1-1970 0:0:0')
    except on E: Exception do
      FInter.RaiseException(E.Message, 'InternalApi');
    end;
  end
  else if FNAme = 'get' then
    Ret := TStringInstance.Create(FInter.PRequest.CookieFields.Values[TStringInstance(FParams[0]).PValue]);
end
