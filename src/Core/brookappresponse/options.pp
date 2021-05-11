else if AType = 'TBrookResponseInstance' then
begin
  if FName = 'redirect' then
  begin
    FInter.PRedirected := True;
    AuxInt := 303;
    if Length(FParams) > 1 then
      if FParams[1].ClassNameIs('TIntegerInstance') then
        AuxInt := TIntegerInstance(FParams[0]).PValue;
    TUltraBrookHandlers(Finter.PWebHandlers).PResponse.SendAndRedirect('', TStringInstance(FParams[0]).PValue, 'text/html', AuxInt);
  end
  else if FName = 'setHeader' then
    FInter.PResponse.SetCustomHeader(TStringInstance(FParams[0]).PValue, FParams[1].AsString)
  else if FName = 'clientRedirect' then
  begin
    ClientRedirect;
  end
  else if FName = 'setContentType' then
    FInter.PResponse.ContentType := TStringInstance(FParams[0]).PValue
  else if FName = 'static' then
    ServeStatic
  else if FName = 'setStatusText' then
    Finter.PResponse.CodeText := TStringInstance(FParams[0]).PValue;
end
