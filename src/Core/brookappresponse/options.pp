else if AType = 'TBrookResponseInstance' then
begin
  if FName = 'redirect' then
  begin
    AuxInt := 303;
    Aux := TInstanceOf(FObj.PMembers.Find('status'));
    if Aux <> nil then
      if Aux.ClassNameIs('TIntegerInstance') then
        AuxInt := Aux.PIntValue;
    FInter.PRedirected := True;
    if Length(FParams) > 1 then
      if FParams[1].ClassNameIs('TIntegerInstance') then
        AuxInt := TIntegerInstance(FParams[1]).PValue;
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
