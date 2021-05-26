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
  else if FName = 'clientRedirect' then
  begin
    BrookClientRedirect;
  end
  else if FName = 'static' then
    BrookServeStatic;
end
