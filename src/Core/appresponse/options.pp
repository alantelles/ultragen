else if AType = 'TAppResponseInstance' then
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


    TUltraFPWebHandlers(FInter.PWebHandlers).PResponse.Location := TStringInstance(FParams[0]).PValue;
    TUltraFPWebHandlers(FInter.PWebHandlers).PResponse.Code := AuxInt;
    if Length(FParams) > 1 then
      TUltraFPWebHandlers(FInter.PWebHandlers).PResponse.Code := TIntegerInstance(FParams[1]).PValue;
  end
  else if FName = 'clientRedirect' then
  begin
    ClientRedirect;
  end
  else if FName = 'static' then
    ServeStatic;
end
