else if AType = 'TAppResponseInstance' then
begin
  if FName = 'redirect' then
  begin
    FInter.PResponse.Location := TStringInstance(FParams[0]).PValue;
    FInter.PResponse.Code := 303;
    FInter.PResponse.CodeText := 'Redirecting to ' + FInter.PResponse.Location;
    if Length(FParams) > 1 then
      FInter.PResponse.CodeText := TStringInstance(FParams[1]).PValue;
    if Length(FParams) > 2 then
      FInter.PResponse.Code := TintegerInstance(FParams[2]).PValue;
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
