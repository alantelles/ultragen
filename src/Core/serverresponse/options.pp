else if AType = 'THTTPResponseHandler' then
begin
  if FName = 'redirect' then
  begin
    FInter.PResponse.Location := TStringInstance(FParams[0]).PValue;
    if Length(FParams) > 1 then
      FInter.PResponse.Code := TIntegerInstance(FParams[1]).PValue;
  end;
end
