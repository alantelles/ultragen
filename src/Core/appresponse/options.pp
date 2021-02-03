else if AType = 'TAppResponseInstance' then
begin
  if FName = 'redirect' then
  begin
    FInter.PResponse.Location := TStringInstance(FParams[0]).PValue;
    FInter.PResponse.Code := 303;
    if Length(FParams) > 1 then
      FInter.PResponse.Code := TIntegerInstance(FParams[1]).PValue;
  end
  else if FName = 'setStatusText' then
    Finter.PResponse.CodeText := TStringInstance(FParams[0]).PValue;
end
