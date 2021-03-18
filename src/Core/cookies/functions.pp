procedure TCoreFunction.SetCookie;
var
  AParams: TActivationRecord;
  AParam: TInstanceOf;
begin
    if (Length(FParams) < 2) or (Length(FParams) > 2) then
      FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
    ACookie := Finter.PResponse.Cookies.Add;
    ACookie.Path := '/';
    ACookie.Name := TStringInstance(FParams[0]).PValue;
    ACookie.Value := FParams[1].AsString;
    {if Length(FParams) = 2 then
    begin
      AParams := TDictionaryInstance(FParams[2]).PValue;
      AParam := AParams.GetMember('path');
      if AParam <> nil then
        if not AParam.ClassNameIs('TStringInstance') then
          FInter.RaiseException('Path cookie setting only accept strings as value', 'Type');
      else
        ACookie.Path := TStringInstance(AParam).PValue;
      AParam := AParams.GetMember('expires');
      if AParam <> nil then
        if AParam.ClassNameIs('TNullInstance') then
        begin
        end
        else if AParam.ClassNameIs('TDateTimeInstance') then
          ACookie.Expires := ;
    end}
end;
