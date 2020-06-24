function TCoreFunction.JoinString:TStringInstance;
var
  len, i: integer;
  joiner, part: string;
  AList: TListInstance;
  AStr: TStringInstance;
begin
  AList := TListInstance(FParams[0]);
  len := AList.Count;
  // joiner := TStringInstance(FParams[0]).PValue;
  joiner := TStringInstance(FParams[1]).PValue;
  part := '';
  if len > 0 then
  begin
    for i:= 0 to len-1 do
    begin
      if AList.GetItem(i).ClassNameIs('TStringInstance') then
      begin
        AStr := TStringInstance(AList.GetItem(i));
        part := part + AStr.PValue;
        if i < (len-1) then
          part := part + joiner;
      end
      else
        raise ERunTimeError.Create('Only string instances can be joined');
    end;
  end;
  Result := TStringInstance.Create(part);
end;

function TCoreFunction.CapitalString:TStringInstance;
var

  s,last, part: string;
  i: integer;
  AStr: string;
begin
  AStr := TStringInstance(FParams[0]).PValue;
  if Length(AStr) > 0 then
  begin
    if Length(FParams) > 1 then
    begin
      if TBooleanInstance(FParams[1]).PValue then
      begin
        part := '';
        last := '';
        if Length(AStr) > 0 then
        begin
          for i:=1 to Length(AStr) do
          begin
            s := AStr[i];
            if last = '' then
            begin
              part := part + UpperCase(s);
              last := 'up'
            end
            else if s = ' ' then
            begin
              part := part + s;
              last := '';
            end
            else if last = 'up' then
            begin
              part := part + LowerCase(s);
              last := 'up';
            end;
          end;
        end;
      end
      else
        part := AnsiUpperCase(AStr[1]) + AnsiLowerCase(Copy(AStr, 2, Length(AStr)));

    end
    else
      part := AnsiUpperCase(AStr[1]) + AnsiLowerCase(Copy(AStr, 2, Length(AStr)));
  end;
  Result := TStringInstance.Create(part);
end;

function TCoreFunction.SplitString:TListInstance;
var
  ASep, s: string;
  AList: TStringList;
  AInsList: TInstanceList;
  len: integer;
begin
  ASep := TStringInstance(FParams[1]).PValue;
  AList := TStringList.Create;
  AList.SkipLastLineBreak := True;
  AList.LineBreak := ASep;
  AList.Text := TStringInstance(FParams[0]).PValue;
  SetLength(AInsList, 0);
  len := 0;
  for s in AList do
  begin
    len := len + 1;
    SetLength(AInsList, len);
    AInsList[len - 1] := TStringInstance.Create(s);
  end;
  Result := TListInstance.Create(AInsList);
end;

