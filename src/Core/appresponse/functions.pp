procedure TCoreFunction.ClientRedirect;
var
  RespStr, Refresh, location: string;
  hText: string = '';
  pText: string = '';
  delayTime: string;
begin
  if Length(FParams) < 1 then
    FInter.RaiseException(E_INVALID_ARGS, 'Arguments');
  location := FParams[0].PStrValue;

  if Length(FParams) > 1 then
    delayTime := FParams[1].AsString;
  if Length(FParams) > 2 then
    hText := '<h1>' + FParams[2].PStrValue + '</h1>';
  if Length(FParams) > 3 then
    hText := '<p>' + FParams[3].PStrValue + '</p>';

  Refresh := '<meta http-equiv="refresh" content="' + delayTime + '; URL=''' + location +  '''" />';
  RespStr := '<html><head><meta charset="UTF-8">' + Refresh + '</head><body>' + hText + pText + '</body></html>';
  FInter.PResponse.Content := RespStr;
  Finter.PResponse.SendResponse;
end;

procedure TCoreFunction.ServeStatic;
var
  AStream: TMemoryStream;
  FileName: string;
  mimetry, mime, mimeGet, getExt: string;
begin
  FileName := TStringInstance(FParams[0]).PValue;
  if FileExists(FileName) then
  begin
    getExt := Trim(Copy(FileName, RPos('.', FileName) + 1, Length(FileName)));
    AStream := TMemoryStream.Create;
    AStream.LoadFromFile(FileName);
    Finter.PResponse.ContentStream := AStream;
    // Finter.PResponse.ContentType := 'image/png';
    for mime in FInter.PMimeFile do
    begin
      mimeTry := Trim(mime);
      if mimeTry[1] = '#' then
        continue;
      if Pos(' ' + getExt, mimeTry) > 1 then
      begin
        mimeGet := Copy(mimeTry, 1, Pos(' ', mimetry) - 1);
        break;
      end;
    end;
    FInter.PResponse.ContentType := mimeGet;
    Finter.PResponse.ContentLength := Finter.PResponse.ContentStream.Size;
    // Finter.PResponse.SendContent;
    // Finter.PResponse.ContentStream.Free;
    // AStream.Free;
    Finter.PResponse.Code := 200;
  end
  else
    Finter.PResponse.Code := 404;
end;
