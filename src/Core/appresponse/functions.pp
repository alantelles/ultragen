procedure TCoreFunction.ClientRedirect;
var
  RespStr, Refresh, location: string;
  hText: string = '';
  pText: string = '';
  delayTime: string;
begin

  FInter.PRedirected := True;

  if Length(FParams) < 1 then
    FInter.RaiseException(E_INVALID_ARGS, 'Arguments');


  // sig(location, delay, h1, p)
  location := FParams[0].PStrValue;

  if Length(FParams) > 1 then
    delayTime := FParams[1].AsString;
  if Length(FParams) > 2 then
    hText := '<h1>' + FParams[2].PStrValue + '</h1>';
  if Length(FParams) > 3 then
    hText := '<p>' + FParams[3].PStrValue + '</p>';

  Refresh := '<meta http-equiv="refresh" content="' + delayTime + '; URL=''' + location +  '''" />';
  RespStr := '<html><head><meta charset="UTF-8">' + Refresh + '</head><body>' + hText + pText + '</body></html>';
  TUltraFPWebHandlers(FInter.PWebHandlers).PResponse.Content := RespStr;
  TUltraFPWebHandlers(FInter.PWebHandlers).PResponse.SendResponse;
end;

procedure TCoreFunction.ServeStatic;
var
  AStream: TMemoryStream;
  FileName: string;
  mimetry, mime, mimeGet, getExt, mimeFileName: string;
  AInst: TInstanceOf;
  AMimeList: TStringList;
begin
  FileName := TStringInstance(FParams[0]).PValue;
  if FileExists(FileName) then
  begin
    AStream := TMemoryStream.Create;
    AStream.LoadFromFile(FileName);
    TUltraFPWebHandlers(FInter.PWebHandlers).PResponse.ContentStream := AStream;
    if Length(FParams) = 1 then
    begin
      getExt := Trim(Copy(FileName, RPos('.', FileName) + 1, Length(FileName)));
      mimeFileName := FInter.PUltraHome + directorySeparator + 'assets' + directorySeparator + 'mime-types.txt';
      AInst := TInstanceOf(FObj.PMembers.Find('mimeTypesFile'));
      if AInst <> nil then
        if AInst.ClassNameIs('TStringInstance') then
          mimeFileName := AInst.PStrValue;
      if FileExists(mimeFileName) then
      begin
        AMimeList := TStringList.Create;
        AMimeList.LoadFromFile(mimeFileName);
      end
      else
        AMimeList := FInter.PWebHandlers.FallbackMimeTypes;
      for mime in AMimeList do
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
      AMimeList.Free;
    end
    else
      mimeGet := FParams[1].PStrValue;

    if mimeGet = '' then
      mimeGet := 'application/octet-stream';
    TUltraFPWebHandlers(FInter.PWebHandlers).PResponse.ContentType := mimeGet;
    TUltraFPWebHandlers(FInter.PWebHandlers).PResponse.ContentLength := TUltraFPWebHandlers(FInter.PWebHandlers).PResponse.ContentStream.Size;
    TUltraFPWebHandlers(FInter.PWebHandlers).PResponse.SendContent;
    // TUltraFPWebHandlers(FInter.PWebHandlers).PResponse.ContentStream.Free;
    AStream.Free;
    TUltraFPWebHandlers(FInter.PWebHandlers).PResponse.Code := 200;
    FInter.PRedirected := True;
  end
  else
  begin
    TUltraFPWebHandlers(FInter.PWebHandlers).PResponse.Code := 404;
    TUltraFPWebHandlers(FInter.PWebHandlers).PResponse.CodeText := 'Not found';
    TUltraFPWebHandlers(FInter.PWebHandlers).PResponse.ContentType := 'text/html; charset=utf-8';
    FInter.PRedirected := True;
    TUltraFPWebHandlers(FInter.PWebHandlers).PResponse.Content := 'File ' + FileName + ' was not found';
    TUltraFPWebHandlers(FInter.PWebHandlers).PResponse.SendResponse;
  end;
end;
