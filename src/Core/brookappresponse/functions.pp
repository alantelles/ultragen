procedure TCoreFunction.BrookClientRedirect;
var
  RespStr, Refresh, location: string;
  hText: string = '';
  pText: string = '';
  delayTime: string = '0';
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
    pText := '<p>' + FParams[3].PStrValue + '</p>';

  Refresh := '<meta http-equiv="refresh" content="' + delayTime + '; URL=''' + location +  '''" />';
  RespStr := '<html><head><meta charset="UTF-8">' + Refresh + '</head><body>' + hText + pText + '</body></html>';
  TUltraBrookHandlers(FInter.PWebHandlers).PResponse.Send(RespStr, 'text/html; charset=utf-8', 200);
end;

procedure TCoreFunction.BrookServeStatic;
var
  AStream: TMemoryStream;
  FileName: string;
  mimetry, mime, mimeGet, getExt, mimeFileName: string;
  AInst: TInstanceOf;
  AMimeList: TStringList;
  ABytes: TBytes;
  i: integer;
begin
  FileName := TStringInstance(FParams[0]).PValue;
  if FileExists(FileName) then
  begin
    AStream := TMemoryStream.Create;
    AStream.LoadFromFile(FileName);
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


    SetLength(ABytes, AStream.Size);
    AStream.Position := 0;
    if AStream.Size > 0 then
      for i:=0 to AStream.Size-1 do
        ABytes[i] := AStream.ReadByte;
    FInter.PRedirected := True;
    TUltraBrookHandlers(FInter.PWebHandlers).PResponse.SendBytes(ABytes, AStream.size, mimeGet, 200);
    AStream.Free;
  end
  else
  begin
    TUltraBrookHandlers(FInter.PWebHandlers).PResponse.Send('File ' + FileName + ' not found', 'text/html; charset=utf-8', 404);
    FInter.PRedirected := True;
  end;


end;
