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
begin
  FileName := TStringInstance(FParams[0]).PValue;
  if FileExists(FileName) then
  begin
    AStream := TMemoryStream.Create;
    AStream.LoadFromFile(FileName);
    Finter.PResponse.ContentStream := TMemoryStream.Create;
    Finter.PResponse.ContentStream := AStream;
    Finter.PResponse.ContentType := 'image/png';
    Finter.PResponse.ContentLength := Finter.PResponse.ContentStream.Size;
    // Finter.PResponse.SendContent;
    // Finter.PResponse.ContentStream.Free;
    AStream.Free;
    Finter.PResponse.Code := 200;
  end
  else
  begin
    Finter.PResponse.Code := 404;
  end;
end;
