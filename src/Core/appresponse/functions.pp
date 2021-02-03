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
