unit UltraWebHandlersClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs,
  BrookHTTPRequest,
  BrookHTTPResponse, StrUtils;

type

  TUltraWebHandlers = class
    FFallbackMimeTypes: TStringList;
    public
      property FallbackMimeTypes: TStringList read FFallbackMimeTypes;
      constructor Create;
  end;

  TUltraFPWebHandlers = class (TUltraWebHandlers)
    FRequest: TRequest;
    FResponse: TResponse;
    public
      property PRequest: TRequest read FRequest;
      property PResponse: TResponse read FResponse;
      constructor Create(ARequest: TRequest; AResponse: TResponse);
  end;

  TUltraBrookHandlers = class (TUltraWebHandlers)
    FRequest: TBrookHTTPRequest;
    FResponse: TBrookHTTPResponse;
    public
      property PRequest: TBrookHTTPRequest read FRequest;
      property PResponse: TBrookHTTPResponse read FResponse;
      constructor Create(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
  end;

implementation

constructor TUltraWebHandlers.Create;
begin
  FFallbackMimeTypes := TStringList.Create;
  FFallbackMimeTypes.Add('image/jpeg            jpeg jpg jpe');
  FFallbackMimeTypes.Add('image/gif               gif');
  FFallbackMimeTypes.Add('image/png                   png');
  FFallbackMimeTypes.Add('text/css                    css');
  FFallbackMimeTypes.Add('application/javascript              js');
  FFallbackMimeTypes.Add('image/svg+xml                   svg svgz');
end;

constructor TUltraBrookHandlers.Create(ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  FRequest := ARequest;
  FResponse := AResponse;
end;

constructor TUltraFPWebHandlers.Create(ARequest: TRequest; AResponse: TResponse);
begin
  FRequest := ARequest;
  FResponse := AResponse;
end;

end.

