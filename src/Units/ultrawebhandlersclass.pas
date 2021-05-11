unit UltraWebHandlersClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs,
  BrookHTTPRequest,
  BrookHTTPResponse;

type

  TUltraWebHandlers = class

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

