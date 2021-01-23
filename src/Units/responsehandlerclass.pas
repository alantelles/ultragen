unit ResponseHandlerClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass, ARClass, httpdefs;

type
  TResponseHandlerInstance = class (TInstanceOf)
    protected
      FValue: TResponse;
    public
      property PValue:TResponse read FValue;
      constructor Create(AResponse: TResponse);
      function AsString: string; override;

end;

implementation

constructor TResponseHandlerInstance.Create(AResponse: TResponse);
begin
  FValue := AResponse;
end;

function TResponseHandlerInstance.AsString: string;
begin
  Result := '<HTTP Request response handler>';
end;

end.

