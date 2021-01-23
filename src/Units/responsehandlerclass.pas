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

end;

implementation

constructor TResponseHandlerInstance.Create(AResponse: TResponse);
begin
  FValue := AResponse;
end;

end.

