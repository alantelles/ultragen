unit DateTimeInstanceClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, InstanceOfClass;


type TDateTimeInstance = class (TInstanceOf)
  private
    FValue: TDateTime;
  public
    property PValue: TDateTime read FValue write FValue;
    constructor Create(ADateTIme: TDateTime);
    function AsString: string; override;
    class function CreateNowDateTime: TDateTimeInstance;
end;

implementation

constructor TDateTimeInstance.Create(ADateTime: TDateTime);
begin
  FValue := ADateTime;
end;

function TDateTimeInstance.AsString: string;
begin
  Result := FormatDateTime('YYYY-MM-DD HH:NN:SS.ZZZ', FValue);
end;

class function TDateTimeInstance.CreateNowDateTime: TDateTimeInstance;
begin
  Result := TDateTimeInstance.Create(Now);
end;

end.

