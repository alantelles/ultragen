unit DateTimeInstanceClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InstanceOfClass;


type TDateTimeInstance = class (TInstanceOf)
  private
    FValue: TDateTime;
  public
    property PValue: TDateTime read FValue write FValue;
    constructor Create(ADateTIme: TDateTime);
    function AsString: string; override;
    function GetPartOfDate(APart: string): TInstanceOf;
    class function CreateNowDateTime: TDateTimeInstance;
    class function ParseDateFromString(AFormat, ADate:string): TDateTimeInstance;
    class function ConstructDateTime(AValues: TInstanceList): TDateTimeInstance;
end;

implementation
uses DateUtils, StringInstanceClass, StrUtils;

class function TDateTimeInstance.ConstructDateTime(AValues: TInstanceList): TDateTimeInstance;
var
  len, s, i: integer;
  fmtStr, dateStr, nowStr, nowFmt: string;
  parts: array of string;
  last: string = '';
begin
  SetLength(parts, 7);
  parts[0] := 'y';
  parts[1] := 'm';
  parts[2] := 'd';
  parts[3] := 'h';
  parts[4] := 'n';
  parts[5] := 's';
  parts[6] := 'z';
  fmtStr := '';
  dateStr := '';
  len := Length(AValues);
  if len > 0 then
  begin
    for i := 0 to len - 1 do
    begin
      nowStr := '';
      nowFmt := '';
      nowStr := AValues[i].AsString;
      s := Length(nowStr);
      nowFmt := DupeString(parts[i], s);
      dateStr := dateStr + nowStr + '-';
      fmtStr := fmtStr + nowFmt + '-';
    end;
    Result := TDateTimeInstance.Create(ScanDateTime(fmtStr, dateStr));
  end
  else
    Result := TDateTimeInstance.Create(Now);
end;

constructor TDateTimeInstance.Create(ADateTime: TDateTime);
begin
  inherited Create;

  FValue := ADateTime;
end;

class function TDateTimeInstance.ParseDateFromString(AFormat, ADate: string): TDAteTimeInstance;
begin
  Result := TDateTimeInstance.Create(dateutils.ScanDateTime(AFormat, ADate));
end;

function TDateTimeInstance.AsString: string;
begin
  Result := FormatDateTime('YYYY-MM-DD HH:NN:SS.ZZZ', FValue);
end;

function TDateTimeInstance.GetPartOfDate(APart: string): TInstanceOf;
begin
  if APart = 'year' then
    Result := TIntegerInstance.Create(YearOf(FValue))
  else if APart = 'month' then
    Result := TIntegerInstance.Create(MonthOf(FValue))
  else if APart = 'day' then
    Result := TIntegerInstance.Create(DayOf(FValue))
  else if APart = 'hour' then
    Result := TIntegerInstance.Create(HourOf(FValue))
  else if APart = 'minute' then
    Result := TIntegerInstance.Create(MinuteOf(FValue))
  else if APart = 'second' then
    Result := TIntegerInstance.Create(SecondOf(FValue))
  else if APart = 'weekDay' then
    Result := TIntegerInstance.Create(DayofTheWeek(FValue))
  else if APart = 'weekDayName' then
    Result := TStringInstance.Create(FormatDateTime('dddd', FValue))
  else if APart = 'weekDayAbbr' then
    Result := TStringInstance.Create(FormatDateTime('ddd', FValue))
  else if APart = 'milli' then
    Result := TIntegerInstance.Create(MilliSecondOf(FValue));
end;

class function TDateTimeInstance.CreateNowDateTime: TDateTimeInstance;
begin
  Result := TDateTimeInstance.Create(Now);
end;

end.

