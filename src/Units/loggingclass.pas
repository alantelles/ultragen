unit LoggingClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  DEBUG = 'LOG_DEBUG';
  TRACE = 'LOG_TRACE';
  INTER = 'INTER';
  NO_LOG = '';

procedure LogText(ALevel, AFrom, AMsg:string);
procedure LogDebug(AMsg: string; AFrom:string = 'Generic log: ');
procedure LogTrace(AMsg: string; AFrom:string = 'Generic log: ');

var
  LogLevel:string = DEBUG;

implementation

procedure LogText(ALevel, AFrom, AMsg:string);
begin
  if ALevel = LogLevel then
    WriteLn(AFrom, ': ', AMsg);
end;

procedure LogDebug(AMsg: string; AFrom:string = 'Generic log: ');
begin
  if LogLevel = DEBUG then
    WriteLn(AFrom, ': ', AMsg);
end;

procedure LogTrace(AMsg: string; AFrom:string = 'Generic log: ');
begin
  if LogLevel = TRACE then
    WriteLn(AFrom, ': ', AMsg);
end;

end.

