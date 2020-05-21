program ultragen_fcgi;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  { you can add units after this }
  fpFCGI, FCGIModule;

begin
  Application.Title:='UltraGen FGCI Server';
  { Uncomment the port setting here if you want to run the
    FastCGI application stand-alone (e.g. for NGINX) }
  // Application.Port:=2015; // For example

  Application.PreferModuleName := True;
  Application.Initialize;
  Application.Run;
end.
