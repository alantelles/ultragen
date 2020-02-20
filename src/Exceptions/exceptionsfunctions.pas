unit ExceptionsFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure RaiseException(EType:string; UltraMsg:string; LineNum:integer; TempName:string; Line:string);

implementation

uses StrUtils;

procedure RaiseException(EType:string; UltraMsg:String; LineNum:integer; TempName:string; Line:string);
begin
  WriteLn;
  WriteLn('==================================');
  WriteLn;
  WriteLn('An error was encountered at line '+IntToStr(LineNum)+' while running "'+TempName+'"');
  WriteLn('    '+EType+': '+UltraMsg+'. ');
  WriteLn;
  WriteLn('>>>    '+trim(Line));
  WriteLn;
  WriteLn('==================================');
  WriteLn;
  Halt;
end;




end.

