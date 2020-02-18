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
  WriteLn('The program encountered an unhandled exception at line '+IntToStr(LineNum)+' in '+TempName);
  WriteLn('    '+EType+': '+UltraMsg+'. ');
  WriteLn;
  WriteLn('>>>    '+trim(Line));
  WriteLn;
  WriteLn('==================================');
  WriteLn;
  Halt;
end;




end.

