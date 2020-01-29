unit QueueClass;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}
    cthreads,
  {$ENDIF}
  Classes, SysUtils,  TypesGlobals, GenFileSetClass, ThreadedTemplateClass, TemplateClass;

const
  TASK_DONE = 2;
  TASK_DOING = 1;
  TASK_QUEUED = 0;


type
  TTask = record
    Func:TUserFunction;
    Args:TStringList;
    Done:integer;
    GenFileSet:TGenFileSet;
    // Done status:
    // 0 - queued
    // 1 - doing
    // 2 - done
  end;

  TTaskQueue = array of TTask;

  TSlot = record
    AThread: TThreadedTemplate;
    IsFree:boolean;
    TaskId:integer;
  end;

  TSlots = array of TSlot;

type
  TQueue = class
  private
    FMaxThreads, FExecuting:integer;
    FName:string;
    FQueue:TTaskQueue;
    FSlots:TSlots;
    FActive:boolean;
  public
    property Name:string read FName write FName;
    property MaxThreads:integer read FMaxThreads write FMaxThreads;
    property Executing:integer read FExecuting write FExecuting;
    property Queue:TTaskQueue read FQueue write FQueue;
    property Active:boolean read FActive write FActive;
    constructor Create(AnAlias:string; AMaxThreads:integer);
    function AddTask(AFunc:TUserFunction; AnArgs:TStringList;AGenFileSet:TGenFileSet):integer;
    procedure Verify;
    function UndoneTasks:integer;
  end;
  TQueueList = array of TQueue;


implementation


constructor TQueue.Create(AnAlias:string; AMaxThreads:integer);
var
  i:integer;
begin
  FMaxThreads := AMaxThreads;
  FName := AnAlias;
  SetLength(FQueue,0);
  FActive := True;
  FExecuting := 0;
  SetLength(FSlots,AMaxThreads);
  for i:=0 to AMaxThreads-1 do
    FSlots[i].IsFree := True;
end;

function TQueue.UndoneTasks:integer;
var
  Ret:integer=-1;
  len,i:integer;
begin
  len := Length(FQueue);
  if len > 0 then
  begin
    Ret := 0;
    for i:=0 to len-1 do
    begin
      if FQueue[i].Done <> TASK_DONE then
        Ret := Ret + 1;
    end;
  end;
  Result := Ret;
end;

function TQueue.AddTask(AFunc:TUserFunction; AnArgs:TStringList; AGenFileSet:TGenFileSet):integer;
var
  len: integer;
begin
  //writeln('The task ',AFunc.FunctionName,' was added to queue ',FName);
  len := Length(FQueue);
  SetLength(FQueue,len+1);
  FQueue[len].Done := TASK_QUEUED;
  FQueue[len].Func := AFunc;
  FQueue[len].Args := TStringList.Create;
  FQueue[len].Args.AddStrings(AnArgs);
  FQueue[len].GenFileSet := AGenFileSet;
  //writeln('task ',AFunc.FunctionName,' added to queue ',FName);
  Result := len;
end;

procedure TQueue.Verify;
var
  i, j, len, ut:integer;
  ATask:TThreadedTemplate;
begin
  len := Length(FQueue);
  //writeln('verify');
  //writeln('verifying inside queue ',FName);
  if (len > 0) then
  begin
    for i:=0 to len-1 do
    begin
      if FQueue[i].Done = TASK_QUEUED then
      begin
        if (FExecuting < FMaxThreads) then
        begin
          for j:=0 to FMaxThreads-1 do
          begin
            if FSlots[j].IsFree then
            begin
              FExecuting := FExecuting + 1;
              FSlots[j].AThread := TThreadedTemplate.Create(FQueue[i].Func, FQueue[i].GenFileSet, i, FQueue[i].Args , FName);
              FSlots[j].IsFree := False;
              FSlots[j].AThread.Start;
              FSlots[j].TaskId := i;
              FQueue[i].Done := TASK_DOING;
              break;
            end;

          end;
        end;
      end;
    end;
    while (FExecuting > 0) and (FExecuting <= MaxThreads) do
    begin
      for j:=0 to FMaxThreads-1 do
      begin
        if not FSlots[j].IsFree then
        begin
          if FSlots[j].AThread.Finished then
          begin
            FSlots[j].IsFree := True;
            FQueue[FSlots[j].TaskId].Done := TASK_DONE;
            FExecuting := FExecuting -1;
            break;
          end;
        end;
      end;
      //ut := UndoneTasks;
    end;
    Verify;
  end;
end;

end.

