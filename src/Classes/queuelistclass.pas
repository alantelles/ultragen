unit QueueListClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypesGlobals, QueueClass, GenFileSetClass, TemplateClass;

type
  TQueueSet = class
  private
    FCount:integer;
    FQueueList:TQueueList;
    FInterval:integer;
  public
    property Queues:TQueueList read FQueueList write FQueueList;
    property Count:integer read FCount write FCount;
    property Interval:integer read FInterval write FInterval;
    constructor Create;
    function AddQueue(AnAlias:string; Slots:integer; Activate:boolean):integer;
    procedure StopQueue(AnAlias:string);
    procedure ActivateQueue(AnAlias:string);
    function FindQueue(AnAlias:string):integer;
    function TasksRunning(AnAlias:string):integer;
    procedure AddTask(AnAlias:string; AFunc:TUserFunction; AParams:TStringList; AGenFileSet:TGenFileSet);
    procedure Verify;
  end;

type
  TThreadVerify = class(TThread)
  public
    constructor Create;
  protected
    procedure Execute; override;
  end;

  TQueueVerify = class(TThread)
  private
    FIndex:integer;
  public
    constructor Create(TaskIndex:integer);
  protected
    procedure Execute; override;
  end;

  TQueueVerifySet = array of TQueueVerify;

var

  GlobalQueue:TQueueSet;
  AVerifyThread:TThreadVerify;

implementation

constructor TThreadVerify.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

procedure TThreadVerify.Execute;
begin
  GlobalQueue.Verify;
end;

constructor TQueueVerify.Create(TaskIndex:integer);
begin
  inherited Create(True);
  FIndex := TaskIndex;
  FreeOnTerminate := True;
end;

procedure TQueueVerify.Execute;
begin
  GlobalQueue.Queues[FIndex].Verify;
end;

constructor TQueueSet.Create;
begin
  FCount := 0;
  SetLength(FQueueList,FCount);
end;

procedure TQueueSet.AddTask(AnAlias:string; AFunc:TUserFunction; AParams:TStringList; AGenFileSet:TGenFileSet);
var
  i:integer;
begin
  i := FindQueue(AnAlias);
  if i > -1 then
    FQueueList[i].AddTask(AFunc,AParams,AGenFileSet);
end;

procedure TQueueSet.Verify;
var
  i:integer;
  QVThreads:TQueueVerifySet;
begin
  while True do
  begin
    //writeln('verifying all');
    if FCount > 0 then
    begin

      SetLength(QVThreads,FCount);
      for i:=0 to FCount-1 do
      begin
        if (FQueueList[i].Active) and (FQueueList[i].Executing < FQueueList[i].MaxThreads) then
        begin
          //writeln('verifying queue ',FQueueList[i].Name,' that has ',FQueueList[i].MaxThreads,' slots.');
          QVThreads[i] := TQueueVerify.Create(i);
          QVThreads[i].Start;
        end;
        //FQueueList[i].Verify;
      end;
    end;
    Sleep(FInterval);
  end;
end;


function TQueueSet.AddQueue(AnAlias:string; Slots:integer; Activate:boolean):integer;
var
  len:integer;
begin
  len := FCount;
  SetLength(FQueueList,len+1);
  FCount := len+1;
  FQueueList[len] := TQueue.Create(AnAlias, Slots);
  FQueueList[len].Active := Activate;
  //writeln('The queue ',FQueueList[len].Name,' was created.');
  //writeln('It has ',FQueueList[len].MaxThreads,' slots.');
  Result := len;
end;

procedure TQueueSet.StopQueue(AnAlias:string);
var
  i:integer;
begin
  i := FindQueue(AnAlias);
   if i > -1 then
     FQueueList[i].Active := False;
   //writeln('stopping queue');
end;

procedure TQueueSet.ActivateQueue(AnAlias:string);
var
  i:integer;
begin
  i := FindQueue(AnAlias);
   if i > -1 then
     FQueueList[i].Active := True;
   //writeln('activating queue');
end;

function TQueueSet.FindQueue(AnAlias:string):integer;
var
  i, Ret:integer;
begin
  Ret := -1;
  if FCount > 0 then
  begin
    for i:=0 to FCount-1 do
    begin
      if FQueueList[i].Name = AnAlias then
      begin
        Ret := i;
        break;
      end;
    end;
  end;
  Result := Ret;
end;

function TQueueSet.TasksRunning(AnAlias:string):integer;
var
  i:integer;
  Ret:integer=-1;
begin
   i := FindQueue(AnAlias);
   if i > -1 then
     Ret := FQueueList[i].Executing;
  Result := Ret;
end;

end.

