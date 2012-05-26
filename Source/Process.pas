unit Process;

interface

uses
  Windows, Messages, Variants;

type
  TProcess = class
  private
    { Private declarations }
    Active: Boolean;
    startInfo: TStartupInfo;
  public
    { Public declarations }
    procInfo: TProcessInformation;
    constructor Create();
    destructor Destroy(); override;
    function Run(ShowWindow: WORD; exeName: String; param: String): Boolean;
    procedure SetPriority(Priority: Cardinal);
  end;

implementation

constructor TProcess.Create;
begin
  Active:= false;

  FillChar(startInfo, SizeOf(TStartupInfo), #0);
  FillChar(procInfo, SizeOf(TProcessInformation), #0);
  startInfo.cb:= SizeOf(TStartupInfo);
end;

destructor TProcess.Destroy;
begin
  CloseHandle(procInfo.hThread);
  CloseHandle(procInfo.hProcess);
end;

function TProcess.Run(ShowWindow: WORD; exeName: String; param: String): Boolean;
var
  r: Boolean;
begin
  if Active then
    begin
      Run:= false;
      exit;
    end;

  startInfo.dwFlags:= STARTF_USESHOWWINDOW;
  startInfo.wShowWindow:= ShowWindow;

  r:= CreateProcess(nil,
          PChar(exeName + ' ' + param),
          nil,
          nil,
          true,
          CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS,
          nil,
          nil,
          startInfo,
          procInfo);

  if r then SetPriorityClass(procInfo.hProcess, Idle_Priority_Class);

  Active:= r;
  Run:= r;
end;

procedure TProcess.SetPriority(Priority: Cardinal);
begin
  SetPriorityClass(procInfo.hProcess, Priority);
end;

end.
