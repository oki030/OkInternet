unit WidgetUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Pipes, OleCtrls, SHDocVw_EWB, EwbCore, EmbeddedWB, Automation, IniFiles,
  uPSComponent, sSkinManager;

type
  TWidget = class(TForm)
    PipeClient: TPipeClient;
    Browser: TEmbeddedWB;
    PSScript: TPSScript;
    sSkinManager: TsSkinManager;
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PipeClientPipeDisconnect(Sender: TObject; Pipe: Cardinal);
    procedure PipeClientPipeMessage(Sender: TObject; Pipe: Cardinal;
      Stream: TStream);
    procedure BrowserGetExternal(Sender: TCustomEmbeddedWB;
      var ppDispatch: IDispatch);
    procedure PSScriptCompile(Sender: TPSScript);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    WidgetIni: TMemIniFile;
    WidgetPath, WidgetName: String;

    procedure Init;
    procedure SendToHostPipe(Cmd: String; Text: String);
    procedure SetFlag(Name: String; State: Boolean);
  end;

var
  Widget: TWidget;

implementation

{$R *.dfm}

procedure TWidget.BrowserGetExternal(Sender: TCustomEmbeddedWB;
  var ppDispatch: IDispatch);
var
  W: TApplicationWrapper;
begin
  W := TApplicationWrapper.Connect(Forms.Application);
  ppDispatch := TAutoObjectDispatch.Create(W) as IDispatch;
end;

procedure TWidget.FormActivate(Sender: TObject);
begin
  ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TWidget.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Widget.Visible:= false;
  CanClose:= false;
end;

procedure TWidget.FormDestroy(Sender: TObject);
begin
  WidgetIni.UpdateFile;
  FreeAndNil(WidgetIni);
end;

procedure TWidget.FormShow(Sender: TObject);
begin
  ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TWidget.PipeClientPipeDisconnect(Sender: TObject; Pipe: Cardinal);
begin
  Application.Terminate;
end;

procedure TWidget.Init;
begin
  if PipeClient.Connect(100) then
    begin
      SendToHostPipe('LogInWidget', IntToStr(Widget.Handle) + ';' + WidgetName);
    end;

  WidgetIni:= TMemIniFile.Create(WidgetPath + WidgetName + '.ini');
end;

function StreamToString(Stream: TStream): String;
var
  ms : TMemoryStream;
begin
  Result := '';
  ms := TMemoryStream.Create;
  try
    ms.LoadFromStream(Stream);
    SetString(Result, PChar(ms.memory), ms.Size div 2);
  finally
    ms.free;
  end;
end;

procedure TWidget.SendToHostPipe(Cmd: String; Text: String);
var
  s: String;
begin
  s:= Cmd + ';' + Text;
  PipeClient.Write(s[1], Length(s) * 2);
end;

procedure TWidget.PipeClientPipeMessage(Sender: TObject; Pipe: Cardinal;
  Stream: TStream);
var
  s, Cmd, Text: String;
  p: Integer;
begin
  s:= StreamToString(Stream);
  p:= System.Pos(';', s);

  Cmd:= System.Copy(s, 0, p - 1);
  Text:= System.Copy(s, p + 1, Length(s) - p);

  if Cmd = 'Show' then
    begin
      Widget.Visible:= true;
    end;

  if Cmd = 'Hide' then
    begin
      Widget.Visible:= false;
    end;
end;

procedure TWidget.PSScriptCompile(Sender: TPSScript);
begin
  Sender.AddMethod(Self, @TWidget.SetFlag, 'procedure SetFlag(Name: String; State: Boolean);');
end;

procedure TWidget.SetFlag(Name: String; State: Boolean);
begin

end;

end.
