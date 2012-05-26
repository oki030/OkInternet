program OkInternet;

uses
  Forms,
  SysUtils,
  Windows,
  Messages,
  Pipes,
  MainUnit in 'MainUnit.pas' {MainForm},
  Process in 'Process.pas',
  BrowserUnit in 'BrowserUnit.pas' {BrowserForm},
  WidgetUnit in 'WidgetUnit.pas' {Widget},
  Automation in 'Automation.pas';

{$R *.res}
var
  Msg: TMsg;
  PipeClient: TPipeClient;

begin
  Application.Initialize;
  PipeClient:= TPipeClient.Create(nil);
  PipeClient.PipeName:= 'OkServer';

  if PipeClient.Connect(100) then
    begin
      if ParamStr(3) = 'Tab' then// Tab
        begin
          Application.MainFormOnTaskbar := false;
          Application.CreateForm(TBrowserForm, BrowserForm);
          BrowserForm.ParentWindow:= StrToInt(ParamStr(1));
          BrowserForm.Host:= StrToInt(ParamStr(2));
          if ParamStr(4) = '' then
            begin
              BrowserForm.Browser.Navigate(ExtractFilePath(Application.ExeName) + 'NewTab\index.html');
            end
          else
            BrowserForm.Browser.Navigate(ParamStr(4));
          BrowserForm.Init;
          Application.Run;
        end;

      if ParamStr(3) = 'Widget' then// Widget
        begin
          Application.MainFormOnTaskbar:= false;
          Application.CreateForm(TWidget, Widget);

          Widget.WidgetName:= ParamStr(1);
          Widget.WidgetPath:= ExtractFilePath(Application.ExeName) + 'Widgets\' + Widget.WidgetName + '\';
          Widget.Caption:= Widget.WidgetName;
          Widget.Browser.Navigate(Widget.WidgetPath + Widget.WidgetName + '.htm');
          Widget.Init;

          //Application.ShowMainForm := false;
          Application.Run;
        end;
    end
  else
    begin
      Application.MainFormOnTaskbar:= True;
      Application.Title:= 'OkInternet';
      Application.CreateForm(TMainForm, MainForm);
      Application.Run;
    end;
end.
