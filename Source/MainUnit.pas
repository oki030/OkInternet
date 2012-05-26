unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Process, StdCtrls, ExtCtrls, Pipes, rkSmartTabs, Menus,
  ToolWin, ComCtrls, sSkinManager, sSkinProvider, sStatusBar, HistoryMenu,
  FavMenu, ImgList, sGauge, sLabel, GR32, GR32_PNG, GR32_Image, IniFiles, RAR;

type
  TMsgRecord = packed record
    Cmd: String[255];
    Msg : String[255];
    X: Integer;
    Y: Integer;
  end;

  TTabInfo = class
    public
      Handle: HWND;
      Pipe: Cardinal;
  end;

  TWidgetBtn = class(TPaintBox32)
    public
      Bmp1: TBitmap32;
      Bmp2: TBitmap32;
      Pipe: Cardinal;
  end;

  TWidgetInfo = class
    public
      Handle: HWND;
      Pipe: Cardinal;
      Btn: TWidgetBtn;
      Name: String;
  end;

type
  TMainForm = class(TForm)
    TabPanel: TPanel;
    PipeServer: TPipeServer;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    NewTab1: TMenuItem;
    sSkinManager: TsSkinManager;
    sSkinProvider: TsSkinProvider;
    Edit1: TMenuItem;
    Widgets1: TMenuItem;
    Help2: TMenuItem;
    BookmarksMenu: TFavoritesMenu;
    HistoryMenu: THistoryMenu;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    N1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N2: TMenuItem;
    Delete1: TMenuItem;
    SelectAll1: TMenuItem;
    N3: TMenuItem;
    FindonPage1: TMenuItem;
    sStatusBar1: TsStatusBar;
    N4: TMenuItem;
    Open1: TMenuItem;
    SaveAs1: TMenuItem;
    N5: TMenuItem;
    Print1: TMenuItem;
    PrintPreview1: TMenuItem;
    PrintSetup1: TMenuItem;
    N6: TMenuItem;
    Close1: TMenuItem;
    About1: TMenuItem;
    Progress: TsGauge;
    StatusLabel: TsLabelFX;
    WidgetPanel: TPanel;
    SmartTabs: TrkSmartTabs;
    InstallWidget1: TMenuItem;
    N7: TMenuItem;
    RAR: TRAR;
    InstallDialog: TOpenDialog;
    N8: TMenuItem;
    PageProperties1: TMenuItem;
    InternetOptions1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PipeServerPipeMessage(Sender: TObject; Pipe: Cardinal;
      Stream: TStream);
    procedure FormResize(Sender: TObject);
    procedure SmartTabsTabChange(Sender: TObject);
    procedure SmartTabsAddClick(Sender: TObject);
    procedure SmartTabsCloseTab(Sender: TObject; Index: Integer;
      var Close: Boolean);
    procedure PipeServerPipeDisconnect(Sender: TObject; Pipe: Cardinal);
    procedure FormDestroy(Sender: TObject);
    procedure NewTab1Click(Sender: TObject);
    procedure HistoryMenuURLSelected(Sender: TObject; Url: string);
    procedure BookmarksMenuURLSelected(Sender: TObject; Url: string);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure FindonPage1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure PrintPreview1Click(Sender: TObject);
    procedure PrintSetup1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure ProgressChange(Sender: TObject);
    procedure InstallWidget1Click(Sender: TObject);
    procedure PageProperties1Click(Sender: TObject);
    procedure InternetOptions1Click(Sender: TObject);
  private
    { Private declarations }
    WidgetList: TStrings;
    WidgetIni: TMemIniFile;
    procedure WMCopyData(var Msg : TWMCopyData); message WM_COPYDATA;
  public
    { Public declarations }
    procedure NewTab(Address: String);
    procedure NewWidget(WidgetName: String);
    procedure SendToPipe(Pipe: Integer; Cmd: String; Text: String);
    procedure SendToTabWM(ToProcess: HWND; Cmd: String; Text: String; X: Integer; Y: Integer);
    procedure SendCmdToActiveTabWM(Cmd: String; Text: String; X: Integer; Y: Integer);
    procedure SendCmdToActiveTabPipe(Cmd: String; Text: String);
    procedure SendMsg(ToProcess: HWND; MsgRecord : TMsgRecord);

    procedure WidgetPaintBuffer(Sender: TObject);
    procedure WidgetMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure WidgetMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure WidgetMouseEnter(Sender: TObject);
    procedure WidgetMouseLeave(Sender: TObject);
    procedure WidgetMenuClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
 ShellApi, SHDocVw_EWB, ActiveX, Registry, EwbTools;

procedure AddFavorite(Title: OleVariant; Url: OleVariant);
const
  CLSID_ShellUIHelper: TGUID = '{64AB4BB7-111E-11D1-8F79-00C04FC2FBE1}';
var
  ShellUIHelper: ISHellUIHelper;
  Success: integer;
begin
    //if Pos('://', Url) = -1 then exit;

    if (Url <> EmptyStr) and (url <> 'about:blank') then
    begin
      Success := CoCreateInstance(CLSID_SHELLUIHELPER, nil, CLSCTX_INPROC_SERVER,
        IID_IShellUIHelper, ShellUIHelper);
      ShellUIHelper.AddFavorite(Url, Title);
    end;

    MainForm.BookmarksMenu.ReBuildMenu;
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

procedure TMainForm.About1Click(Sender: TObject);
begin
  ShowMessage('Diplomski rad - Oliver Marković, Elfak 2011');
end;

procedure TMainForm.BookmarksMenuURLSelected(Sender: TObject; Url: string);
begin
  SendCmdToActiveTabWM('Go', Url, 0, 0);
end;

procedure TMainForm.Close1Click(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.Copy1Click(Sender: TObject);
begin
  SendCmdToActiveTabWM('Copy', '', 0, 0);
end;

procedure TMainForm.Cut1Click(Sender: TObject);
begin
  SendCmdToActiveTabWM('Cut', '', 0, 0);
end;

procedure TMainForm.Delete1Click(Sender: TObject);
begin
  SendCmdToActiveTabWM('Delete', '', 0, 0);
end;

procedure TMainForm.FindonPage1Click(Sender: TObject);
begin
  SendCmdToActiveTabWM('Find', '', 0, 0);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  PipeServer.Active:= false;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i,j: Integer;
  WidgetName: String;
begin
  BookmarksMenu.CreateMenu;
  HistoryMenu.CreateMenu;

  PipeServer.Active:= true;
  WidgetList:= TStringList.Create;

  WidgetIni:= TMemIniFile.Create(ExtractFilePath(Application.ExeName) + 'Widgets\widgets.ini');
  j:= WidgetIni.ReadInteger('Settings', 'NumberOfWidgets', 0);

  for i:= 1 to j do
    begin
      WidgetName:= WidgetIni.ReadString('Widgets', 'W' + IntToStr(i), '');
      NewWidget(WidgetName);
    end;

  NewTab('');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
   WidgetIni.UpdateFile;

  FreeAndNil(WidgetList);
  FreeAndNil(WidgetIni);
end;

procedure TMainForm.FormResize(Sender: TObject);
var
  i: Integer;
begin
  Progress.Left:= MainForm.Width - Progress.Width - 35;

  for i := 0 to SmartTabs.Tabs.Count - 1 do
    if SmartTabs.Tabs.Objects[i] <> nil then
      begin
        SendToTabWM((SmartTabs.Tabs.Objects[i] as TTabInfo).Handle, 'ReSize', '', TabPanel.Width, TabPanel.Height);
      end;
end;

procedure TMainForm.HistoryMenuURLSelected(Sender: TObject; Url: string);
begin
  SendCmdToActiveTabWM('Go', Url, 0, 0);
end;

procedure TMainForm.InstallWidget1Click(Sender: TObject);
var
   WidName: String;
begin
  if InstallDialog.Execute then
    begin
      RAR.OpenFile(InstallDialog.FileName);
      WidName:= Copy(ExtractFileName(InstallDialog.FileName), 0, Length(ExtractFileName(InstallDialog.FileName)) - 4);

      if RAR.Extract(ExtractFilePath(Application.ExeName) + 'Widgets\', true, nil) then
        begin
          WidgetIni.WriteInteger('Settings', 'NumberOfWidgets', WidgetIni.ReadInteger('Settings', 'NumberOfWidgets', 0) + 1);
          WidgetIni.WriteString('Widgets', 'W' + IntToStr(WidgetIni.ReadInteger('Settings', 'NumberOfWidgets', 0)), WidName);
          WidgetIni.UpdateFile;

          NewWidget(WidName);

          ShowMessage('Widget was successfully installed.');
        end;
    end;
end;

procedure TMainForm.InternetOptions1Click(Sender: TObject);
begin
  SendCmdToActiveTabPipe('InternetOptions', '');
end;

procedure TMainForm.NewTab(Address: String);
var
  p: TProcess;
begin
  p:= TProcess.Create;
  p.Run(SW_HIDE, Application.ExeName, IntToStr(TabPanel.Handle) + ' ' + IntToStr(MainForm.Handle) + ' Tab ' + Address);
  FreeAndNil(p);
end;

procedure TMainForm.NewWidget(WidgetName: String);
var
  p: TProcess;
begin
  p:= TProcess.Create;
  p.Run(SW_HIDE, Application.ExeName, WidgetName + ' ' + IntToStr(MainForm.Handle) + ' Widget');
  FreeAndNil(p);
end;

procedure TMainForm.NewTab1Click(Sender: TObject);
begin
  NewTab('');
end;

procedure TMainForm.Open1Click(Sender: TObject);
begin
  SendCmdToActiveTabPipe('Open', '');
end;

procedure TMainForm.SendToPipe(Pipe: Integer; Cmd: String; Text: String);
var
  s: String;
begin
  s:= Cmd + ';' + Text;
  PipeServer.Write(Pipe, s[1], Length(s) * 2);
end;

procedure TMainForm.SendToTabWM(ToProcess: HWND; Cmd: String; Text: String; X: Integer; Y: Integer);
var
  MsgRecord: TMsgRecord;
begin
    MsgRecord.Cmd:= Cmd;
    MsgRecord.Msg:= Text;
    MsgRecord.X:= X;
    MsgRecord.Y:= Y;

    SendMsg(ToProcess, MsgRecord);
end;

procedure TMainForm.SmartTabsAddClick(Sender: TObject);
begin
  NewTab('');
end;

procedure TMainForm.SmartTabsCloseTab(Sender: TObject; Index: Integer;
  var Close: Boolean);
begin
  if SmartTabs.Tabs.Objects[Index] <> nil then
    begin
      SendToTabWM((SmartTabs.Tabs.Objects[Index] as TTabInfo).Handle, 'Quit', '', 0 , 0);
      Close:= True;
    end;
end;

procedure TMainForm.SmartTabsTabChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to SmartTabs.Tabs.Count - 1 do
    if SmartTabs.Tabs.Objects[i] <> nil then
      begin
        SendToTabWM((SmartTabs.Tabs.Objects[i] as TTabInfo).Handle, 'Hide', '', 0, 0);
      end;

  for i := 0 to WidgetList.Count - 1 do
    if WidgetList.Objects[i] <> nil then
      begin
        (WidgetList.Objects[i] as TWidgetInfo).Btn.Repaint;
      end;

  if SmartTabs.Tabs.Count = 0 then exit;

  if SmartTabs.Tabs.Objects[SmartTabs.ActiveTab] <> nil then
    SendToTabWM((SmartTabs.Tabs.Objects[SmartTabs.ActiveTab] as TTabInfo).Handle, 'Show', '', 0, 0);
end;

procedure TMainForm.Undo1Click(Sender: TObject);
begin
  SendCmdToActiveTabWM('Undo', '', 0, 0);
end;

procedure TMainForm.SaveAs1Click(Sender: TObject);
begin
  SendCmdToActiveTabPipe('Save', '');
end;

procedure TMainForm.SelectAll1Click(Sender: TObject);
begin
  SendCmdToActiveTabWM('SelectAll', '', 0, 0);
end;

procedure TMainForm.SendCmdToActiveTabWM(Cmd: String; Text: String; X: Integer; Y: Integer);
begin
  if SmartTabs.Tabs.Count = 0 then exit;

  if SmartTabs.Tabs.Objects[SmartTabs.ActiveTab] <> nil then
    SendToTabWM((SmartTabs.Tabs.Objects[SmartTabs.ActiveTab] as TTabInfo).Handle, Cmd, Text, X, Y);
end;

procedure TMainForm.SendCmdToActiveTabPipe(Cmd: String; Text: String);
begin
  if SmartTabs.Tabs.Count = 0 then exit;

  if SmartTabs.Tabs.Objects[SmartTabs.ActiveTab] <> nil then
    SendToPipe((SmartTabs.Tabs.Objects[SmartTabs.ActiveTab] as TTabInfo).Pipe, Cmd, Text);
end;

procedure TMainForm.SendMsg(ToProcess: HWND; MsgRecord : TMsgRecord);
var
  copyDataStruct: TCopyDataStruct;
begin
  copyDataStruct.dwData := 0;// MsgRecord
  copyDataStruct.cbData := SizeOf(MsgRecord);
  copyDataStruct.lpData := @MsgRecord;

  SendMessage(ToProcess, WM_COPYDATA, Integer(Handle), Integer(@copyDataStruct));
end;

procedure TMainForm.PageProperties1Click(Sender: TObject);
begin
  SendCmdToActiveTabPipe('PageProperties', '');
end;

procedure TMainForm.Paste1Click(Sender: TObject);
begin
  SendCmdToActiveTabWM('Paste', '', 0, 0);
end;

procedure TMainForm.WidgetMouseEnter(Sender: TObject);
begin
  (Sender as TPaintBox32).Tag:= 1;
  (Sender as TPaintBox32).Repaint;
end;

procedure TMainForm.WidgetMouseLeave(Sender: TObject);
begin
  (Sender as TPaintBox32).Tag:= 0;
  (Sender as TPaintBox32).Repaint;
end;

procedure TMainForm.WidgetMenuClick(Sender: TObject);
var
  i: Integer;
begin
  for I := 0 to WidgetList.Count - 1 do
    if WidgetList.Objects[i] <> nil then
      begin
        SendToPipe((WidgetList.Objects[i] as TWidgetInfo).Pipe, 'Hide', '');
      end;

  SendToPipe((Sender as TMenuItem).Tag, 'Show', '');
end;

procedure TMainForm.WidgetMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  (Sender as TPaintBox32).Tag:= 2;
  (Sender as TPaintBox32).Repaint;
end;

procedure TMainForm.WidgetMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  (Sender as TPaintBox32).Tag:= 3;
  (Sender as TPaintBox32).Repaint;

  for I := 0 to WidgetList.Count - 1 do
    if WidgetList.Objects[i] <> nil then
      begin
        SendToPipe((WidgetList.Objects[i] as TWidgetInfo).Pipe, 'Hide', '');
      end;

  SendToPipe((Sender as TWidgetBtn).Pipe, 'Show', '');
end;

procedure TMainForm.WidgetPaintBuffer(Sender: TObject);
begin
  (Sender as TWidgetBtn).Buffer.Clear($00D2E3FA);
  (Sender as TWidgetBtn).Buffer.PenColor:= $00808080;
  (Sender as TWidgetBtn).Buffer.MoveTo(0, (Sender as TPaintBox32).Height - 1);

  case (Sender as TWidgetBtn).Tag of
    0: (Sender as TWidgetBtn).Buffer.Draw(0, 0, (Sender as TWidgetBtn).Bmp1);
    1: (Sender as TWidgetBtn).Buffer.Draw(0, 0, (Sender as TWidgetBtn).Bmp2);
    2: (Sender as TWidgetBtn).Buffer.Draw(2, 2, (Sender as TWidgetBtn).Bmp2);
    3: (Sender as TWidgetBtn).Buffer.Draw(0, 0, (Sender as TWidgetBtn).Bmp2);
  end;

  if SmartTabs.Tabs.Count <> 0 then
    (Sender as TPaintBox32).Buffer.LineToS((Sender as TPaintBox32).Buffer.Width, (Sender as TPaintBox32).Buffer.Height - 1);
end;

procedure TMainForm.PipeServerPipeDisconnect(Sender: TObject; Pipe: Cardinal);
var
  i: Integer;
begin
  for i := 0 to SmartTabs.Tabs.Count - 1 do
    if SmartTabs.Tabs.Objects[i] <> nil then
      begin
        if (SmartTabs.Tabs.Objects[i] as TTabInfo).Pipe = Pipe then
          begin
            SmartTabs.Tabs.Objects[i].Free;
            SmartTabs.DeleteTab(i);
            break;
          end;
      end;

  for i := 0 to WidgetList.Count - 1 do
    if WidgetList.Objects[i] <> nil then
      begin
        if (WidgetList.Objects[i] as TWidgetInfo).Pipe = Pipe then
          begin
            (WidgetList.Objects[i] as TWidgetInfo).Btn.Free;
            WidgetList.Objects[i].Free;
            WidgetList.Delete(i);
            break;
          end;
      end;
end;

procedure TMainForm.PipeServerPipeMessage(Sender: TObject; Pipe: Cardinal;
  Stream: TStream);
var
  s, Cmd, Text: String;
  p: Integer;
  Item: TMenuItem;
begin
  s:= StreamToString(Stream);
  p:= System.Pos(';', s);

  Cmd:= System.Copy(s, 0, p - 1);
  Text:= System.Copy(s, p + 1, Length(s) - p);

  if Cmd = 'LogIn' then
    begin
      SmartTabs.AddTab('New Tab');
      SmartTabs.Tabs.Objects[SmartTabs.Tabs.Count - 1]:= TTabInfo.Create;

      (SmartTabs.Tabs.Objects[SmartTabs.Tabs.Count - 1] as TTabInfo).Handle:= StrToInt(Text);
      (SmartTabs.Tabs.Objects[SmartTabs.Tabs.Count - 1] as TTabInfo).Pipe:= Pipe;

      SendToTabWM(StrToInt(Text), 'ReSize', '', TabPanel.Width, TabPanel.Height);
    end;

  if Cmd = 'LogInWidget' then
    begin
      s:= Text;
      p:= System.Pos(';', s);

      Cmd:= System.Copy(s, 0, p - 1);
      Text:= System.Copy(s, p + 1, Length(s) - p);

      WidgetList.Add(Cmd);
      WidgetList.Objects[WidgetList.Count - 1]:= TWidgetInfo.Create;

      (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Name:= Text;
      Text:= ExtractFilePath(Application.ExeName) + 'Widgets\' + Text + '\';

      (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Handle:= StrToInt(Cmd);
      (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Pipe:= Pipe;

      Item:= TMenuItem.Create(Widgets1);
      Item.Caption:= (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Name;
      Item.Tag:= Pipe;
      Item.OnClick:= WidgetMenuClick;
      Widgets1.Add(Item);

      (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Btn:= TWidgetBtn.Create(Self);
      (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Btn.Pipe:= Pipe;
      (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Btn.Align:= alRight;
      (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Btn.Width:= 32;
      (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Btn.Parent:= WidgetPanel;
      (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Btn.OnPaintBuffer:= WidgetPaintBuffer;
      (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Btn.OnMouseDown:= WidgetMouseDown;
      (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Btn.OnMouseUp:= WidgetMouseUp;
      (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Btn.OnMouseEnter:= WidgetMouseEnter;
      (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Btn.OnMouseLeave:= WidgetMouseLeave;

      (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Btn.Bmp1:= TBitmap32.Create;
      (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Btn.Bmp1.DrawMode:= dmBlend;

      (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Btn.Bmp2:= TBitmap32.Create;
      (WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Btn.Bmp2.DrawMode:= dmBlend;

      with TPortableNetworkGraphic32.Create do
        try
          LoadFromFile(Text + '\Icon1.png');
          AssignTo((WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Btn.Bmp1);
          LoadFromFile(Text + '\Icon2.png');
          AssignTo((WidgetList.Objects[WidgetList.Count - 1] as TWidgetInfo).Btn.Bmp2);
        finally
          Free;
        end;
    end;

  if Cmd = 'New' then
    begin
      NewTab(Text);
    end;

  if Cmd = 'Zoom' then
    begin
      SendCmdToActiveTabPipe('Zoom', Text);
    end;
end;

procedure TMainForm.Print1Click(Sender: TObject);
begin
  SendCmdToActiveTabPipe('Print', '');
end;

procedure TMainForm.PrintPreview1Click(Sender: TObject);
begin
  SendCmdToActiveTabPipe('PrintPreview', '');
end;

procedure TMainForm.PrintSetup1Click(Sender: TObject);
begin
  SendCmdToActiveTabPipe('PrintSetup', '');
end;

procedure TMainForm.ProgressChange(Sender: TObject);
begin
  if Progress.Progress = 0 then
    Progress.Visible:= false
  else
    Progress.Visible:= true;
end;

procedure TMainForm.Redo1Click(Sender: TObject);
begin
  SendCmdToActiveTabWM('Redo', '', 0, 0);
end;

procedure TMainForm.WMCopyData(var Msg : TWMCopyData);
var
  MsgRecord: TMsgRecord;
  i: Integer;
begin
  MsgRecord.Cmd:= TMsgRecord(Msg.copyDataStruct.lpData^).Cmd;
  MsgRecord.Msg:= TMsgRecord(Msg.copyDataStruct.lpData^).Msg;
  MsgRecord.X:= TMsgRecord(Msg.copyDataStruct.lpData^).X;
  MsgRecord.Y:= TMsgRecord(Msg.copyDataStruct.lpData^).Y;

  try
    if MsgRecord.Cmd = 'Title' then
      begin
        for i := 0 to SmartTabs.Tabs.Count - 1 do
          if SmartTabs.Tabs.Objects[i] <> nil then
            begin
              if (SmartTabs.Tabs.Objects[i] as TTabInfo).Handle = Msg.From then SmartTabs.Tabs.Strings[i]:= MsgRecord.Msg;
            end;

          SmartTabs.Repaint;
      end;

    if MsgRecord.Cmd = 'Status' then
      begin
          if SmartTabs.Tabs.Count <> 0 then
          if SmartTabs.Tabs.Objects[SmartTabs.ActiveTab] <> nil then
          if (SmartTabs.Tabs.Objects[SmartTabs.ActiveTab] as TTabInfo).Handle = Msg.From then
            if Length(MsgRecord.Msg) > 100 then
              StatusLabel.Caption:= Copy(MsgRecord.Msg, 0, 100) + '...'
            else
              StatusLabel.Caption:= MsgRecord.Msg;
      end;

    if MsgRecord.Cmd = 'Progress' then
      begin
          if SmartTabs.Tabs.Count <> 0 then
          if SmartTabs.Tabs.Objects[SmartTabs.ActiveTab] <> nil then
          if (SmartTabs.Tabs.Objects[SmartTabs.ActiveTab] as TTabInfo).Handle = Msg.From then
            Progress.Progress:= StrToInt(MsgRecord.Msg);
      end;

    if MsgRecord.Cmd = 'AddFavorite' then
      begin
        AddFavorite('Enter Title', MsgRecord.Msg);
      end;
  except

  end;
end;

end.
