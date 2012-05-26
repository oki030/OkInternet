unit BrowserUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, OleCtrls, SHDocVw_EWB, EwbCore, EmbeddedWB, StdCtrls, Pipes,
  ToolWin, ComCtrls, XPMan, sToolBar, sSkinManager, sEdit, sButton, sComboBox,
  sPanel, Buttons, sSpeedButton, sStatusBar, acAlphaHints, Menus, FavoritesPopup, MSHTML_EWB,
  uPSComponent, IniFiles;

type
  TMsgRecord = packed record
    Cmd: String[255];
    Msg : String[255];
    X: Integer;
    Y: Integer;
  end;

type
  TBrowserForm = class(TForm)
    Browser: TEmbeddedWB;
    PipeClient: TPipeClient;
    sSkinManager: TsSkinManager;
    Panel1: TPanel;
    sPanel1: TsPanel;
    sSpeedButton1: TsSpeedButton;
    sSpeedButton2: TsSpeedButton;
    sSpeedButton3: TsSpeedButton;
    sSpeedButton4: TsSpeedButton;
    AddressEdit: TsComboBox;
    SearchEdit: TsEdit;
    sSpeedButton5: TsSpeedButton;
    sSpeedButton6: TsSpeedButton;
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PipeClientPipeDisconnect(Sender: TObject; Pipe: Cardinal);
    procedure PipeClientPipeMessage(Sender: TObject; Pipe: Cardinal;
      Stream: TStream);
    procedure sPanel1Paint(Sender: TObject; Canvas: TCanvas);
    procedure sSpeedButton1Click(Sender: TObject);
    procedure sSpeedButton2Click(Sender: TObject);
    procedure sSpeedButton3Click(Sender: TObject);
    procedure sSpeedButton4Click(Sender: TObject);
    procedure AddressEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure BrowserCommandStateChange(ASender: TObject; Command: Integer;
      Enable: WordBool);
    procedure AddressEditEnter(Sender: TObject);
    procedure AddressEditExit(Sender: TObject);
    procedure SearchEditEnter(Sender: TObject);
    procedure SearchEditExit(Sender: TObject);
    procedure sSpeedButton6Click(Sender: TObject);
    procedure BrowserNavigateComplete2(ASender: TObject; const pDisp: IDispatch;
      var URL: OleVariant);
    procedure BrowserClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BrowserTitleChange(ASender: TObject; const Text: WideString);
    procedure BrowserStatusTextChange(ASender: TObject; const Text: WideString);
    procedure BrowserProgressChange(ASender: TObject; Progress,
      ProgressMax: Integer);
    procedure sSpeedButton5Click(Sender: TObject);
    procedure SearchEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BrowserDocumentComplete(ASender: TObject; const pDisp: IDispatch;
      var URL: OleVariant);
    procedure BrowserBeforeNavigate2(ASender: TObject; const pDisp: IDispatch;
      var URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
      var Cancel: WordBool);
    procedure BrowserNewWindow3(ASender: TObject; var ppDisp: IDispatch;
      var Cancel: WordBool; dwFlags: Cardinal; const bstrUrlContext,
      bstrUrl: WideString);
  private
    { Private declarations }
    procedure WMCopyData(var Msg : TWMCopyData); message WM_COPYDATA;
  public
    { Public declarations }
    Host: HWND;

    procedure Init;
    procedure SendToHostPipe(Cmd: String; Text: String);
    procedure SendToHostWM(Cmd: String; Text: String);
    procedure SendMsg(MsgRecord : TMsgRecord);

    procedure ShowNewMessage(const Message: string);
    procedure OnCompile(Sender: TPSScript);
  end;

var
  BrowserForm: TBrowserForm;

implementation

{$R *.dfm}

procedure PostKeyExHWND(hWindow: HWnd; key: Word; const shift: TShiftState;
specialkey: Boolean);
{************************************************* ***********
* Procedure PostKeyEx
*
* Parameters:
* hWindow: target window to be send the keystroke
* key : virtual keycode of the key to send. For printable
* keys this is simply the ANSI code (Ord(character)).
* shift : state of the modifier keys. This is a set, so you
* can set several of these keys (shift, control, alt,
* mouse buttons) in tandem. The TShiftState type is
* declared in the Classes Unit.
* specialkey: normally this should be False. Set it to True to
* specify a key on the numeric keypad, for example.
* If this parameter is true, bit 24 of the lparam for
* the posted WM_KEY* messages will be set.
* Description:
* This procedure sets up Windows key state array to correctly
* reflect the requested pattern of modifier keys and then posts
* a WM_KEYDOWN/WM_KEYUP message pair to the target window. Then
* Application.ProcessMessages is called to process the messages
* before the keyboard state is restored.
* Error Conditions:
* May fail due to lack of memory for the two key state buffers.
* Will raise an exception in this case.
* NOTE:
* Setting the keyboard state will not work across applications
* running in different memory spaces on Win32 unless AttachThreadInput
* is used to connect to the target thread first.
*Created: 02/21/96 16:39:00 by P. Below
************************************************** **********}
type
TBuffers = array [0..1] of TKeyboardState;
var
pKeyBuffers: ^TBuffers;
lParam: LongInt;
begin
(* check if the target window exists *)
if IsWindow(hWindow) then
begin
(* set local variables to default values *)
pKeyBuffers := nil;
lParam := MakeLong(0, MapVirtualKey(key, 0));

(* modify lparam if special key requested *)
if specialkey then
lParam := lParam or $1000000;

(* allocate space for the key state buffers *)
New(pKeyBuffers);
try
(* Fill buffer 1 with current state so we can later restore it.
Null out buffer 0 to get a "no key pressed" state. *)
GetKeyboardState(pKeyBuffers^[1]);
FillChar(pKeyBuffers^[0], SizeOf(TKeyboardState), 0);

(* set the requested modifier keys to "down" state in the buffer*)
if ssShift in shift then
pKeyBuffers^[0][VK_SHIFT] := $80;
if ssAlt in shift then
begin
(* Alt needs special treatment since a bit in lparam needs also be set *)
pKeyBuffers^[0][VK_MENU] := $80;
lParam := lParam or $20000000;
end;
if ssCtrl in shift then
pKeyBuffers^[0][VK_CONTROL] := $80;
if ssLeft in shift then
pKeyBuffers^[0][VK_LBUTTON] := $80;
if ssRight in shift then
pKeyBuffers^[0][VK_RBUTTON] := $80;
if ssMiddle in shift then
pKeyBuffers^[0][VK_MBUTTON] := $80;

(* make out new key state array the active key state map *)
SetKeyboardState(pKeyBuffers^[0]);
(* post the key messages *)
if ssAlt in Shift then
begin
PostMessage(hWindow, WM_SYSKEYDOWN, key, lParam);
PostMessage(hWindow, WM_SYSKEYUP, key, lParam or $C0000000);
end
else
begin
PostMessage(hWindow, WM_KEYDOWN, key, lParam);
PostMessage(hWindow, WM_KEYUP, key, lParam or $C0000000);
end;
(* process the messages *)
Application.ProcessMessages;

(* restore the old key state map *)
SetKeyboardState(pKeyBuffers^[1]);
finally
(* free the memory for the key state buffers *)
if pKeyBuffers <> nil then
Dispose(pKeyBuffers);
end; { If }
end;
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

procedure TBrowserForm.ShowNewMessage(const Message: string);
begin
  ShowMessage(Message);
end;

procedure TBrowserForm.OnCompile(Sender: TPSScript);
begin
  Sender.AddFunction(@Pos, 'function Pos(subStr: String; Str: String): Integer;');
  Sender.AddMethod(Self, @TBrowserForm.ShowNewMessage, 'procedure ShowNewMessage(const Message: string);');
end;

procedure TBrowserForm.BrowserBeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
var
  Script: TPSScript;
  WidgetIni: TMemIniFile;
  i, j: Integer;
  WidgetName, s: String;
begin
  try
    Script:= TPSScript.Create(Self);
    Script.OnCompile:= OnCompile;

    WidgetIni:= TMemIniFile.Create(ExtractFilePath(Application.ExeName) + 'Widgets\widgets.ini');
    j:= WidgetIni.ReadInteger('Settings', 'NumberOfWidgets', 0);

    for i:= 1 to j do
      begin
        WidgetName:= ExtractFilePath(Application.ExeName) + 'Widgets\' + WidgetIni.ReadString('Widgets', 'W' + IntToStr(i), '')  + '\Events.pas';
        Script.Script.LoadFromFile(WidgetName);
        Script.Compile;
        try
          if URL = 'about:blank' then
            s:= TargetFrameName
          else
            s:= URL;

          Cancel:= Script.ExecuteFunction([s], 'OnBeforeNavigate');
        except
        end;
      end;

    FreeAndNil(Script);
    FreeAndNil(WidgetIni);
  except

  end;
end;

procedure TBrowserForm.BrowserClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SearchEdit.OnExit(SearchEdit);
  AddressEdit.OnExit(AddressEdit);
end;

procedure TBrowserForm.BrowserCommandStateChange(ASender: TObject;
  Command: Integer; Enable: WordBool);
begin
  case command of
    CSC_NAVIGATEFORWARD : sSpeedButton4.Enabled:= Enable;
    CSC_NAVIGATEBACK    : sSpeedButton1.Enabled:= Enable;
  end;
end;

procedure TBrowserForm.BrowserDocumentComplete(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
var
  Script: TPSScript;
  WidgetIni: TMemIniFile;
  i, j: Integer;
  WidgetName, s: String;
begin
  try
    Script:= TPSScript.Create(Self);
    Script.OnCompile:= OnCompile;

    WidgetIni:= TMemIniFile.Create(ExtractFilePath(Application.ExeName) + 'Widgets\widgets.ini');
    j:= WidgetIni.ReadInteger('Settings', 'NumberOfWidgets', 0);

    for i:= 1 to j do
      begin
        WidgetName:= ExtractFilePath(Application.ExeName) + 'Widgets\' + WidgetIni.ReadString('Widgets', 'W' + IntToStr(i), '')  + '\Events.pas';
        Script.Script.LoadFromFile(WidgetName);
        Script.Compile;
        try
          s:= URL;
          Script.ExecuteFunction([s], 'OnDocumentComplete');
        except
        end;
      end;

    FreeAndNil(Script);
    FreeAndNil(WidgetIni);
  except

  end;

  if AddressEdit.Items.IndexOf(URL) = -1 then AddressEdit.Items.Add(URL);
end;

procedure TBrowserForm.BrowserNavigateComplete2(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  if URL = 'about:blank' then exit;
  if Pos('\NewTab\index.html', URL) <> 0 then
    begin
      AddressEdit.Text:= ' Enter address';
      exit;
    end;

  if Pos('/NewTab/index.html', URL) <> 0 then
    begin
      AddressEdit.Text:= ' Enter address';
      exit;
    end;

  AddressEdit.Text:= Browser.LocationURL;
end;

procedure TBrowserForm.BrowserNewWindow3(ASender: TObject;
  var ppDisp: IDispatch; var Cancel: WordBool; dwFlags: Cardinal;
  const bstrUrlContext, bstrUrl: WideString);
var
  Script: TPSScript;
  WidgetIni: TMemIniFile;
  i, j: Integer;
  Can: Boolean;
  WidgetName, s: String;
begin
  Cancel:= true;
  Can:= true;

  try
    Script:= TPSScript.Create(Self);
    Script.OnCompile:= OnCompile;

    WidgetIni:= TMemIniFile.Create(ExtractFilePath(Application.ExeName) + 'Widgets\widgets.ini');
    j:= WidgetIni.ReadInteger('Settings', 'NumberOfWidgets', 0);

    for i:= 1 to j do
      begin
        WidgetName:= ExtractFilePath(Application.ExeName) + 'Widgets\' + WidgetIni.ReadString('Widgets', 'W' + IntToStr(i), '')  + '\Events.pas';
        Script.Script.LoadFromFile(WidgetName);
        Script.Compile;
        try
          s:= bstrUrl;
          can:= Script.ExecuteFunction([s], 'OnNewWindow');
          if not can then break;
        except
        end;
      end;

      if can then SendToHostPipe('New', bstrUrl);

    FreeAndNil(Script);
    FreeAndNil(WidgetIni);
  except

  end;
end;

procedure TBrowserForm.BrowserProgressChange(ASender: TObject; Progress,
  ProgressMax: Integer);
begin
  SendToHostWM('Progress', IntToStr(Progress));
end;

procedure TBrowserForm.BrowserStatusTextChange(ASender: TObject;
  const Text: WideString);
begin
  SendToHostWM('Status', Text);
end;

procedure TBrowserForm.BrowserTitleChange(ASender: TObject;
  const Text: WideString);
begin
  SendToHostWM('Title', Text);
end;

procedure TBrowserForm.FormActivate(Sender: TObject);
begin
  ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TBrowserForm.Init;
begin
  if PipeClient.Connect(100) then
    begin
      SendToHostPipe('LogIn', IntToStr(BrowserForm.Handle));
    end;
end;

procedure TBrowserForm.FormCreate(Sender: TObject);
begin
  Browser.Navigate('about:blank');
end;

procedure TBrowserForm.FormResize(Sender: TObject);
begin
  AddressEdit.Width:= BrowserForm.Width - 385;
  sSpeedButton5.Left:= AddressEdit.Left + AddressEdit.Width + 5;
  SearchEdit.Left:= sSpeedButton5.Left + sSpeedButton5.Width + 5;
  sSpeedButton6.Left:= SearchEdit.Left + SearchEdit.Width + 5;
end;

procedure TBrowserForm.SendToHostPipe(Cmd: String; Text: String);
var
  s: String;
begin
  s:= Cmd + ';' + Text;
  PipeClient.Write(s[1], Length(s) * 2);
end;

procedure TBrowserForm.SendToHostWM(Cmd: String; Text: String);
var
  MsgRecord: TMsgRecord;
begin
    MsgRecord.Cmd:= Cmd;
    MsgRecord.Msg:= Text;

    SendMsg(MsgRecord);
end;

procedure TBrowserForm.sPanel1Paint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Pen.Color:= clWhite;
  Canvas.MoveTo(4, 0);
  Canvas.LineTo(Browser.Width - 4, 0);
end;

procedure TBrowserForm.sSpeedButton1Click(Sender: TObject);
begin
  Browser.GoBack;
end;

procedure TBrowserForm.sSpeedButton2Click(Sender: TObject);
begin
  Browser.Refresh;
end;

procedure TBrowserForm.sSpeedButton3Click(Sender: TObject);
begin
  Browser.Stop;
end;

procedure TBrowserForm.sSpeedButton4Click(Sender: TObject);
begin
  Browser.GoForward;
end;

procedure TBrowserForm.sSpeedButton5Click(Sender: TObject);
var
  Ini: TMemIniFile;
  Page: TStrings;
  i, j: Integer;
  url, url2: String;
  GoOut: Boolean;
label l1;
begin
  try
    Ini:= TMemIniFile.Create(ExtractFilePath(Application.ExeName) + 'NewTab\SpeedDial.ini');
    Page:= TStringList.Create;
    Page.LoadFromFile(ExtractFilePath(Application.ExeName) + 'NewTab\index.part');

    j:= Ini.ReadInteger('SpeedDial', 'No', 0);

    GoOut:= false;
    for i := 0 to j - 1 do
      begin
        url:= Ini.ReadString('SpeedDial', 'URL' + IntToStr(i), '');
        url2:= url;
        if length(url2) > 25 then url2:= Copy(url2, 0, 25) + '...';

        Page.Add('<div><a href="#" onclick="window.location = ''' + url + '''"><img src="images/' + IntToStr(i) + '.bmp" /></a><p align="center">' + url2 + '</p></div>');

        if url = Browser.LocationURL then
          begin
            GoOut:= true;
            Browser.GetBmpFromBrowser(ExtractFilePath(Application.ExeName) + 'NewTab\images\' + IntToStr(i) + '.bmp');
          end;
      end;

    if GoOut then goto l1;

    url:= Browser.LocationURL;
    url2:= url;
    if length(url2) > 25 then url2:= Copy(url2, 0, 25) + '...';

    if Pos('/NewTab/index.html', url) <> 0 then goto l1;
    if Pos('\NewTab\index.html', url) <> 0 then goto l1;

    if (Browser.LocationURL <> 'about:blank') then
      begin
        Ini.WriteInteger('SpeedDial', 'No', j + 1);
        Ini.WriteString('SpeedDial', 'URL' + IntToStr(j), Browser.LocationURL);
        Page.Add('<div><a href="#" onclick="window.location = ''' + Browser.LocationURL + '''"><img src="images/' + IntToStr(j) + '.bmp" /></a><p align="center">' + url2 + '</p></div>');
        Browser.GetBmpFromBrowser(ExtractFilePath(Application.ExeName) + 'NewTab\images\' + IntToStr(j) + '.bmp');
        ShowMessage('Site added to Speed Dial.');
      end;

l1: Page.Add('</div></body></html>');
    Page.SaveToFile(ExtractFilePath(Application.ExeName) + 'NewTab\index.html');
    Ini.UpdateFile;

    FreeAndNil(Page);
    FreeAndNil(Ini);
  except

  end;
end;

procedure TBrowserForm.sSpeedButton6Click(Sender: TObject);
begin
  if SearchEdit.Text = '  Search with Google' then exit;

  Browser.Navigate('http://www.google.rs/search?q=' + SearchEdit.Text + '&sourceid=opera&num=0&ie=utf-8&oe=utf-8');
end;

procedure TBrowserForm.SearchEditEnter(Sender: TObject);
begin
  //SearchEdit.Font.Color:= $004B4B4B;
  if SearchEdit.Text = '  Search with Google' then SearchEdit.Text:= '';
end;

procedure TBrowserForm.SearchEditExit(Sender: TObject);
begin
  //SearchEdit.Font.Color:= $00AEAEAE;
  if SearchEdit.Text = '' then
    begin
      SearchEdit.Text:= '  Search with Google';
    end;
end;

procedure TBrowserForm.SearchEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
    begin
      Browser.Navigate('http://www.google.rs/search?q=' + SearchEdit.Text + '&sourceid=opera&num=0&ie=utf-8&oe=utf-8');
    end;
end;

procedure TBrowserForm.SendMsg(MsgRecord : TMsgRecord);
var
  copyDataStruct: TCopyDataStruct;
begin
  copyDataStruct.dwData := 0;// MsgRecord
  copyDataStruct.cbData := SizeOf(MsgRecord);
  copyDataStruct.lpData := @MsgRecord;

  SendMessage(Host, WM_COPYDATA, Integer(Handle), Integer(@copyDataStruct));
end;

procedure TBrowserForm.FormShow(Sender: TObject);
begin
  ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TBrowserForm.PipeClientPipeDisconnect(Sender: TObject;
  Pipe: Cardinal);
begin
  Application.Terminate;
end;

procedure TBrowserForm.PipeClientPipeMessage(Sender: TObject; Pipe: Cardinal;
  Stream: TStream);
var
  s, Cmd, Text: String;
  p: Integer;
begin
  s:= StreamToString(Stream);
  p:= System.Pos(';', s);

  Cmd:= System.Copy(s, 0, p - 1);
  Text:= System.Copy(s, p + 1, Length(s) - p);

  if Cmd = 'Open' then Browser.OpenDialog;

  if Cmd = 'Save' then Browser.SaveDialog;

  if Cmd = 'Print' then Browser.Print;

  if Cmd = 'PrintSetup' then Browser.PrintSetup;

  if Cmd = 'PrintPreview' then Browser.PrintPreview;

  if Cmd = 'PageProperties' then Browser.ShowPageProperties;

  if Cmd = 'InternetOptions' then Browser.ShowInternetOptions;

  if Cmd = 'Zoom' then Browser.ZoomPercent:= StrToInt(Text);
end;

procedure TBrowserForm.AddressEditEnter(Sender: TObject);
begin
  //AddressEdit.Font.Color:= $004B4B4B;
  if AddressEdit.Text = ' Enter address' then AddressEdit.Text:= '';
end;

procedure TBrowserForm.AddressEditExit(Sender: TObject);
begin
  if AddressEdit.Text = '' then
    begin
      //AddressEdit.Font.Color:= $00AEAEAE;
      AddressEdit.Text:= ' Enter address';
    end;
end;

procedure TBrowserForm.AddressEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
    begin
      Browser.Navigate(Trim(AddressEdit.Text));
    end;
end;

procedure TBrowserForm.WMCopyData(var Msg : TWMCopyData);
var
  MsgRecord: TMsgRecord;
begin
  MsgRecord.Cmd:= TMsgRecord(Msg.copyDataStruct.lpData^).Cmd;
  MsgRecord.Msg:= TMsgRecord(Msg.copyDataStruct.lpData^).Msg;
  MsgRecord.X:= TMsgRecord(Msg.copyDataStruct.lpData^).X;
  MsgRecord.Y:= TMsgRecord(Msg.copyDataStruct.lpData^).Y;

  if MsgRecord.Cmd = 'ReSize' then
    begin
      BrowserForm.Width:= MsgRecord.X;
      BrowserForm.Height:= MsgRecord.Y;
    end;

  if MsgRecord.Cmd = 'Muve' then
    begin
      BrowserForm.Left:= MsgRecord.X;
      BrowserForm.Top:= MsgRecord.Y;
    end;

  if MsgRecord.Cmd = 'Show' then
    begin
      BrowserForm.Visible:= true;
    end;

  if MsgRecord.Cmd = 'Hide' then
    begin
      BrowserForm.Visible:= false;
    end;

  if MsgRecord.Cmd = 'Quit' then
    begin
      Application.Terminate;
    end;

  if MsgRecord.Cmd = 'Go' then
    begin
      Browser.Navigate(MsgRecord.Msg);
    end;

  if MsgRecord.Cmd = 'Undo' then Browser.Undo;

  if MsgRecord.Cmd = 'Redo' then Browser.Redo;

  if MsgRecord.Cmd = 'Cut' then Browser.Cut;

  if MsgRecord.Cmd = 'Copy' then Browser.Copy;

  if MsgRecord.Cmd = 'Paste' then Browser.Paste;

  if MsgRecord.Cmd = 'Delete' then Browser.Delete;

  if MsgRecord.Cmd = 'SelectAll' then Browser.SelectAll;

  if MsgRecord.Cmd = 'Find' then Browser.ShowFindDialog;
end;

end.
