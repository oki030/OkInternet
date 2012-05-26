unit Automation;

interface

uses Windows, SysUtils, Classes, Contnrs, TypInfo, ObjAuto, ObjComAuto,
  Graphics, Controls, Forms, Menus, StdCtrls, ComCtrls, CheckLst, Tabs, Grids, Dialogs;

type
  TAutoObjectDispatch = class(TObjectDispatch)
  protected
    function GetObjectDispatch(Obj: TObject): TObjectDispatch; override;
    function GetMethodInfo(const AName: ShortString; var AInstance: TObject): PMethodInfoHeader; override;
    function GetPropInfo(const AName: string; var AInstance: TObject; var CompIndex: Integer): PPropInfo; override;
  end;

{$METHODINFO ON}
  TObjectWrapper = class;
{$METHODINFO OFF}
  TObjectWrapperClass = class of TObjectWrapper;

  TClassMap = class(TObjectList)
  public
    procedure AddClass(AClass: TClass; AObjWrapper: TObjectWrapperClass);
    procedure RemoveClass(AClass: TClass; AObjWrapper: TObjectWrapperClass);
    function FindObjectWrapper(AClass: TClass): TObjectWrapperClass;
  end;

  { TObjectWrapper }

{$METHODINFO ON}
  TObjectWrapper = class(TObject)
  private
    function GetClassName: string;
    function GetParentClass: string;
  protected
    FObject: TObject;
  public
    constructor Connect(AObject: TObject); virtual;
    function InheritsFrom(const ClassName: string): Boolean;
  published
    property ParentClass: string read GetParentClass;
    property ClassName: string read GetClassName;
  end;

{$METHODINFO OFF}

{ TApplicationWrapper }

  TApplicationWrapper = class(TObjectWrapper)
  private
    FHeight, FWidth: Integer;
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
  public
    constructor Connect(AObject: TObject); override;
    function GetObjFromHandle(Handle: Integer): TObject;
  published
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;

    procedure WriteIniString(Section: String; Ident: String; Value: String);
    procedure WriteIniInteger(Section: String; Ident: String; Value: Integer);
    procedure WriteIniFloat(Section: String; Ident: String; Value: Extended);
    procedure WriteIniBool(Section: String; Ident: String; Value: Boolean);

    function ReadIniString(Section: String; Ident: String): String;
    function ReadIniInteger(Section: String; Ident: String): Integer;
    function ReadIniFloat(Section: String; Ident: String): Double;
    function ReadIniBool(Section: String; Ident: String): Boolean;

    procedure SendCmd(Cmd: String; Text: String);
    procedure ClearCache();
    procedure Close;
  end;

function ClassMap: TClassMap;

implementation

uses
  WidgetUnit;

resourcestring
  sAppWrapperOnly = 'Automation wrapper for the Application can only be created with a TApplication';
  sClassAlreadyInMap = 'Class ''%s'', already in class map';

var
  FClassMap: TClassMap;

{ TAutoObjectDispatch }

function TAutoObjectDispatch.GetMethodInfo(const AName: ShortString; var AInstance: TObject): PMethodInfoHeader;
begin
  Result := inherited GetMethodInfo(AName, AInstance);
  if (Result = nil) and (Instance is TObjectWrapper) then
  begin
    Result := ObjAuto.GetMethodInfo(TObjectWrapper(Instance).FObject, AName);
    if Result <> nil then
    begin
      AInstance := TObjectWrapper(Instance).FObject;
      Exit;
    end;
  end;
end;

function TAutoObjectDispatch.GetObjectDispatch(Obj: TObject): TObjectDispatch;
var
  ObjWrap: TObjectWrapperClass;
begin
  ObjWrap := ClassMap.FindObjectWrapper(Obj.ClassType);
  if ObjWrap <> nil then
    Result := TAutoObjectDispatch.Create(ObjWrap.Connect(Obj), True)
  else
    Result := nil;
end;

function TAutoObjectDispatch.GetPropInfo(const AName: string; var AInstance: TObject;
  var CompIndex: Integer): PPropInfo;
var
  Component: TComponent;
begin
  Result := inherited GetPropInfo(AName, AInstance, CompIndex);
  if (Result = nil) and (Instance is TObjectWrapper) then
  begin
    Result := TypInfo.GetPropInfo(TObjectWrapper(Instance).FObject, AName);
    if Result <> nil then
    begin
      AInstance := TObjectWrapper(Instance).FObject;
      Exit;
    end else if TObjectWrapper(Instance).FObject is TComponent then
    begin
      // Not a property, try a sub component
      Component := TComponent(TObjectWrapper(Instance).FObject).FindComponent(AName);
      if Component <> nil then
      begin
        AInstance := TObjectWrapper(Instance).FObject;
        CompIndex := Component.ComponentIndex;
      end;
    end else
      AInstance := nil;
  end;
end;

function ClassMap: TClassMap;
begin
  if FClassMap = nil then
    FClassMap := TClassMap.Create;
  Result := FClassMap;
end;

{ TObjectWrapper }

constructor TObjectWrapper.Connect(AObject: TObject);
begin
  FObject := AObject;
end;

function TObjectWrapper.GetClassName: string;
begin
  Result := FObject.ClassName;
end;

function TObjectWrapper.GetParentClass: string;
begin
  if FObject.ClassParent <> nil then
    Result := FObject.ClassParent.ClassName
  else
    Result := '';
end;

function TObjectWrapper.InheritsFrom(const ClassName: string): Boolean;
var
  AClass: TClass;
begin
  AClass := FObject.ClassType;
  while (AClass <> nil) and not AClass.ClassNameIs(ClassName) do
    AClass := AClass.ClassParent;
  Result := AClass <> nil;
end;

{ TApplicationWrapper }

constructor TApplicationWrapper.Connect(AObject: TObject);
begin
  if AObject <> Application then
    raise Exception.Create(sAppWrapperOnly);
  inherited;
end;

function TApplicationWrapper.GetObjFromHandle(Handle: Integer): TObject;
begin
  Result := FindControl(Handle);
end;

procedure TApplicationWrapper.WriteIniString(Section: String; Ident: String; Value: String);
begin
  Widget.WidgetIni.WriteString(Section, Ident, Value);
  Widget.WidgetIni.UpdateFile;
end;

procedure TApplicationWrapper.WriteIniInteger(Section: String; Ident: String; Value: Integer);
begin
  Widget.WidgetIni.WriteInteger(Section, Ident, Value);
  Widget.WidgetIni.UpdateFile;
end;

procedure TApplicationWrapper.WriteIniFloat(Section: String; Ident: String; Value: Extended);
begin
  Widget.WidgetIni.WriteFloat(Section, Ident, Value);
  Widget.WidgetIni.UpdateFile;
end;

procedure TApplicationWrapper.WriteIniBool(Section: String; Ident: String; Value: Boolean);
begin
  Widget.WidgetIni.WriteBool(Section, Ident, Value);
  Widget.WidgetIni.UpdateFile;
end;

function TApplicationWrapper.ReadIniString(Section: String; Ident: String): String;
begin
  Result:= Widget.WidgetIni.ReadString(Section, Ident, '');
end;

function TApplicationWrapper.ReadIniInteger(Section: String; Ident: String): Integer;
begin
  Result:= Widget.WidgetIni.ReadInteger(Section, Ident, 0);
end;

function TApplicationWrapper.ReadIniFloat(Section: String; Ident: String): Double;
begin
  Result:= Widget.WidgetIni.ReadFloat(Section, Ident, 0);
end;

function TApplicationWrapper.ReadIniBool(Section: String; Ident: String): Boolean;
begin
  Result:= Widget.WidgetIni.ReadBool(Section, Ident, false);
end;

procedure TApplicationWrapper.SetWidth(Value: Integer);
begin
  FWidth:= Value;
  Widget.Width:= Value;
end;

procedure TApplicationWrapper.SetHeight(Value: Integer);
begin
  FHeight:= Value;
  Widget.Height:= Value;
end;

procedure TApplicationWrapper.SendCmd(Cmd: String; Text: String);
begin
  Widget.SendToHostPipe(Cmd, Text);
end;

procedure TApplicationWrapper.ClearCache();
begin
  Widget.Browser.ClearCache;
end;

procedure TApplicationWrapper.Close;
begin
  Widget.Hide;
end;

{ TAutoRecord }

type
  TAutoRecord = class
  private
    FClass: TClass;
    FObjWrapper: TObjectWrapperClass;
  public
    constructor Create(AClass: TClass; AObjWrapper: TObjectWrapperClass);
    function GetWrapper(AClass: TClass): TObjectWrapperClass;
    function Equal(AClass: TClass): Boolean;
  end;

{ TClassMap }

procedure TClassMap.AddClass(AClass: TClass; AObjWrapper: TObjectWrapperClass);
var
  I: Integer;
begin
  I := 0;
  while I < Count do
  begin
    with TAutoRecord(Items[I]) do
      if AClass.InheritsFrom(FClass) then
        if AClass = FClass then
          raise Exception.CreateFmt(sClassAlreadyInMap, [AClass.ClassName])
        else
          Break;
    Inc(I);      
  end;
  if I < Count then
    inherited Insert(I, TAutoRecord.Create(AClass, AObjWrapper))
  else
    inherited Add(TAutoRecord.Create(AClass, AObjWrapper));
end;

function TClassMap.FindObjectWrapper(AClass: TClass): TObjectWrapperClass;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with TAutoRecord(Items[I]) do
    begin
      Result := GetWrapper(AClass);
      if Result <> nil then
        Exit;
    end;
  Result := nil;
end;

procedure TClassMap.RemoveClass(AClass: TClass; AObjWrapper: TObjectWrapperClass);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    with TAutoRecord(Items[I]) do
      if Equal(AClass) and (GetWrapper(AClass) = AObjWrapper) then
        Delete(I);
end;

{ TAutoRecord }

constructor TAutoRecord.Create(AClass: TClass; AObjWrapper: TObjectWrapperClass);
begin
  FClass := AClass;
  FObjWrapper := AObjWrapper;
end;

function TAutoRecord.Equal(AClass: TClass): Boolean;
begin
  Result := AClass = FClass;
end;

function TAutoRecord.GetWrapper(AClass: TClass): TObjectWrapperClass;
begin
  if AClass.InheritsFrom(FClass) then
    Result := FObjWrapper
  else
    Result := nil;
end;

initialization
  with ClassMap do
  begin
    AddClass(TObject, TObjectWrapper);
    AddClass(TApplication, TApplicationWrapper);
  end;
end.
