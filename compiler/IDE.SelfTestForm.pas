unit IDE.SelfTestForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.TreeView, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo,
  IDE.SelfTest;


type
  TSelfTestF = class(TForm)
    tvTests: TTreeView;
    Layout1: TLayout;
    Layout2: TLayout;
    btnLoadTests: TButton;
    btnAll: TButton;
    btnNone: TButton;
    btnRun: TButton;
    btnFailed: TButton;
    mmReport: TMemo;
    mmDetails: TMemo;
    lbTestName: TLabel;
    Layout3: TLayout;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure btnLoadTestsClick(Sender: TObject);
    procedure tvTestsChangeCheck(Sender: TObject);
    procedure btnAllClick(Sender: TObject);
    procedure btnNoneClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnFailedClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvTestsChange(Sender: TObject);
  private
    //If > 0 we're in UpdateChecks and calls to ChangeCheck should be ignored
    FInUpdateChecks: Integer;


    function ItemToTest(Item: TTreeViewItem): TTestable;
    //Updates check marks for the item and any child items based on the Run properties
    //of the data
    procedure UpdateChecks(Item: TTreeViewItem);
    //Update the item, and all it's child items to show test result status
    procedure ItemShowResults(Item: TTreeViewItem);
    procedure EnableIfFailed(Item: TTreeViewItem);

    procedure RunItemTests(Item: TTreeViewItem);
  public
    { Public declarations }
  end;

var
  SelfTestF: TSelfTestF;

implementation

{$R *.fmx}

procedure TSelfTestF.btnAllClick(Sender: TObject);
var I: Integer;
begin
  try
    tvTests.BeginUpdate;
    for I := 0 to tvTests.Count-1 do
      tvTests.Items[I].IsChecked := True;
  finally
    tvTests.EndUpdate;
  end;
end;

procedure TSelfTestF.btnFailedClick(Sender: TObject);
var I: Integer;
begin
  try
    tvTests.BeginUpdate;
    for I := 0 to tvTests.Count-1 do
      EnableIfFailed(tvTests.Items[I]);
  finally
    tvTests.EndUpdate;
  end;
end;

procedure TSelfTestF.btnLoadTestsClick(Sender: TObject);
var List: TTestGroup;
  FileItem: TTreeViewItem;
  TestItem: TTreeViewItem;
  I: Integer;
begin
  tvTests.Clear;
  LoadSelfTests;

  try
    tvTests.BeginUpdate;

    //Now load the files + tests in ListView
    for List in TestGroups do
    begin
      FileItem := TTreeViewItem.Create(tvTests);
      tvTests.AddObject(FileItem);
      FileItem.Text := List.Name;
      FileItem.TagObject := List;
      FileItem.IsChecked := List.IsEnabled;
      for I := 0 to List.Count-1 do
      begin
        TestItem := TTreeViewItem.Create(FileItem);
        FileItem.AddObject(TestItem);
        TestItem.Text := List[I].Name;
        TestItem.TagObject := List[I];
        TestItem.IsChecked := List[I].IsEnabled;
      end;
    end;
  finally
    tvTests.EndUpdate;
  end;
end;

procedure TSelfTestF.btnNoneClick(Sender: TObject);
var I: Integer;
begin
  try
    tvTests.BeginUpdate;
    for I := 0 to tvTests.Count-1 do
      tvTests.Items[I].IsChecked := False;
  finally
    tvTests.EndUpdate;
  end;
end;

procedure TSelfTestF.btnRunClick(Sender: TObject);
var I: Integer;
begin
//  if tvTests.Selected = nil then
  begin //Run all tests
    for I := 0 to tvTests.Count-1 do
//      if tvTests.Items[I].IsChecked then
        RunItemTests(tvTests.Items[I]);
  end;
{  else
    RunItemTests(tvTests.Selected);
}end;

procedure TSelfTestF.EnableIfFailed(Item: TTreeViewItem);
var I: Integer;
  Test: TTestable;
begin
  try
    tvTests.BeginUpdate;
    Test := ItemToTest(Item);
    if Assigned(Test) then
      Item.IsChecked := Test.Status <> tsPassed;

    for I := 0 to Item.Count-1 do
      EnableIfFailed(Item.Items[I]);
  finally
    tvTests.EndUpdate;
  end;
end;

procedure TSelfTestF.FormShow(Sender: TObject);
begin
  btnLoadTestsClick(Self);
  lbTestName.Text := '';
end;

procedure TSelfTestF.ItemShowResults(Item: TTreeViewItem);
var Status: TTestStatus;
  Test: TTestable;
begin
  Test := ItemToTest(Item);
  if Assigned(Test) then
    Status := Test.Status
  else
    Status := tsNotRun;

  //TODO: Show status
  Item.StyledSettings := Item.StyledSettings - [TStyledSetting.FontColor];
  case Status of
    tsNotRun: Item.FontColor := TAlphaColorRec.White;
    tsPassed: Item.FontColor := TAlphaColorRec.Green;
    tsFailed: Item.FontColor := TAlphaColorRec.Orange;
    tsError: Item.FontColor := TAlphaColorRec.Red;
  else
    raise Exception.Create('Unknown test Status');
  end;

  if Item = tvTests.Selected then
    if Assigned(Test) then
    begin
      Test := Item.TagObject as TTestable;
      lbTestName.Text := Test.Name;
      mmReport.Text := Test.Report;
      mmDetails.Text := Test.Details;
      //TODO: Adjust memo sizes
    end;
  Application.ProcessMessages;
end;

function TSelfTestF.ItemToTest(Item: TTreeViewItem): TTestable;
begin
  if Item.TagObject is TTestable then
    Result := Item.TagObject as TTestable
  else
    Result := nil;
end;

procedure TSelfTestF.RunItemTests(Item: TTreeViewItem);
var I: Integer;
begin
  try
    tvTests.BeginUpdate;
    if Item.IsChecked then
      if Item.TagObject is TTestGroup then
        (Item.TagObject as TTestable).Run;
      for I := 0 to Item.Count-1 do
        RunItemTests(Item.Items[I]);

    ItemShowResults(Item);
  finally
    tvTests.EndUpdate;
  end;
end;

procedure TSelfTestF.tvTestsChange(Sender: TObject);
begin
  if tvTests.Selected = nil then
  begin
    mmDetails.Text := '';
    mmReport.Text := '';
  end
  else
    ItemShowResults(tvTests.Selected);
end;

procedure TSelfTestF.tvTestsChangeCheck(Sender: TObject);
var Item: TTreeViewItem;
begin
  if FInUpdateChecks > 0 then
    EXIT;

  if Sender is TTreeViewItem then
  begin
    Item := Sender as TTreeViewItem;
    if not tvTests.IsUpdating then
      tvTests.Selected := Item;
    if Item.TagObject is TTestable then
      (Item.TagObject as TTestable).IsEnabled := Item.IsChecked;

    UpdateChecks(Item);
  end;
end;

procedure TSelfTestF.UpdateChecks(Item: TTreeViewItem);
var I: Integer;
begin
  try
    tvTests.BeginUpdate;
    inc(FInUpdateChecks);
    if Item.TagObject is TTestable then
      Item.IsChecked := (Item.TagObject as TTestable).IsEnabled;

    for I := 0 to Item.Count-1 do
        UpdateChecks(Item.Items[I]);
  finally
    tvTests.EndUpdate;
    dec(FInUpdateChecks);
  end;
end;

end.
