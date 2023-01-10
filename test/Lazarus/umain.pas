unit uMain;

{$mode delphi}{$H+}

interface

uses
  Classes, StrUtils, SysUtils, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, Math, dcube, variants
  {$ifdef Profiling}, hirestimer{$endif} ;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    Edit1: TEdit;
    grd: TStringGrid;
    Memo1: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Pivot:TPivotControl;
    procedure Button1Click(Sender: TObject);
    procedure Edit1DblClick(Sender: TObject);
    procedure UpdateCubeDimention(Sender:TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    DataObj:TObjData;
    procedure DrawCube(const Cube:TObjData;Grid:TStringGrid);
    function Log(const str:string):integer;                              overload;
    function Log(const fmt: string; vals: array of const): integer;      overload;
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

function GenerateRecord():TDataRecord;
var i:integer;
  const Columns=6;
begin
  setLength(result,Columns);
  result[0]:=RandomFrom(['IBM','Google','Amazon','Microsoft','Facebook','Cisco','Oracle'])  ;
  result[1]:=RandomFrom(['Storage','Compute','Database','Processor','Data Science','Containers','Authentication'])  ;
  result[2]:=RandomFrom(['Private','Public','NGO','']);
  result[3]:=1+random(99)      ; if result[3]>50 then VarClear(result[3]);
  result[4]:=50+random(200)/4 ; if result[4]>80 then VarClear(result[4]);
  result[5]:=TDateTime(EncodeDate(randomrange(2000,2020),EnsureRange(round(RandG(6,2)),1,12),RandomRange(1,27)))

end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var i:integer;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  Pivot:=TPivotControl.Create(Self);
  Pivot.OnUpdateCubeDimenstion:=UpdateCubeDimention;
  Pivot.Parent:=Self;
  Pivot.SendToBack;
  PageControl1.Parent:=Pivot;
  PageControl1.Align:=alClient;
  DefaultFormatSettings.ShortDateFormat:='yyyy-mm-dd';
  setLength(DataObj.Data,100000);
  DataObj.Headers.columns:=[[
    THeader.Create('Provider',varString),
    THeader.Create('Service',varString),
    THeader.Create('Sector',varString),
    THeader.Create('Quantity',varInteger),
    THeader.Create('Price',varDouble),
    THeader.Create('Date',varDate)
  ]];
  {$ifdef Profiling}Profiler.Start;{$endif}
  for i:=0 to high(DataObj.Data) do
    DataObj.Data[i]:=GenerateRecord;
  {$ifdef Profiling}Profiler.Log(format('Data Generated! with [%d] row',[Length(DataObj.Data)]));{$endif}
  //Dataobj.Data:=TServiceTools.TransformData(DataObj.Data);
  //{$ifdef Profiling}Profiler.Log(format('Data Transform! with [%d] row',[Length(DataObj.Data)]));{$endif}
  //DataObj.SaveToFile(GetCurrentDir+'gentable.csv');
  Pivot.DataObject:=@DataObj;
 {$ifdef Profiling}Memo1.Lines.Text:=Profiler.LogStr;
 Button1.Visible:=true;
 Edit1.Visible:=true

 {$else}
 TabSheet2.TabVisible:=false;
 PageControl1.ActivePageIndex:=0;
 {$endif}

end;

procedure TForm1.DrawCube(const Cube: TObjData; Grid: TStringGrid);
var i,j,k, MeasureCount:integer; ss:string;
begin
  //{$ifdef Profiling}Memo1.Lines.Text:=Profiler.LogStr;{$endif}
  //log('HEADERS LIST:');
  //for i:=0 to high(Cube.Headers.rows) do
  //  Log(format('    Headers.Rows[%d]: %s',[i,StringReplace(Cube.Headers.rows[i].toString,#$FF,'',[rfReplaceAll])]));
  //for i:=0 to high(Cube.Headers.Columns) do
  //  Log(format('    Headers.Columns[%d]: %s',[i,StringReplace(Cube.Headers.Columns[i].toString,#$FF,'',[rfReplaceAll])]));
  //log('');
  //log('LIST:');
  //Log('    Rows: '+StringReplace(Cube.rows.ToString(),#$FF,'',[rfReplaceAll] ));
  //Log('    Cols: '+StringReplace(Cube.cols.ToString(),#$FF,'',[rfReplaceAll] ));
  //log('Result:');

  //for ss in Cube.results.Keys do
  //  Log('  %s => %s',[ss,Cube.results[ss].DataRecord.toString()]);



  //exit;

  Grid.Clear;
  MeasureCount:=length(Cube.Dimensions.Measures);
  Grid.ColCount:=length(Cube.Dimensions.Rows)+Length(Cube.Headers.Columns[High(Cube.Headers.Columns)])+ord(not (assigned(Cube.Dimensions.Rows) or assigned(Cube.Dimensions.Cols)));

  if assigned(Cube.Headers.rows) then
    Grid.RowCount:=length(Cube.Dimensions.cols)+length(Cube.Headers.rows[High(Cube.Headers.rows)])+1
  else
    Grid.RowCount:=length(Cube.Dimensions.cols)+2;// make +1 row space for measure headers
  Grid.FixedCols:=length(Cube.Headers.rows);
  Grid.FixedRows:=length(Cube.Headers.columns);
  Application.ProcessMessages;

  for i:=0 to High(Cube.Headers.Columns) do begin
    k:=0;
    for j:=0 to High(Cube.Headers.Columns[i]) do begin
      Grid.Cells[k+length(Cube.Headers.rows),i]:=Cube.Headers.Columns[i][j].Name.Replace(#$ff,'',[rfReplaceAll]);
      inc(k,Cube.Headers.Columns[i][j].span) ;
    end;
  end;

  for i:=0 to high(Cube.headers.Rows) do
    for j:=0 to High(Cube.headers.Rows[i]) do begin
      Grid.Cells[i,j+length(Cube.Headers.columns)]:=Cube.Headers.Rows[i][j].Name.Replace(#$ff,'',[rfReplaceAll]);
    end;

  for i:=0 to High(Cube.Data) do
    for j:=0 to High(Cube.Data[i]) do
      Grid.Cells[j+length(Cube.Headers.rows),i+length(Cube.Headers.columns)]:=Cube.Data[i][j];
  Grid.AutoSizeColumns;
end;

function TForm1.Log(const str: string): integer;
begin
  result:=Memo1.Lines.Add(str);
end;

function TForm1.Log(const fmt: string; vals: array of const): integer;
begin
  result:=Memo1.Lines.add(fmt, vals)
end;

procedure TForm1.UpdateCubeDimention(Sender: TObject);

var
  dObj: TObjData;
begin
  DataObj.Dimensions:=Pivot.Dimensions;
  //defaultTotalsOption.subCols:=false;
  //defaultTotalsOption.subRows:=false;
  //defaultTotalsOption.grandCols:=false;
  //defaultTotalsOption.grandRows:=false;
  //dObj:=cube(DataObj);
  dObj:=DataObj.cube();
  {$ifdef Profiling}
  Profiler.Stop;
  Log(Profiler.LogStr);
  Application.ProcessMessages;
  Profiler.Start;

  {$endif}

  DrawCube(dObj,grd);
  {$ifdef Profiling}
  Log('Cols :'+sLineBreak+dObj.Cols.ToString);
  Log('Rows :'+sLineBreak+dObj.Rows.ToString);

  Profiler.Log('Pivot Grid Draw...');
  Log(Profiler.LogStr);
  Application.ProcessMessages;
  Profiler.Stop
  {$endif}


  //
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i:integer;
  ar,ab, transposed:TTableData;
  arr,abb:TVariantArray;
  ColsIds:array of integer;
  function InitWord:integer;
  var i:integer;
  begin
//    setLength(result,RandomRange(3,4));
    result:=randomrange(1,100);

  end;

begin

  {$ifdef Profiling}Profiler.start; {$endif}
  Transposed := TServiceTools.TransposeData(DataObj.data);
  {$ifdef Profiling}Profiler.Log('Data Transposed');   {$endif}
  ar:=TServiceTools.SparseValues<TVariantArray>(transposed,[0,1]);
  {$ifdef Profiling}Profiler.Log('Data Sliced');    {$endif}
  ab:=TServiceTools.TransposeData(Ar);
  {$ifdef Profiling}Profiler.Log('Data2 Transposed');   {$endif}
  TServiceTools.arraySort(ab,0,High(ab),[true,false]);
  {$ifdef Profiling}Profiler.Log('Data Sorted');      {$endif}
  ab:=TServiceTools.arrayUnique(ab);
  {$ifdef Profiling}Profiler.Log('Data Uniqued');   {$endif}
  Log(ab.ToString);
  {$ifdef Profiling}Log(Profiler.LogStr);       {$endif}
  exit;

  setLength(arr,10000000);
  for i:=0 to high(arr) do arr[i]:=initWord ;
  ShowMessage('Will sort now, hang on...');
  TVariantArray(arr).Sort;
  ShowMessage('Will Unique now, hang on...');
  abb:=TVariantArray(arr).Unique;
  Log(TVariantArray(abb).toString) ;


 // Log( Eval(Edit1.Text))
end;

procedure TForm1.Edit1DblClick(Sender: TObject);
var v,w:variant;
begin
  w:=15.0 ;
  Edit1.Text:=floattostr(double(v));
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
var ar:TTableData;a:TDataRecord; bb:array of boolean;
begin
  bb:=[false,false];
  ar:=[['a','a'],['a','b'],['b','c']];
  a:=[100,300];
  Memo1.Lines.Add('%d',[sizeof(ar)]);
//  Memo1.Lines.Add('%d',[arrayLookup<TDataRecord>(ar,a,bb,true)])

end;

end.

