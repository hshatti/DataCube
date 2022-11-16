unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, StrUtils, Math, System.Variants, System.Classes, Vcl.Graphics
  ,Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, DateUtils
  ,Vcl.ComCtrls, Vcl.Grids
  , DCube
  {$ifdef Profiling}, hirestimer{$endif}
  ;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    Grid: TTabSheet;
    Log: TTabSheet;
    Memo1: TMemo;
    Edit1: TEdit;
    BitBtn1: TBitBtn;
    Button1: TButton;
    grd: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure UpdateCubeDimention(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
      DataObj:TObjData;
    procedure DrawCube(const Cube: TObjData; Grid: TStringGrid);
    function Logs(const str:string):integer;                          //    overload;
//    function Logs(const fmt: string; vals: array of const): integer;      overload;      { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Pivot: TPivotControl   ;

implementation


{$R *.dfm}

function GenerateRecord():TDataRecord;
var i:integer;
  const Columns=6;
begin
  setLength(result,Columns);
  result[0]:=RandomFrom(['IBM','Google','Amazon','Microsoft','Facebook','Cisco','Oracle'])  ;
  result[1]:=RandomFrom(['Storage','Compute','Database','Processor','Data Science','Containers','Authentication'])  ;
  result[2]:=RandomFrom(['Private','Public','NGO']);
  result[3]:=random(100) ;
  result[4]:=50+random(200)/4 ;
  result[5]:=TDateTime(EncodeDate(randomrange(2000,2020),EnsureRange(round(RandG(6,2)),1,12),RandomRange(1,27)))

end;



procedure TForm1.BitBtn1Click(Sender: TObject);
//var ar:TTableData;a:TDataRecord; bb:array of boolean;
begin
//  bb:=[false,false];
//  ar:=[['a','a'],['a','b'],['b','c']];
//  a:=[100,300];
//  Memo1.Lines.Add(format('%d',[sizeof(ar)]));
////  Memo1.Lines.Add('%d',[arrayLookup<TDataRecord>(ar,a,bb,true)])

end;

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
  //DefaultFormatSettings.ShortDateFormat:='yyyy-mm-dd';
  setLength(DataObj.Data,1000000);
  DataObj.Headers.columns:=[[
    THeader.Create('Provider',htString),
    THeader.Create('Service',htString),
    THeader.Create('Sector',htString),
    THeader.Create('Quantity',htInteger),
    THeader.Create('Price',htDouble),
    THeader.Create('Date',htDateTime)
  ]];
  {$ifdef Profiling}Profiler.Start;{$endif}
  for i:=0 to high(DataObj.Data) do
    DataObj.Data[i]:=GenerateRecord;
  {$ifdef Profiling}Profiler.Log(format('Data Generated! with [%d] row',[Length(DataObj.Data)]));{$endif}
  //DataObj.SaveToFile(GetCurrentDir+'gentable.csv');
  Pivot.DataObject:=@DataObj;
 {$ifdef Profiling}Memo1.Lines.Text:=Profiler.LogStr; {$endif}

end;


function TForm1.Logs(const str: string): integer;
begin
  result:=Memo1.Lines.Add(str);
end;

procedure TForm1.UpdateCubeDimention(Sender: TObject);
begin
  DataObj.Dimensions:=Pivot.Dimensions;
  DrawCube(Cube(Pivot.DataObject^),grd)
end;

procedure TForm1.DrawCube(const Cube: TObjData; Grid: TStringGrid);
var i,j,k, MeasureCount:integer;ss:string;
begin
   {$ifdef Profiling}Memo1.Lines.Text:=Profiler.LogStr;{$endif}
  logs('HEADERS LIST:');
  for i:=0 to high(Cube.Headers.rows) do
    Logs(format('    Headers.Rows[%d]: %s',[i,StringReplace(Cube.Headers.rows[i].toString,#$FF,'',[rfReplaceAll])]));
  for i:=0 to high(Cube.Headers.Columns) do
    Logs(format('    Headers.Columns[%d]: %s',[i,StringReplace(Cube.Headers.Columns[i].toString,#$FF,'',[rfReplaceAll])]));
  logs('');
  logs('LIST:');
//  Logs('    Rows: '+StringReplace(Cube.rows.ToString(),#$FF,'',[rfReplaceAll] ));
//  Logs('    Cols: '+StringReplace(Cube.cols.ToString(),#$FF,'',[rfReplaceAll] ));
  logs('Result:');

  for ss in Cube.results.Keys do
    Logs(format('  %s => %s',[ss,Cube.results[ss].DataRecord.toString()]));




//  Grid.Clear;
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
//  Grid.AutoSizeColumns;
end;




end.
