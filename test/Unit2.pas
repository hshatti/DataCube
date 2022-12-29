unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, StrUtils, Math, System.Variants, System.Classes, Vcl.Graphics
  ,Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, DateUtils
  ,Vcl.ComCtrls, Vcl.Grids
  , DCube
  {$ifdef Profiling} , hirestimer {$endif}
  ;

type

  TPivotGrid=class(TStringGrid)
    private
      FDataObject:PObjData;
      procedure setDataObject(AValue:PObjData);
    public
      procedure UpdateGrid;
            property DataObject:PObjData read FDataObject write setDataObject;
  end;


  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure UpdateCubeDimention(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    { Public declarations }
    procedure DrawGrid(grd:TStringGrid);
  end;

var
  Form1: TForm1;
  Grid1:TPivotGrid;
  DataObj:TObjData;
  Pivot: TPivotControl   ;

implementation


{$R *.dfm}

function GenerateRecord():TDataRecord;
  const Columns=6;
begin
  setLength(result,Columns);
  result[0]:=RandomFrom(['IBM','Google','Amazon','Microsoft','Facebook','Cisco','Oracle'])  ;
  result[1]:=RandomFrom(['Storage','Compute','Database','Processor','Data Science','Containers','Authentication'])  ;
  result[2]:=RandomFrom(['Private','Public','NGO']);
  result[3]:=random(100) ;   if Random>0.5 then VarClear(result[3]);

  result[4]:=50+random(200)/4 ;  if Random>0.5 then VarClear(result[4]);
  result[5]:=TDateTime(EncodeDate(randomrange(2000,2020),EnsureRange(round(RandG(6,2)),1,12),RandomRange(1,27)))

end;



procedure TForm1.DrawGrid(grd: TStringGrid);

begin

end;

procedure TForm1.FormCreate(Sender: TObject);
var i:integer;
begin
  DataObj.Headers.columns:=[[
  THeader.Create('Provider', htString),
  THeader.Create('Service', htString),
  THeader.Create('Sector',   htString),
  THeader.Create('Quantity', htString),
  THeader.Create('Price',    htString),
  THeader.Create('Date',     htString)
  ]];
  setLength(DataObj.Data,1000000);
  for i:=0 to High(DataObj.Data) do
    DataObj.Data[i]:=GenerateRecord  ;

  Pivot:=TPivotControl.Create(Self);
  Pivot.Parent:=Self;
  Grid1:=TPivotGrid.Create(Self);
  Pivot.OnUpdateCubeDimenstion:=UpdateCubeDimention;
  Grid1.Parent:=Pivot    ;
  Grid1.Options:=Grid1.Options+[TGridOption.goRangeSelect{,TGridOption.goEditing},TGridOption.goDrawFocusSelected];
  Grid1.Align:=alClient;
  Grid1.DoubleBuffered:=True;
  Pivot.DataObject:=@DataObj
end;

procedure TForm1.UpdateCubeDimention(Sender: TObject);
begin
  DataObj.Dimensions:=Pivot.Dimensions;
  Grid1.DataObject:=@Cube(DataObj)

end;





{ TPivotGrid }

procedure TPivotGrid.setDataObject(AValue: PObjData);
begin
  if AValue<>FDataObject then
    begin
      FDataObject:=AValue;

    end;
      UpdateGrid
end;

procedure TPivotGrid.UpdateGrid;
var ro,co,ro1,co1,ro2,co2,i,j,k,MaxColWidth:integer;d:PObjData;  s:string;
begin
  ro1:=0;co1:=0; ro2:=0;co2:=0;
  d:=FDataObject;
  inc(co1,length(d.Headers.rows));
  inc(ro1,length(d.Headers.columns));
  if ro1>0 then inc(co2,length(d.Headers.columns[ro1-1]));
  if co1>0 then inc(ro2,length(d.Headers.rows[co1-1]));
  ColCount:=co1+co2;
  RowCount:=ro1+ro2+ord(co1=0);
  for i:=0 to ColCount-1 do
   Cols[i].Clear;

  // fill column headers

  for ro:=0 to ro1-1 do begin
    co:=0;
    i:=co+co1;
    while co < length(d.Headers.columns[ro]) do begin
      cells[i,ro]:=StringReplace(d.Headers.columns[ro][co].Name,#$ff,'',[rfReplaceall]);
      inc(i,d.Headers.columns[ro][co].span);
      inc(co);
    end;
  end;

  // fill row headers
  for co:=0 to co1-1 do begin
    ro:=0;
    i:=ro+ro1;
    while ro < length(d.Headers.rows[co]) do begin
      cells[co,i]:=StringReplace(d.Headers.rows[co][ro].Name,#$ff,'',[rfReplaceall]);
      inc(i,d.Headers.rows[co][ro].span);
      inc(ro);
    end;
  end;

//
//  for co := 0 to co1-1 do
//   for ro:=0 to ro2-1 do
//      cells[co,ro1+ro]:=StringReplace(d.Headers.Rows[co][ro].Name,#$ff,'',[rfReplaceall]);

  // fill data
  with d^ do begin
    for ro := 0 to High(Data) do
      for co:=0 to High(Data[ro]) do
        Cells[co1+co,ro1+ro]:=Data[ro][co]
  end;

  // fit the StringGrid cells width
  for i:=0 to ColCount-1 do  begin
    for j:=0 to RowCount-1 do
      MaxColWidth:= Max(MaxColWidth, Canvas.TextWidth(Cells[i,j])+12);
    ColWidths[i]:=MaxColWidth
  end;

//  Form1.Memo1.lines.clear;
//  for i:=0 to High(d.results.Keys) do
//    Form1.Memo1.Lines.Add(format('[%s]=>[%s]',[d.results.Keys[i],d.results.Values[i].TableData[0]{$ifdef fpc}.Data{$endif}.ToString()]))

end;

end.
