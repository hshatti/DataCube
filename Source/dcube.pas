unit dcube;

{$mode delphi} {$ModeSwitch advancedrecords} {$Macro on} {$ModeSwitch typehelpers}


interface

uses
   {$ifdef fpc}LCLType, {$endif}
   Types, Classes, SysUtils, ArrayHelper, ArrayHelperCommon, variants, db, hirestimer, Controls, StdCtrls, Graphics
  //,fpjson,extjsjson
  ;


const   varsOrdinal=[varShortInt,varSmallInt,varInteger,varInt64,varByte,varWord,varLongWord,varQWord];
        varsFloats=[varSingle,varDouble,varCurrency,varDecimal, varDate];
        //varsStrings= [varString, varOleStr, varUString]
        varsNumeric=varsOrdinal + varsFloats;

        HeaderTypeStr:array[0..12] of string= ('Unknowen', 'String', 'Boolean', 'Byte', 'Word', 'Integer', 'Int64', 'Single', 'Double', 'Currency', 'DateTime', 'SingleComplex', 'DoubleComplex');

type


  THeaderType=( htUnknowen, htString, htBoolean, htByte, htWord, htInteger, htInt64, htSingle, htDouble, htCurrency, htDateTime, htSingleComplex, htDoubleComplex);

  THeader=record
    Name:string;
    size:integer;
    typ:THeaderType ;
    span:integer;
    constructor Create(aName:string;const aTyp:THeaderType;const aSpan:integer=1;const aSize:integer=10);
  end;

  THeaders=array of THeader;

  { THeadersHelper }

  THeadersHelper=type helper for THeaders
  public
    function toString():string;
  end;

  TResultHeaders=record
    columns,rows:Array of THeaders

  end;

  TMeasureFunction=TReduceCallback<variant>;//function(const a,b:Variant):Variant;

  TAggregation = record
    Name :string;
//    typ: THeaderType;  // result;
    operation:TMeasureFunction
  end;

  { TMeasure }

  TMeasure=record
    _Label, Field, Formula:string;
    typ:THeaderType;
    decimals:word;
    operation:TAggregation;
    constructor Create(const aField: string; const aOperation: TAggregation;
      const aLabel: string=''; const aTyp: THeaderType=htUnknowen; const aDecimals: shortint=2);

  end;



  TDataRecord = TVariantArray;
  TTableData=TVariantArrayArray;

  TRecordStack=record
    i:integer;
    v:TDataRecord;
  end;

  TDimensions=record
    Cols,Rows:TStringDynArray;
    Measures:array of TMeasure
  end;

  TFilter=record
    fieldName:string;
    data:TStringDynArray
  end;

  TFilters=array of TFilter;

  { TTableOptions }

  TTotalsOptions=record
    public
    bc,br,          //Show totals: br = totals befor rows , bc = totals before columns
    grandCols,
    grandRows,
    subCols,
    subRows:boolean;
    class operator initialize(var dest:TTotalsOptions); overload;
  end;

const defaultTotalsOption:TTotalsOptions = (bc:false; br:false; grandCols:true; grandRows:true; subCols:true; subRows:true);

type

  { TResults }
  TResults=record
    TableData:TTableData;
    DataRecord:TDataRecord;
    constructor Create(const RowCount:integer);
  end;

  {$define KeyValueList}
  {$ifdef KeyValueList}
  TResultData=TKeyValueList<string,TResults>;
  {$else}
  { TResultData }

  TResultData=record
  private
    function GetValues(const key: string): TResults;
    procedure SetValues(const key: string; const AValue: TResults);
  public
    Keys:TStringDynArray;
    ValueList:array of TResults;
    function KeyExists(key: string): boolean;
    property Values[key:string]:TResults read GetValues write SetValues ;default;
    function Count:integer;
    procedure Remove(const index:integer);
  end;
  {$endif}

  { TObjData }

  TObjData=record
    Headers:TResultHeaders;
    Data:TTableData;
    Dimensions:TDimensions;
    filters:TFilters;
    descends:TStringDynArray;
    Cols,Rows:TTableData;
    colsHeadSubTotals, colsHeadGrandTotals,rowsHeadSubTotals, rowsHeadGrandTotals:TIntegerDynArray;
    results:TResultData;
    procedure LoadFromFile(const FileName:TFileName);
  end;

  { TDimensionPan }

  TDimensionPan=class(TCustomGroupBox)
    FSpace: TLabel;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    constructor Create(AOwner: TComponent); override;
  end;


  { TDimensionButton }

  TDimensionButton=class(TCustomControl)
  type

    { TDimDragControlObject }

    TDimDragControlObject=class(TDragControlObject)
      FDragImages: TDragImageList;
      constructor Create(AControl: TControl); override;
      function GetDragImages: TDragImageList; override;
    end;
  private
    FBackgroundColor: TColor;
    FFieldName: string;
    FPng      : TBitmap;
    FDragControlObject:TDimDragControlObject;
    FMeasures:TComboBox;
    procedure SetBackgroundColor(AValue: TColor);
  protected
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure SetFieldName(AValue: string);
    procedure TextChanged; override;
  published
    procedure DoOnResize; override;
    constructor Create(AOwner: TComponent); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
    procedure Paint ;override;
    property FieldName:string read FFieldName write SetFieldName;
    property BackgroundColor:TColor read FBackgroundColor write SetBackgroundColor;
    procedure DoStartDrag( var DragObject: TDragObject); override;

  end;


  { TPivotControl }

  TPivotControl=class(TWinControl)
    const panSpacing=8 ;
    private
      FDataObject: TObjData;
      //FDimensions:TDimensions;
      FFieldsPan,FColumnsPan,FRowsPan,FMeasuresPan:TDimensionPan;
      FOnUpdateCubeDimenstion: TNotifyEvent;
      FButtons:array of TDimensionButton;
      procedure UpdateMeasures(Sender:TObject);
      procedure SetDataObject(const AValue: TObjData);
//      procedure SetDimentions(const AValue: TDimensions);
      procedure SetOnUpdateCubeDimenstion(AValue: TNotifyEvent);
      procedure updateAvailableDimensions; virtual ;
      procedure updateCubeDimensions; virtual;
    public
      Dimensions: TDimensions;
      constructor Create(AOwner: TComponent); override;
//      property Dimensions:TDimensions read FDimentions write SetDimentions;
      property DataObject:TObjData read FDataObject write SetDataObject;
      property OnUpdateCubeDimenstion:TNotifyEvent read FOnUpdateCubeDimenstion write SetOnUpdateCubeDimenstion;


  end;

  function min<T>(const a,b:T):T;
  function max<T>(const a,b:T):T;
  //function _sum    (const a, b: Variant; const i: integer; const Arr: array of Variant):Variant;
  //function _max    (const a, b: Variant; const i: integer; const Arr: array of Variant):Variant;
  //function _min    (const a, b: Variant; const i: integer; const Arr: array of Variant):Variant;
  //function _first  (const a, b: Variant; const i: integer; const Arr: array of Variant):Variant;
  //function _last   (const a, b: Variant; const i: integer; const Arr: array of Variant):Variant;
  //function _count  (const a, b: Variant; const i: integer; const Arr: array of Variant):Variant;
  //function _average(const a, b: Variant; const i: integer; const Arr: array of Variant):Variant;
  //function _median(const a, b: Variant; const i: integer; const Arr: array of Variant):Variant;
  //function _mode(const a, b: Variant; const i: integer; const Arr: array of Variant):Variant;
  //function _stddev(const a, b: Variant; const i: integer; const Arr: array of Variant):Variant;


  function cube(const obj:TObjData;const options:TTotalsOptions):TObjData; overload;
  function cube(const obj:TObjData):TObjData; overload;

  function arrayComp(const a,b:Variant;const rev:boolean;const p:boolean):TValueRelationship;                   overload;
  function arrayComp(const a,b:TVariantArray;const rev:boolean;const p:boolean):TValueRelationship;             overload;
  function arrayComp(const a,b:TVariantArray;const rev:array of boolean;const p:boolean):TValueRelationship;    overload;

  function arrayLookup<T>(const arr:array of T;const e:T;const reverse:array of boolean;const rp:boolean):integer;   overload;
  function arrayLookup<T>(const arr:array of T;const e:T;const reverse:boolean=false;const rp:boolean=false):integer;   overload;//function comp<T>(const a,b:T):integer;                              overload;
  function Eval(str:string):double;
  //function comp<T>(const a,b:T;const rev:boolean):integer;            overload;
  //function aComp<T>(const a,b:T;const rev:boolean):integer;           overload;
  //
  //function Lookup<T>(const Ar:array of T;const e:T;const reverse:boolean =false ;const p:boolean=false):integer;   overload;
  //function Lookup<T>(const Ar:array of T;const e:T;const reverse:array of boolean;const p:boolean=false):integer;   overload;
//  TIfthen = function (val:boolean;const iftrue:integer; const iffalse:integer) :integer ;
//var
//  ifthen :TIfthen;
var
  AggregationArray:array of TAggregation;

implementation

const splitter=#9;
      arrayCompFiller=#$ff+'Total';

      defaultButtonHeight=40;
      defaultButtonWidth =200;


const oprChars=['*','/','+','-'];

function EvalStr( str:string):double;
var i,start:integer; len:integer=0;
    oprs:array{[0..127]} of string;
    nums:array{[0..127]} of double;

begin
  str:=trim(str);
  start:=1;
  for i:=1 to Length(str) do
    if str[i] in oprChars then begin
      //nums[len]:= StrToFloat(trim(copy(str,start,i-start)));
      Insert(StrToFloat(trim(copy(str,start,i-start))),nums,length(nums));
      //oprs[len]:= str[i];
      insert(str[i],oprs,length(oprs));
      //inc(len);
      start:=i+1
    end;
  //nums[len]:=strToFloat(Trim(copy(str,start,length(str)+1-start)));
  insert(strToFloat(Trim(copy(str,start,length(str)+1-start))),nums,length(nums));

  i:=0;
  while i<length(oprs) do begin     // BOADMAS prioritises divisions
  //while i<len do begin     // BOADMAS prioritises divisions
    if oprs[i]='/' then begin
      nums[i]:=nums[i]/nums[i+1];
      delete(oprs,i,1);
      delete(nums,i+1,1)
      //dec(len)
    end else inc(i);
  end;

  i:=0;
  while i<length(oprs) do begin     // then Multiplication
  //while i<len do begin     // then Multiplication
    if oprs[i]='*' then begin
      nums[i]:=nums[i]*nums[i+1];
      delete(oprs,i,1);
      delete(nums,i+1,1)
      //dec(len)
    end else inc(i);
  end;

  i:=0;
  while i<length(oprs) do begin    // then add and subtract
  //while i<len do begin     // then Multiplication
    if oprs[i]='+' then begin
      nums[i]:=nums[i]+nums[i+1];
      delete(oprs,i,1);
      delete(nums,i+1,1)
      //dec(len)
    end else if oprs[i]='-' then begin
      nums[i]:=nums[i]-nums[i+1];
      delete(oprs,i,1);
      delete(nums,i+1,1)
      //dec(len)
    end else  inc(i);
  end;
  result:=nums[0]


end;

function Eval(str:string):double;
  type

    TEnclosure=record
    start,finish:integer;
  end;

  var
    i,j,k:integer; inQuoteLvl:boolean=false;

    Enclosure:TEnclosure;
    Enclosures:array of TEnclosure;
    r:string;
begin
  i:=1;
  while i<=Length(str) do begin
    if str[i]='''' then
      inQuoteLvl:=not(inQuoteLvl);
    if inQuoteLvl then continue;
    if str[i] ='(' then begin
      Enclosure.start:=i;
      Insert(Enclosure, Enclosures, length(Enclosures))    ;
      inc(i);
      continue
    end;
    if str[i] =')' then begin
      Enclosures[high(Enclosures)].finish:=i;
      j:=Enclosures[high(Enclosures)].finish-Enclosures[high(Enclosures)].start-1 ;  // determine parenthesis boendry
      r:=EvalStr(copy(str, Enclosures[high(Enclosures)].start+1,j )) ; // evaluate whats between the parenthesis
      delete(str,Enclosures[high(Enclosures)].start,j+2); // delete the parenthesis with the content
      insert(r,str,Enclosures[high(Enclosures)].start);   // insert the parenthesis evaluation
      i:=Enclosures[high(Enclosures)].start+length(r) ;   //set i to at the end of the evaluation
      delete(Enclosures,high(Enclosures),1);
      continue
    end;
    inc(i)
  end  ;
  Result:=EvalStr(str);
end;


function StringsToDataRec(const str:TStringDynArray):TDataRecord;
var i:integer;vInt:integer;vDouble:double;vBool:boolean;vDateTime:TDateTime;
begin
  setLength(result,length(str));
  for i:=0 to High(str) do
    if TryStrToInt(str[i],vInt) then result[i]:=vInt else
    if TryStrToFloat(str[i],vDouble) then result[i]:=vDouble else
    if TryStrToBool(str[i],vBool) then result[i]:=vBool else
    if TryStrToDateTime(str[i],vDateTime) then result[i]:=vDateTime else
    result[i]:=str[i]

end;

{ TDimensionButton }

const defaultBtnTextStyle:TTextStyle=
   (Alignment : taLeftJustify;
    Layout    : tlCenter;
    //SingleLine: boolean;
    //Clipping  : boolean;
    //ExpandTabs: boolean;
    //ShowPrefix: boolean;
    //Wordbreak : boolean;
    //Opaque    : boolean;
    //SystemFont: Boolean;
    //RightToLeft: Boolean;
    //EndEllipsis: Boolean
    ) ;

{ TDimensionButton.TDimDragControlObject }

constructor TDimensionButton.TDimDragControlObject.Create(AControl: TControl);
begin
  inherited Create(AControl);
  FDragImages := TDragImageList.Create(AControl);
  AlwaysShowDragImages:=true;
end;

function TDimensionButton.TDimDragControlObject.GetDragImages: TDragImageList;
begin
  Result:=FDragImages;
end;

{ TDimensionPan }

procedure TDimensionPan.DragDrop(Source: TObject; X, Y: Integer);
begin
  inherited ;
  if (Source is TDimensionButton) then begin
    if Caption='Rows' then begin
      TDimensionButton(Source).FMeasures.Hide;
      TDimensionButton(Source).Align:=alTop ;
      TDimensionButton(Source).Top:=y;
      TDimensionButton(Source).Height:=defaultButtonHeight;
    end
    else if Caption='Measures' then begin
      TDimensionButton(Source).FMeasures.Show;
      TDimensionButton(Source).FMeasures.OnChange:=TPivotControl(Self.parent).UpdateMeasures;
      TDimensionButton(Source).Left:=X;
      TDimensionButton(Source).Width:=defaultButtonWidth;
      TDimensionButton(Source).Align:=alLeft;
    end else  begin
      TDimensionButton(Source).FMeasures.Hide;
      TDimensionButton(Source).Left:=X;
      TDimensionButton(Source).Width:=defaultButtonWidth;
      TDimensionButton(Source).Align:=alLeft;
    end;
//    FSpace.Hide ;
    TDimensionButton(Source).Parent:=Self;
    SetControlIndex(TDimensionButton(Source),ControlCount-1);
    TPivotControl(Parent).updateCubeDimensions;
  end;

  //if (TDragControlObject(Source).Control is TDimensionButton) then TDragControlObject(Source).Control.Parent:=Self
end;

procedure TDimensionPan.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);
  Accept:=(Source is TDimensionButton);
  //Accept:=Accept or (TDragControlObject(Source).Control is TDimensionButton);
  if Accept then begin
//    FSpace.Show;

    //TControl(Source).Align:=alNone;
    //FSpace.Left:=X
  end;

end;

constructor TDimensionPan.Create(AOwner: TComponent);
begin
  inherited ;
  Height:=64;
  FSpace:=TLabel.Create(Self);
  FSpace.AutoSize:=true;
  FSpace.Caption:='...';
  FSpace.Align:=alLeft;
  FSpace.Visible:=False;
  FSpace.Parent:=Self;
end;

procedure TDimensionButton.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor=AValue then Exit;
  FBackgroundColor:=AValue;
  Invalidate;
end;

procedure TDimensionButton.DragDrop(Source: TObject; X, Y: Integer);
begin
  inherited;
  if (Source is TDimensionButton) then begin
    if Parent.Caption='Rows' then begin
      TDimensionButton(Source).FMeasures.Hide;
      TControl(Source).Top:=Parent.Top+Y;
      TControl(Source).Align:=alTop ;
      TDimensionButton(Source).Height:=defaultButtonHeight;
    end
    else if Parent.Caption='Measures' then begin
      TDimensionButton(Source).FMeasures.Show;
      TDimensionButton(Source).FMeasures.OnChange:=TPivotControl(Self.Parent.Parent).UpdateMeasures;
      TControl(Source).Left:=Parent.Left+X;
      TControl(Source).Align:=alLeft;
      TDimensionButton(Source).Width:=defaultButtonWidth;

    end else  begin
    TDimensionButton(Source).FMeasures.Hide;
      TControl(Source).Left:=Parent.Left+X;
      TControl(Source).Align:=alLeft;
      TDimensionButton(Source).Width:=defaultButtonWidth;
    end;
    TControl(Source).Parent:=Parent;
   // Parent.SetControlIndex(TDimensionButton(Source), Parent.ControlCount-1);
    TPivotControl(Parent.Parent).updateCubeDimensions;
//    FSpace.Hide ;
  end;

  //if (TDragControlObject(Source).Control is TDimensionButton) then TDragControlObject(Source).Control.Parent:=Self
end;

procedure TDimensionButton.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);
  if Source=Self then Exit;
  Accept:=Source is TDimensionButton;
  if Accept then  begin
//    TDimensionPan(Parent).FSpace.Show;
    //TDimensionPan(Parent).Left:=Left+Width
  end;
end;

procedure TDimensionButton.SetFieldName(AValue: string);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
  if Caption='' then Caption:=FFieldName;
end;

procedure TDimensionButton.TextChanged;
var sz:TSize;
begin
  inherited ;
  if AutoSize then  begin
    Sz:= Canvas.TextExtent(Text);
    SetBounds(Self.left,Self.top,sz.Width+8, sz.Height+8);
  end;

end;

procedure TDimensionButton.DoOnResize;
begin
   inherited ;
   FPng.SetSize(Width,Height);
end;

constructor TDimensionButton.Create(AOwner: TComponent);
var i:integer;
begin
  inherited ;
  FBackGroundColor:=clSilver;
  ChildSizing.SetGridSpacing(4);
  FMeasures:=TComboBox.Create(Self);
  FMeasures.BorderStyle:=bsNone;
  FMeasures.Parent:=Self;
  FMeasures.Hide;
  for i:=0 to High(AggregationArray) do
    FMeasures.Items.Add(AggregationArray[i].Name);
  FMeasures.ItemIndex:=0;
  FMeasures.Align:=alRight;
  FMeasures.Width:=defaultButtonWidth-80;
  FMeasures.top:=8;
  Parent:=TWinControl(AOwner);
  Width := defaultButtonWidth;
  //Color:=clGray;//$ff8800;
//  AutoSize:=True;
  DragKind:=dkDrag;
  DragMode:=dmAutomatic;
  DragCursor:=crDrag;
  Cursor:=crSizeAll;
  FPng:=TBitmap.Create;
  FPng.Canvas.TextStyle:=defaultBtnTextStyle;

  //FDragControlObject:=TDimDragControlObject.Create(Self);

end;

procedure TDimensionButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited ;
  if CanFocus then SetFocus;
end;

procedure TDimensionButton.Click;
begin
  inherited Click;
end;

procedure TDimensionButton.Paint;
var FocusRect:TRect;
begin
  FocusRect:=ClientRect;
  FocusRect.Inflate(-2,-2);
  with FPng do begin
    Transparent:=True;
    TransparentColor:=crDefault;
    Canvas.Erase;
    Canvas.Pen.Style:=psClear;
    Canvas.Brush.Style:=bsSolid;
    Canvas.Brush.Color:=FBackgroundColor;
    Canvas.RoundRect(ClientRect, ClientRect.Height shr 2  {div 4}, ClientRect.Height shr 2 {div 4});

    Canvas.Pen.Style:=psDot;
    Canvas.Brush.Style:=bsClear;
    Canvas.Pen.Color:=clWhite;
    if Focused then
      Canvas.RoundRect( FocusRect,1,1);//Brush.Color:=clBlue;

    Canvas.Font.Color:=$404040;
    Canvas.TextRect(FocusRect,4,0, Caption);
  end;
  Canvas.Draw(0,0,FPng);

  inherited ;
end;


procedure TDimensionButton.DoStartDrag(var DragObject: TDragObject);
begin
  inherited ;
  DragObject:=FDragControlObject;
  //FDragControlObject.FDragImages.Width:=Width;
  //FDragControlObject.FDragImages.Height:=Height;
  //FDragControlObject.FDragImages.Add(FPng,nil);
  //FDragControlObject.FDragImages.DragHotspot:=Point(Width,Height);
end;

{ TPivotControl }

//procedure TPivotControl.SetDimentions(const AValue: TDimensions);
//begin
//  //if FDimentions=AValue then Exit;
//  FDimentions:=AValue;
//end;

procedure TPivotControl.SetOnUpdateCubeDimenstion(AValue: TNotifyEvent);
var i:integer;
begin
  //if FOnUpdateCubeDimenstion=AValue then Exit;
  FOnUpdateCubeDimenstion:=AValue;

end;

procedure TPivotControl.UpdateMeasures(Sender: TObject);
begin
  updateCubeDimensions;
end;

procedure TPivotControl.SetDataObject(const AValue: TObjData);
begin
  //if FDataObject=AValue then Exit;
  FDataObject:=AValue;
  updateAvailableDimensions;
end;

procedure TPivotControl.updateAvailableDimensions;
var i:SizeInt;
begin
  // assume Data is flat and the 1st row is the column header
  if not assigned(FDataObject.Headers.columns) then exit;
  for i:=0 to high(FDataObject.Headers.columns[0]) do
    begin
      Insert(TDimensionButton.Create(Self),FButtons,length(FButtons));
      FButtons[High(FButtons)].FieldName:=FDataObject.Headers.columns[0][i].Name;
      FButtons[High(FButtons)].Align:=alLeft;
      FButtons[High(FButtons)].Parent:=FFieldsPan;
    end;


end;

procedure TPivotControl.updateCubeDimensions;
var i,j:integer;
begin
  setLength(Dimensions.Cols,FColumnsPan.ControlCount-1); j:=0;
  for i:=0 to FColumnsPan.ControlCount-1 do if FColumnsPan.Controls[i] is TDimensionButton then begin
    Dimensions.Cols[j]:=TDimensionButton(FColumnsPan.Controls[i]).FieldName;
    inc(j)
  end;

  setLength(Dimensions.Rows,FRowsPan.ControlCount-1); j:=0;
  for i:=0 to FRowsPan.ControlCount-1 do if FRowsPan.Controls[i] is TDimensionButton then begin
    Dimensions.Rows[j]:=TDimensionButton(FRowsPan.Controls[i]).FieldName;
    inc(j)
  end;

  setLength(Dimensions.Measures,FMeasuresPan.ControlCount-1);  j:=0;
  for i:=0 to FMeasuresPan.ControlCount-1 do if FMeasuresPan.Controls[i] is TDimensionButton then begin
    Dimensions.Measures[j]:=TMeasure.Create(
      TDimensionButton(FMeasuresPan.Controls[i]).FieldName
      ,AggregationArray[TDimensionButton(FMeasuresPan.Controls[i]).FMeasures.ItemIndex]
      //,TDimensionButton(FMeasuresPan.Controls[i]).Caption
    )  ;
    inc(j)
  end;
  if assigned(FOnUpdateCubeDimenstion) then
    FOnUpdateCubeDimenstion(Self)

end;

constructor TPivotControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align:=alClient;

  FColumnsPan      :=TDimensionPan.Create(AOwner);
  FColumnsPan.Caption:='Columns';
  FColumnsPan.Align:=alTop;
  FColumnsPan.ChildSizing.SetGridSpacing(panSpacing);
  FColumnsPan.Parent:=Self;

  FMeasuresPan:=TDimensionPan.Create(AOwner);
  FMeasuresPan.Caption:='Measures';
  FMeasuresPan.Align:=alTop;
  FMeasuresPan.ChildSizing.SetGridSpacing(panSpacing);
  FMeasuresPan.Parent:=Self;

  FFieldsPan      :=TDimensionPan.Create(AOwner);
  FFieldsPan.Caption:='Available Fields';
  FFieldsPan.Align:=alTop;
  FFieldsPan.ChildSizing.SetGridSpacing(panSpacing);
  FFieldsPan.Parent:=Self;

  FRowsPan   :=TDimensionPan.Create(AOwner);
  FRowsPan.Caption:='Rows';
  FRowsPan.Align   :=alLeft;
  FRowsPan.ChildSizing.SetGridSpacing(panSpacing);
  FRowsPan.Parent:=Self;


end;

{ THeadersHelper }

function THeadersHelper.toString(): string;
var i:integer;
begin
  result:='';
  for i:=0 to High(Self) do
    result:=result+format('(Name: %s, Span: %d, size: %d, type:%s), ',[Self[i].Name, Self[i].Span, self[i].size, HeaderTypeStr[ord(Self[i].typ)]])+LineEnding;

  result:='['+result+']'
end;

{ TResults }

constructor TResults.Create(const RowCount:integer);
begin
  setLength(TableData,RowCount);
  setLength(DataRecord,RowCount);
  //
end;

{ TMeasure }

constructor TMeasure.Create(const aField: string; const aOperation: TAggregation;
  const aLabel: string; const aTyp: THeaderType; const aDecimals: shortint);
begin
  Field:=aField;
  Operation:=aOperation;
  _label:=aLabel;
  typ:=aTyp;
  decimals:=aDecimals
end;

{ TObjData }

procedure TObjData.LoadFromFile(const FileName: TFileName);
var sl:TStringList;i:Integer;
begin
  sl:=TStringList.Create;
  sl.LoadFromFile(FileName);
  setLength(Data,SL.Count-1);
  if LowerCase(ExtractFileExt(FileName))='.csv' then
    for i:=1 to sl.Count-1 do
      Data[i]:=StringsToDataRec(sl[i].Split(',','"'))
  else if LowerCase(ExtractFileExt(FileName))='.tab' then
    for i:=1 to sl.Count-1 do
      Data[i]:=StringsToDataRec(sl[i].Split(#9));
  FreeAndNil(sl);
end;


constructor THeader.Create(aName:string;const aTyp:THeaderType;const aSpan:integer;const aSize:integer);
begin
  Name:=aName;typ:=aTyp;Size:=aSize;span:=aSpan
end;

{ TTableOptions }

class operator TTotalsOptions.initialize(var dest: TTotalsOptions);
begin
  dest.grandCols:=true;
  dest.grandRows:=true;
  dest.subCols:=true;
  dest.subRows:=true;
end;

{$ifdef KeyValueList}
{$else}
{ TResultData }

function TResultData.Count: integer;
begin
  result:=Length(Keys)
end;

procedure TResultData.Remove(const index: integer);
begin
  if (index>=0) and (index<Count) then begin
    Delete(Keys,index,1);
    Delete(ValueList,index,1);
  end;
end;

function TResultData.GetValues(const key: string): TResults;
var i:integer;
begin
  i:=_BinSearch<string,TStringDynArray>(Keys,key,Length(Keys));
  if i>=0 then
    result:=ValueList[i]
end;

procedure TResultData.SetValues(const key: string;
  const AValue: TResults);
var i:integer;
begin
  i:=_BinSearch<string,TStringDynArray>(Keys,key,Length(Keys));
  if i<0 then
    begin
      i:=-(i+1);
      Insert(key, Keys,i);
      Insert(AValue,ValueList,i)
    end
  else
    //begin
      ValueList[i]:=AValue
    //end;
end;

function TResultData.KeyExists(key: string): boolean;
begin
  result:=_BinSearch<string,TStringDynArray>(Keys,key,Length(Keys))>=0;
end;
{$endif KeyValueList}

{function floor(const v:Single):Integer;
begin
  if v<>trunc(v) if Single>0 then result:=Trunc(v) else
    ;
end;}

(*
function comp<T>(const a,b:T):integer;
begin
  result:=1;
  if a=b then result:=0
  else if a<b then result:=-1
end;

function comp<T>(const a,b:T;const rev:boolean):integer;
begin
  result:=1;
  if a=b then result:=0
  else if a<b then result:=-1 ;
  if rev then result:=-result
end;

function aComp<T>(const a,b:T;const rev:boolean):integer;
begin
  result:=1;
  if a=b then result:=0
  else if a<b then result:=-1  ;
  if rev then result:=-result
end;


function Lookup<T>(const Ar:array of T;const e:T;const reverse:boolean;const p:boolean):integer;    { TODO : handle p "position of Totals marked as arrayCompFiller" )  }
var compareRes,L,R,I:integer;rv:integer;

begin
    rv:=ifthen<integer>(reverse,-1,1);
    L := 0;R := length(Ar); I:=0;
    while L<=R do begin
        I:=L+{Math.floor}((R - L) shr 1{/ 2});
        compareRes:=(comp<T>(e,Ar[I]))*rv;
        if compareRes>0 then L:=I+1
        else begin R:=I-1;if(compareRes=0) then result:=I end;
    end;
    result:= -L-1
end;

function Lookup<T>(const Ar:array of T;const e:T;const reverse:array of boolean;const p:boolean):integer;    { TODO : handle p "position of Totals marked as arrayCompFiller" )  }
var compareRes,L,R,I:integer;
begin
    L := 0;R := length(Ar); I:=0;
    while L<=R do begin
        I:=L+{Math.floor}((R - L) shr 1{/ 2});
        compareRes:=comp<T>(e,Ar[I]{,reverse});
        if compareRes>0 then L:=I+1
        else begin R:=I-1;if(compareRes=0) then result:=I end;
    end;
    result:= -L-1
end;
*)
function arrayComp(const a,b:Variant;const rev:boolean;const p:boolean):TValueRelationship;                 overload;
var rv:TValueRelationship; va,vb:TVarType;
begin
    //rv:=Integer(rev)*2-1;
    va:=TVarData(a).VType;vb:=TVarData(b).VType;
    if va=vb then
      if a=b then begin result:=0; exit end;
    if string(a)=arrayCompFiller then begin  result:=ifthen<TValueRelationship>(p,-1,1);exit end;
    if string(b)=arrayCompFiller then begin  result:=ifthen<TValueRelationship>(p,1,-1);exit end;//arrayCompFiller is larger than anything
    rv:=ifthen<TValueRelationship>(rev=True,1,-1);

    //if (a=b) and (a=null) then begin result:= 0;exit end
    //else if a=null then begin result:= 1*rv;exit end
    //else if b=null then begin result:=-1*rv;exit end;
    if  (vb=va) and ((va in varsNumeric) or (va = varString)) then
       //((vb=varBoolean) and (vb=varBoolean)) or
       //((va in varsNumeric) and (vb =va)) or
       //((va=varString) and (vb=varString)) or
       //((va=varDate) and (vb=varDate))
       //then
       begin
         result:=-1*rv;
         if a<b then
           result:= 1*rv
       end;
end;

function arrayComp(const a,b:TVariantArray;const rev:boolean;const p:boolean):TValueRelationship;    overload;
var
  r,rv:TValueRelationship;
  i,m:integer;
begin
    rv:=ifthen<TValueRelationship>(rev=True,1,-1);
    //rv:=Integer(rev)*2-1;
    m:=Min<Integer>(High(a), High(b));
    for i:=0 to m do begin
      r:=arrayComp(a[i],b[i],rev,p);
      if r<>0 then begin result:=r*rv;exit end;
    end;
    result:=-1*rv;
    if Length(a)=Length(b) then begin result:= 0;exit end;
    if Length(a)<Length(b) then result:= 1*rv
end;

function arrayComp(const a,b:TVariantArray;const rev:array of boolean;const p:boolean):TValueRelationship;    overload;
var
  m,i:integer;
  r:TValueRelationship;
begin
    m:=Min<Integer>(High(a), High(b));
    for i:=0 to m do begin
      r:=arrayComp(a[i],b[i],rev[i],p);
      if r<>0 then begin result:=r;exit end;
    end;

    result:=-1;
    if Length(a)=Length(b) then begin result:= 0;exit end;
    if Length(a)<Length(b) then result:= 1
end;

function arrayLookup<T>(const arr:array of T;const e:T;const reverse:array of boolean;const rp:boolean):integer;
var compareres:  TValueRelationship; L,R,I:Integer;
begin
   // if not Array.isArray(arr)) then exit;
    L := 0;R := Length(arr);I:=0;
    while L<=R do begin
        I:=L+{floor}((R - L) {/ 2} shr 1);
        compareRes:=arrayComp(e,arr[I],reverse,rp);
        if compareRes>0 then
          L:=I+1
        else begin
          R:=I-1;
          if compareRes=0 then begin result:=I;exit end
        end
    end;
    result:= -L-1
end;

function arrayLookup<T>(const arr:array of T;const e:T;const reverse:boolean;const rp:boolean):integer;
var compareres:  TValueRelationship; L,R,I:Integer;
begin
   // if not Array.isArray(arr)) then exit;
    L := 0;R := Length(arr);I:=0;
    while L<=R do begin
        I:=L+{floor}((R - L) {/ 2} shr 1);
        compareRes:=arrayComp(e,arr[I],reverse,rp);
        if compareRes>0 then
          L:=I+1
        else begin
          R:=I-1;
          if compareRes=0 then begin result:=I;exit end
        end
    end;
    result:= -L-1
end;

//function Lookup<T>(const Ar:array of T;const e:T;const comp:TCompareFunc<T>;const reverse:boolean=false;const p:boolean=false):integer;  overload;     { TODO : handle p "position of Totals marked as arrayCompFiller" )  }
//var compareRes,L,R,I:integer;rv:integer;
//
//  //function comp(const a,b:T):integer;
//  //begin
//  //  result:=1;
//  //  if a=b then result:=0
//  //  else if a<b then result:=-1
//  //end;
//
//begin
//
//    rv:=ifthen<integer>(reverse,-1,1);
//    L := 0;R := length(Ar); I:=0;
//    while L<=R do begin
//        I:=L+{Math.floor}((R - L) shr 1{/ 2});
//        compareRes:=comp(e,Ar[I])*rv;
//        if compareRes>0 then L:=I+1
//        else begin R:=I-1;if(compareRes=0) then result:=I end;
//    end;
//    result:= -L-1
//end;

function Dice(const a:TDataRecord;const v:Variant;const mm:integer):TArray<TDataRecord>;
var n:TArray<TDataRecord>;
    i,j,m:integer;
begin
  if length(a)>0 then m:=1 shl Length(a) else m:=ifthen<integer>(mm>0,1,2);

  //m:=StrToInt('%'+TStringDynArray.fill(ifthen(length(a)>0,Length(a),ifthen(Length(colsIds>0),0,1)),1).ToString(''));
//    m:=parseInt(createFilled(a.length||(colsIds.length>0?0:1),1).join(''),2);
  setLength(n,m);
  for i:=0 to m-1 do begin
    n[i]:=TDataRecord.Fill(max<Integer>(length(a),1),0);
    for j:=0 to high(n[i]) do
      if i and (1 shl j)>0 then
        n[i][j]:=v
      else
        if assigned(a) then n[i][j]:=a[j] //else n[i][j]:=v
  end ;
    //n.push(createFilled(a.length,0).join('')+i.toString(2)).slice(-a.length).split('').map(function(el,i){return (el==='1'?v:a[i])}));
  result := n
end;


function min<T>(const a,b:T):T;
begin
   result:=ifthen<T>(a<=b,a,b)
end;

function max<T>(const a,b:T):T;
begin
   result:=ifthen<T>(a>b,a,b)
end;

function cube(const obj:TObjData;const options:TTotalsOptions):TObjData;
var
  i, j, k, rowNo, m_span:integer;
  rCube:string;
  row, col, r, c, id, Rw:TDataRecord;
  colStack, rowStack:array of TRecordStack;
  dims, fieldNames :TStringDynArray;
  cub:TTableData;
  colsIds, rowsIds, filtIds, measIds:TIntegerDynArray;
  rSort, cSort: array of boolean;
  rowAgg ,colAgg {,tmpAgg}:THeader;
  Aggs: THeaders;
  oprsIds:array of TAggregation;
  fieldTypes:array of THeaderType;
  filtRow:boolean=false;
 // dimensions:  TDimensions ;
//  data: TCubeData;
  //_result:TResultData;

  procedure calcVals;
  var m,r,c:integer;formula:string;v:TDataRecord;vv:Variant;
  begin
        for m:=0 to high(obj.dimensions.Measures) do begin
            for r:=0 to High(result.data) do begin
                formula:=obj.dimensions.measures[m].formula;
                if formula<>'' then
                  for c:=0 to High(result.cols) do
                    begin
                        v:=result.cols[c];
                        if result.data[r][c*length(obj.dimensions.measures)+m]='' then exit;
                        result.data[r][c*length(obj.dimensions.measures)+m]:=eval(
                                formula.replace('{val}',result.data[r][c*length(obj.dimensions.measures)+m],[rfReplaceAll])
                                .replace('{colTotal}',result.results[TDataRecord.Fill(Length(obj.dimensions.rows),arrayCompFiller).concat(v).ToString('#9','',false)].DataRecord[m],[rfReplaceAll])
                                .replace('{rowTotal}',result.results[result.rows[r].concat(TDataRecord.Fill(length(obj.dimensions.cols),arrayCompFiller)).ToString('#9','',false)].DataRecord[m],[rfReplaceAll])
                                .replace('{grandTotal}',result.results[TDataRecord.Fill(length(obj.dimensions.cols)+length(obj.dimensions.rows),arrayCompFiller).ToString('#9','',false)].DataRecord[m],[rfReplaceAll])
                        );
                    end;
                if obj.dimensions.measures[m].decimals>=0 then
                    for c:=0 to High(result.cols) do begin
                        vv:=result.data[r][c*Length(obj.dimensions.measures)+m];
                        if (TVarData(vv).vtype in [varEmpty,varNull])
                          or VarIsStr(vv) then
                            exit;
                        if TVarData(vv).vtype in varsOrdinal then
                          result.data[r][c*Length(obj.dimensions.measures)+m]:=format('%n',[double(vv)])
                        else if TVarData(vv).vtype in varsFloats then
                          result.data[r][c*Length(obj.dimensions.measures)+m]:=format('%.'+obj.dimensions.measures[m].decimals.ToString+'n',[double(vv)]) ;
                    end;
            end
        end
  end;

begin
  {$ifdef Profiling}
    Profiler.start;

  {$endif}

    setLength(fieldNames,length(obj.Headers.Columns[0]));
    setLength(fieldTypes,length(fieldNames));

    for i:=0 to High(obj.Headers.Columns[0]) do begin
        fieldNames[i]:=obj.Headers.Columns[0][i].name;
        fieldTypes[i]:=obj.Headers.Columns[0][i].typ;
    end;

    setLength(rowsIds,length(obj.dimensions.rows));for i:=0 to High(obj.dimensions.rows) do begin
      rowsIds[i]:=fieldNames.indexOf(obj.dimensions.rows[i]);
      if rowsIds[i]<0 then raise Exception.CreateFmt('Field [%s] not found!',[obj.dimensions.rows[i]]);
    end;
    setLength(colsIds,length(obj.dimensions.cols));for i:=0 to High(obj.dimensions.cols) do begin
      colsIds[i]:=fieldNames.indexOf(obj.dimensions.cols[i]);
      if colsIds[i]<0 then raise Exception.CreateFmt('Field [%s] not found!',[obj.dimensions.cols[i]]);
    end;
    setLength(measIds,length(obj.dimensions.measures)); setLength(oprsIds,length(obj.dimensions.measures));
    for i:=0 to High(obj.dimensions.measures) do begin
      measIds[i]:=fieldNames.indexOf(obj.dimensions.measures[i].field);
      if measIds[i]<0 then raise Exception.CreateFmt('Field [%s] not found!',[obj.dimensions.measures[i].field]);
      oprsIds[i]:=obj.dimensions.measures[i].operation;
    end;

    setLength(filtIds,length(obj.filters));
    for i:=0 to High(obj.filters) do
      filtIds[i]:=fieldNames.indexOf(obj.filters[i].fieldName);
    push<TDataRecord>(result.rows,TDataRecord.fill(ifthen<Integer>(options.grandRows,ifthen<Integer>(length(rowsIds)>0,length(rowsIds),ifthen<Integer>(length(colsIds)>0,0,1)),1),arrayCompFiller)); //grand total columns
    push<TDataRecord>(result.cols,TDataRecord.fill(ifthen<Integer>(options.grandCols,length(colsIds),0),arrayCompFiller)); //grand total rows
    // add grand total row anyway (horizonal measures case)

    {$ifdef Profiling} Profiler.Log('Cube initialization.');{$endif}

    setLength(rSort,length(obj.dimensions.rows));for i:=0 to high(obj.dimensions.rows) do rSort[i]:=IndexOf<String>(obj.descends,obj.dimensions.Rows[i])>=0;
    setLength(cSort,length(obj.dimensions.cols));for i:=0 to high(obj.dimensions.cols) do cSort[i]:=IndexOf<String>(obj.descends,obj.dimensions.Cols[i])>=0;
    for rowNo:=0 to high(obj.data) do begin
        for j:=0 to high(filtIds) do
            if obj.filters[j].data.Lookup(obj.Data[rowNo][filtIds[j]])<0 then
              begin filtRow:=true;break;end;
        if filtRow then begin filtRow:=false;continue end;                  // if filter list is inroduced then skip elements not in the filter list
        setLength(row,length(rowsIds));
        for i:=0 to high(rowsIds) do
          row[i]:= obj.Data[rowNo][rowsIds[i]];     //take values of selected rows into row[]
        setLength(col,length(colsIds));
        for i:=0 to high(colsIds) do                //take values of selected columns into columns[]
          col[i]:= obj.Data[rowNo][colsIds[i]];

        // calculate subtotal rows
        for i:=ifthen<Integer>(options.subRows, 1, length(rowsIds)) to length(rowsIds) do begin
            r:=slice<TDataRecord>(row,0,i).concat(TDataRecord.Fill(length(rowsIds)-i,arrayCompFiller));  // explore using TArray<FieldType> instead of TVariantArray?
            j:=arrayLookup<TDataRecord>(result.rows,r,rSort,options.br);
            if j<0 then
              splice<TTableData>(result.rows,-j-1,0,[r]);
        end;

        // calculate subtotal columns
        for i:=ifthen<Integer>(options.subCols,1,length(colsIds)) to length(colsIds) do begin
            c:=slice<TDataRecord>(col,0,i).concat(TDataRecord.fill(length(colsIds)-i,arrayCompFiller));
            k:=arrayLookup<TDataRecord>(result.cols,c,cSort,options.bc);
            if k<0 then
              splice<TTableData>(result.cols,-k-1,0,[c]);
        end;

        // ********************* check factorial array multiplication from here
        cub:=Dice(row.Slice(0,length(rowsIds)).concat(col.slice(0,length(colsIds))),arrayCompFiller,length(colsIds));

        for i:=0 to high(cub) do  begin
            rCube:=cub[i].toString(splitter,'',false);
            if not result.results.KeyExists(rCube) then
              result.results[rCube]:=TResults.Create(length(measIds));
            for j:=0 to High(measIds) do
              result.results[rCube].TableData[j].Push(obj.Data[rowNo][measIds[j]]) // measIds.forEach(function(el,idx){result[rCube][idx].push(obj.data[rowNo][el])})
        end
    end;//end scanning table

    {$ifdef Profiling} Profiler.Log('Cube tables scanned.');{$endif}

    if Length(result.cols)>0 then begin
        setLength(colStack,length(result.cols[0]));
        for j:=0 to high(result.cols[0]) do begin
            colStack[j].i:=0;
            colStack[j].v:=result.cols[0].slice(0,j+1)
        end;//i=0;
        for i:=0 to high(result.cols) do begin
            setLength(result.headers.columns,Length(result.cols[0]));
            for j:=0 to high(result.cols[0]) do begin
                if arrayComp(result.cols[i].Slice(0,j+1),colStack[j].v,false,false)<>0 then begin
                    m_span:=i-colStack[j].i;
                    Insert(THeader.Create({name:=}colStack[j].v[j],{typ:=}htString,{span:=}m_span * length(measIds)),result.headers.columns[j],Length(result.headers.columns[j]));
                    colStack[j].i:=i;colStack[j].v:=result.cols[i].slice(0,j+1);
                end
            end;
            // column identify subtotals
            if result.cols[i].indexOf(arrayCompFiller)>-1 then
                for j:=0 to high(measIds) do
                      result.colsHeadSubTotals.push(i*length(measIds)+j)
        end;
        for i:=0 to high(result.cols) do
          for j:=0 to high(obj.dimensions.measures) do begin
            //tmpAgg.Name:=ifthen<string>(obj.dimensions.measures[j]._Label<>'', obj.dimensions.measures[j]._Label, oprsIds[j].Name+' of '+obj.dimensions.measures[j].Field);
            //tmpAgg.typ:=Ifthen<THeaderType>(IndexOf<string>(['sum','count','distinct_count','distinct_sum','mean','average','mean','stddev','mode','median'], lowerCase(oprsIds[j].Name))>-1,htDouble,fieldTypes[measIds[j]]);
            insert(
              THeader.Create(
                ifthen<string>(obj.dimensions.measures[j]._Label<>'', obj.dimensions.measures[j]._Label, oprsIds[j].Name+' of '+obj.dimensions.measures[j].Field),
                Ifthen<THeaderType>(IndexOf<string>(['sum','count','distinct_count','distinct_sum','mean','average','mean','stddev','mode','median'], lowerCase(oprsIds[j].Name))>-1,htDouble,fieldTypes[measIds[j]])
              )
             ,Aggs,length(Aggs));
          end;
        Insert(Aggs,result.headers.columns,length(result.headers.columns));
        for j:=0 to high(result.cols[0]) do begin
              m_span:=length(result.cols) -colStack[j].i;
              //with tmpAgg do begin Name:=colStack[j].v[j]; typ:=htString; span:=m_span*length(measIds) end;
              insert(THeader.Create(colStack[j].v[j], htString, m_span*length(measIds)),result.headers.columns[j],length(result.headers.columns[j]));
        end;

    end;
    {$ifdef Profiling} Profiler.Log('Cube Headers prepared');{$endif}

   if length(result.rows)>0 then begin
     for j:=0 to ifthen<Integer>(length(rowsIds)>0,length(rowsIds),ifthen<Integer>(length(colsIds)>0,0,1)) do begin
          setLength(rowStack,j+1);rowStack[j].i:=0; rowStack[j].v:=slice<TDataRecord>(result.rows[0],0,j+1)
     end;
     i:=0;
     setLength(result.headers.rows,length(result.rows[0]));
     for i:=0 to high(result.rows) do begin
         if result.rows[i].indexOf(arrayCompFiller)>-1 then result.rowsHeadSubTotals.Push(i);
         for j:= 0 to high(result.rows[i]) do begin
           if (j<length(rowStack)) and (arrayComp(result.rows[i].Slice(0,j+1),rowStack[j].v, false,false)<>0) then begin
              m_span:= i-rowStack[j].i;
              //if m_span=0 then
              //  beep;              // for debugging
              //setLength(result.headers.rows[j],length(result.headers.rows[j])+1);
              //with result.headers.rows[j][high(result.headers.rows[j])] do begin
              //  name := rowStack[j].v[j];
              //  span:=m_span;
              //  typ:=htString;
              //end;
              insert(THeader.create(rowStack[j].v[j],htString, m_span),result.headers.rows[j],length(result.headers.rows[j]));
              setLength(result.headers.rows[j],high(result.headers.rows[j])+m_span) ;

              rowStack[j].i:=i;rowStack[j].v:=result.rows[i].Slice(0,j+1);
           end;
          end
     end;
     for j:=0 to high(result.rows[0]) do begin
       m_span:=length(result.rows) -rowStack[j].i;
       //if m_span=0 then
       //  beep;              // for debugging
       //setLength(result.headers.rows[j],length(row[j])+1);
       //with result.headers.rows[j][high(result.headers.rows[j])] do
       //  begin name:= rowStack[j].v[j]; typ:=htString; Span:=m_span end;
       insert(THeader.Create(rowStack[j].v[j], htString, m_span),result.headers.rows[j],length(result.headers.rows[j]));
       setLength(result.headers.rows[j],high(result.headers.rows[j])+m_span);
     end;
   end;
   {$ifdef Profiling} Profiler.Log('Cube SubTotals prepared');{$endif}

   for i:=0 to High(result.results.Keys) do begin
      for j:=0 to high(oprsIds) do begin
        if LowerCase(oprsIds[j].Name)='count' then
          result.results.ValueList[i].DataRecord[j]:=length(result.results.ValueList[i].TableData[j])//reduce<Variant>(result.results.ValueList[i].TableData[j], oprsIds[j].operation, 0)
        else if LowerCase(oprsIds[j].Name)='distinct_count' then
          result.results.ValueList[i].DataRecord[j]:=Length(result.results.ValueList[i].TableData[j].unique())//reduce<Variant>(result.results.ValueList[i].TableData[j], oprsIds[j].operation, 0)
        else if LowerCase(oprsIds[j].Name)='concat' then
          result.results.ValueList[i].DataRecord[j]:=result.results.ValueList[i].TableData[j].tostring(', ','',false)
        else if LowerCase(oprsIds[j].Name)='distinct_concat' then
          result.results.ValueList[i].DataRecord[j]:=result.results.ValueList[i].TableData[j].unique().toString(', ','',false)
        else result.results.ValueList[i].DataRecord[j]:=reduce<Variant>(result.results.ValueList[i].TableData[j],oprsIds[j].operation)
       end;
   end;
   {$ifdef Profiling} Profiler.Log('Cube Reduction calculated');{$endif}

   for i:=0 to high(result.rows) do begin
     setLength(Rw,0);
     for j:=0 to high(result.cols) do begin
       id:=result.results[result.rows[i].concat(result.cols[j]).ToString(splitter,'',false)].DataRecord;
       if length(id)=0 then id:=TDataRecord.Fill(length(measIds),'');
       for k:=0 to high(id) do
         insert(id[k],Rw,length(rw))
     end;
     result.data.push(Rw);
   end;
   if length(colsIds)>0 then
     for i:=0 to high(measIds) do
       insert((arrayLookup<TVariantArray>(result.cols ,TDataRecord.Fill(length(colsIds),arrayCompFiller)))*length(measIds)+i,result.colsHeadGrandTotals,length(result.colsHeadGrandTotals));
       //insert((result.cols.lookup(TDataRecord.Fill(length(colsIds),arrayCompFiller)))*length(measIds)+i,result.colsHeadGrandTotals,length(result.colsHeadGrandTotals));
   if length(rowsIds)>0 then
     Insert(arrayLookup<TVariantArray>(result.rows,TDataRecord.Fill(length(rowsIds),arrayCompFiller)),result.rowsHeadGrandTotals,length(result.rowsHeadGrandTotals));
     //Insert(result.rows.lookup(TDataRecord.Fill(length(rowsIds),arrayCompFiller)),result.rowsHeadGrandTotals,length(result.rowsHeadGrandTotals));
   {$ifdef Profiling} Profiler.Log('Cube GrandTotals prepared');{$endif}
   calcVals;
   result.Dimensions:=obj.Dimensions;
   {$ifdef Profiling} Profiler.Log('Cube finalized');{$endif}

end;

function cube(const obj:TObjData):TObjData;
begin
   result:=cube(obj,defaultTotalsOption)
end;

function _sum(const a, b: variant; const i: integer; const Arr: array of variant):variant;
begin
  result:=a+b
end;

function _max(const a, b: variant; const i: integer; const Arr: array of variant):variant;
begin
  result:=ifthen<variant>(a>=b,a,b)
end;

function _min(const a, b: variant; const i: integer; const Arr: array of variant):variant;
begin
  result:=ifthen<variant>(a<=b,a,b)
end;

function _first(const a, b: variant; const i: integer; const Arr: array of variant):variant;
begin
  result:=a
end;

function _last(const a, b: variant; const i: integer; const Arr: array of variant):variant;
begin
  result:=b
end;

function _count(const a, b: variant; const i: integer; const Arr: array of variant):variant;
begin
  result:=ifthen<variant>(varIsEmpty(a),0,a);
  if not VarIsEmpty(b) then
    Result:= Int64(Result)+1
end;

function _average(const a, b: variant; const i: integer; const Arr: array of variant):variant;
begin
   if i=high(Arr) then result:=double(a+b)/ length(Arr) else result:=a+b;
end;

function _median(const a, b: variant; const i: integer; const Arr: array of variant):variant;
var p:integer;r:TVariantArray;
begin
   if i=high(Arr) then begin
     p:=Length(Arr) shr 1;
     setLength(r,Length(Arr));
     Move(Arr[0],r[0],length(Arr)*SizeOf(Variant));
     r.Sort();
     if (Length(Arr) mod 2>0) or not (TVarData(Arr[0]).vtype in varsNumeric) then
       result:=r[p]
     else
       result:=Double(r[p-1]+r[p])/2;
   end;
end;

function _mode(const a, b: variant; const i: integer; const Arr: array of variant):Variant;
begin
  if i=High(Arr) then
    result:=Mode<Variant>(arr);
end;

function _stddev(const a, b: variant; const i: integer; const Arr: array of variant):Variant;
var Variance:Double;Mean:Double; j:integer;
begin
  result:=a+b;
  if i=High(Arr) then begin
    Variance:=0;
    Mean:=Double(result)/Length(Arr);
    for j:=0 to High(Arr) do Variance:=Variance+sqr(double(Arr[j])-Mean);
    result:=Sqrt(Variance/Length(Arr));
  end;

end;

const
    aggSum    :TAggregation=    (name:'Sum'    ; operation:_sum);
    aggMax    :TAggregation=    (name:'Max'    ; operation:_max);
    aggMin    :TAggregation=    (name:'Min'    ; operation:_min);
    aggFirst  :TAggregation=    (name:'First'  ; operation:_first);
    aggLast   :TAggregation=    (name:'Last'   ; operation:_last);
    aggCount          :TAggregation=    (name:'Count'  ; operation:nil);
    aggDistinctCount  :TAggregation=    (name:'Distinct_Count' ; operation:nil);
    aggConcat         :TAggregation=    (name:'Concat'         ; operation:nil);
    aggDistinctConcat :TAggregation=    (name:'Distinct_Concat'; operation:nil);
    aggAverage:TAggregation=    (name:'Mean'   ; operation:_average);
    aggMedian :TAggregation=    (name:'Median' ; operation:_median);
    aggMode   :TAggregation=    (name:'Mode'   ; operation:_mode);
    aggStdDev :TAggregation=    (name:'StdDev' ; operation:_stddev);


initialization

  AggregationArray:=[
    aggCount   ,
    aggDistinctCount,
    aggConcat  ,
    aggDistinctConcat,
    aggSum     ,
    aggMax     ,
    aggMin     ,
    aggFirst   ,
    aggLast    ,
    aggAverage ,
    aggMedian  ,
    aggMode    ,
    aggStdDev
  ];

end.

