unit dcube;
{H+}
{$ifdef fpc}
  {$mode delphi}
  {$ModeSwitch advancedrecords}
  {$Macro on}
  {$ModeSwitch typehelpers}

{$endif}

interface

uses
   {$ifdef fpc}LCLType, {$endif}
   Types, Classes, SysUtils
   , Generics.Collections, Generics.Defaults
   ,Variants
   //{$ifdef MSWINDOWS}, windows{$endif}
   {$ifdef Profiling}, hirestimer{$endif}
   , Controls, StdCtrls, Graphics
  //,fpjson,extjsjson
  ;


const   varsOrdinal=[varShortInt,varSmallInt,varInteger,varInt64,varByte,varWord,varLongWord,varUInt64];
        varsFloats=[varSingle,varDouble,varCurrency, varDate];
        varsNumeric=varsOrdinal + varsFloats;

        HeaderTypeStr:array[0..12] of string= ('Unknowen', 'String', 'Boolean', 'Byte', 'Word', 'Integer', 'Int64', 'Single', 'Double', 'Currency', 'DateTime', 'SingleComplex', 'DoubleComplex');

type

  TFilterCallback<T,PT>=function (const a:T;const i:Integer; Arr:PT):boolean of object;

  TCompareFunc<T>=function (const a,b:T):integer;

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

  THeadersHelper=record helper for THeaders
  public
    function toString():string;
  end;

  TResultHeaders=record
    columns,rows:Array of THeaders

  end;



  TStringArrayHelper = record helper for TStringDynArray
    function IndexOf(const str:string):integer;
    function Lookup(const str:string;const CaseSensitive:boolean=true):{$ifdef fpc}int64{$else} integer{$endif};
  end;
  PStringArray=^TStringArray;
  TStringArray=TStringDynArray;
  TVariantArray=TArray<Variant>;
  TDateTimeArray=TArray<TDateTime>;
  TReduceCallback<T> = function(const a,b:T;const i:integer;const arr:array of T):T;

  { TVariantArrayHelper }

  TVariantArrayHelper=record helper for TVariantArray
  type

    PPType=^PType;
    PType=^TType;
    TType=Variant;
    TSelf=TVariantArray;
    TFilterFunc=TFilterCallback<TType,TSelf>;
    TVariantCompareFunc=TCompareFunc<TType>;
  private
  public
    function Concat(const Arr:TSelf):TSelf;
    function ToString(const Seperator: string=', '; const quote: string='"';const Brackets:boolean=true): string;
    function IndexOf(const AVal:Variant):integer;
    function Filter(const func:TFilterFunc):TSelf;
    function Sort():TSelf;
    function Unique():TSelf;
    function Push(const AValue: Variant): Integer;
    function Reduce(const func:TReduceCallback<Variant>):Variant;//  _SIMPLEREDUCE_;
    class function Fill(ACount:integer;const AValue:Variant):TSelf;static;
    class function uniqueFilt(const a:TType;const i:integer;arr:TSelf):boolean;static;
    class function Compare(const a,b:Variant):integer;static;
  end;

  TVariantArrayArray=TArray<TVariantArray>;

  { TVariantArray2DHelper }

  TVariantArray2DHelper=record helper for TVariantArrayArray
    public
      function ToString(const Seperator: string=','; const quote: string='"'; const Brackets:boolean=false): string;

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



  { TDataRecord }

  TDataRecord = TVariantArray;
  TTableData = TVariantArrayArray;

  { TDataRecord2 }

  TDataRecord2<T>=record                 // allocated heap data for cacheing
  private
    function GetCurrent: T;
    function GetItem( index: integer): T;
    procedure SetItem( index: integer; const AValue: T);
  public
    Data:TArray<T>;
    Indicator:integer;
    property Current:T read GetCurrent;
    function Push(const AValue:T):integer;
    function reduce(const func: TReduceCallback<T>): T;       overload;
    class function reduce(const self:TArray<T>;const func:TReduceCallback<T>):T;  overload;static;
    procedure Shrink;
    constructor Create(const ACapacity:integer);
    property Item[index:integer]:T read GetItem write SetItem ;default;
    class operator initialize({$ifdef fpc}var{$else}out{$endif} dest:TDataRecord2<T>);
  end;

  TTableData2<T> = array of TDataRecord2<T>;

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
    class operator initialize({$ifdef fpc}var{$else}out{$endif} dest:TTotalsOptions); overload;
  end;

//const defaultTotalsOption:TTotalsOptions = (bc:false; br:false; grandCols:true; grandRows:true; subCols:true; subRows:true);

type

  { TResults }
  TResults=record
    TableData:{$ifdef fpc}TTableData2<Variant>{$else}TTableData{$endif};
    DataRecord:TDataRecord;
    constructor Create(const MeasureCount,RowCount:integer);
  end;


//  {$ifndef fpc}
//  { TKeyValueList<TK,TR> }
//
//  TSortedKeyValueList<TK,TV>=record
//  type
//    TKeyArray=TArray<TK>;
//    TValueArray=TArray<TV>;
//  public
//    Keys:TKeyArray;
//    Values:TValueArray;
//  private
//    function GetValues( key: TK): TV;
//    procedure SetValues( key: TK; AValue: TV);
//  public
//    function ContainsKey(const key: TK): boolean;
//    function IndexOfKey(const Key:TK):integer;
//    function AddOrUpdate(const Key:TK;const AValue:TV):integer;
//    procedure Add(const Key:TK;const AValue:TV);
//    function TryGetValue(const Key:TK;out Value:TV):boolean;
//    property Items[key:TK]:TV read GetValues write SetValues ;default;
//    function Count:integer;
//    procedure Remove(const index:integer);
//  end;
//  {$endif}

  TResultData=
//  {$ifdef fpc}
  TDictionary<string,TResults>
//  {$else}
//  TSortedKeyValueList<string,TResults>
//  {$endif}
  ;

  { TObjData }
  PObjData=^TObjData;
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
    procedure SaveToFile(const FileName:TFileName);
    class operator Initialize({$ifdef fpc}var{$else}out{$endif} dest:TObjData);
    class operator Finalize(var dest:TObjData);
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
    procedure TextChanged; {$ifdef fpc}override;{$endif}
  published

    procedure Resize; override;
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
      FDataObject: PObjData;
      //FDimensions:TDimensions;
      FFieldsPan,FColumnsPan,FRowsPan,FMeasuresPan:TDimensionPan;
      FOnUpdateCubeDimenstion: TNotifyEvent;
      FButtons:array of TDimensionButton;
      procedure UpdateMeasures(Sender:TObject);
      procedure SetDataObject(const AValue: PObjData);
//      procedure SetDimentions(const AValue: TDimensions);
      procedure SetOnUpdateCubeDimenstion(AValue: TNotifyEvent);
      procedure updateAvailableDimensions; virtual ;
      procedure updateCubeDimensions; virtual;
    public
      Dimensions: TDimensions;
      constructor Create(AOwner: TComponent); override;
//      property Dimensions:TDimensions read FDimentions write SetDimentions;
      property DataObject:PObjData read FDataObject write SetDataObject;
      property OnUpdateCubeDimenstion:TNotifyEvent read FOnUpdateCubeDimenstion write SetOnUpdateCubeDimenstion;


  end;

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
  type

  { TCubeArr }

  TCubeArr=class
    class function min<T>(const a,b:T):T; static;
    class function max<T>(const a,b:T):T; static;
    class function arrayLookup(const arr:array of TVariantArray;const e:TVariantArray;const reverse:array of boolean;const rp:boolean):integer;   overload;
    class function arrayLookup(const arr:array of TVariantArray;const e:TVariantArray;const reverse:boolean=false;const rp:boolean=false):integer;   overload;
    class function IfThen<T>(const cond:boolean;const whenTrue,whenFalse:T):T; static;
    class procedure QuickSort<T>(var Arr: array of T; L, R: Longint; const Descending: boolean; const Compare: TComparefunc<T>);
    class function BinSearch<T>(const Arr:array of T;const Val:T; R:integer):integer;
    class function cmp<T>(const a, b: T): integer;
  end;

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
  AggrStr :TStringDynArray;
  defaultTotalsOption:TTotalsOptions ;

implementation

const splitter=#9;
      arrayCompFiller=#$ff+'Total';

      defaultButtonHeight=40;
      defaultButtonWidth =200;


const oprChars=['*','/','+','-'];


function EvalStr( str:string):double;
var i,start:integer; len:integer;
    oprs:array{[0..127]} of string;
    nums:array{[0..127]} of double;

begin
len:=0;
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
    i,j,k:integer; inQuoteLvl:boolean;

    Enclosure:TEnclosure;
    Enclosures:array of TEnclosure;
    r:string;
begin
  inQuoteLvl:=false    ;
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
      r:=EvalStr(copy(str, Enclosures[high(Enclosures)].start+1,j )).ToString ; // evaluate whats between the parenthesis
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
    if str[i]='' then continue else
    if TryStrToInt(str[i],vInt) then result[i]:=vInt else
    if TryStrToFloat(str[i],vDouble) then result[i]:=vDouble else
    if TryStrToBool(str[i],vBool) then result[i]:=vBool else
    if TryStrToDateTime(str[i],vDateTime) then result[i]:=vDateTime else
    result[i]:=str[i]

end;

{ TDimensionButton }


//{$ifndef fpc}
//
// { TSortedKeyValueList }
//
//function TSortedKeyValueList<TK,TV>.Count: integer;
//begin
//  result:=Length(Keys)
//end;
//
//procedure TSortedKeyValueList<TK,TV>.Remove(const index: integer);
//begin
//  if (index>=0) and (index<Count) then begin
//    Delete(Keys,index,1);
//    Delete(Values,index,1);
//  end;
//end;
//
//function TSortedKeyValueList<TK,TV>.GetValues( key: TK): TV;
//var i:integer;
//begin
//  i:= TCubeArr.BinSearch<TK>(Keys ,key,Length(Keys));
//  if i>=0 then
//    result:=Values[i]
//end;
//
//procedure TSortedKeyValueList<TK,TV>.SetValues( key: TK;  AValue: TV);
//var i:integer;
//begin
//  i:=TCubeArr.BinSearch<TK>(Keys,key,Length(Keys));
//  if i<0 then
//    begin
//      i:=-(i+1);
//      Insert(key, Keys,i);
//      Insert(AValue,Values,i)
//    end
//  else
//    //begin
//      Values[i]:=AValue
//    //end;
//end;
//
//function TSortedKeyValueList<TK, TV>.TryGetValue(const Key: TK; out Value: TV): boolean;
//var i:integer;
//begin
//  i:=TCubeArr.BinSearch<TK>(Keys,key,Length(Keys));
//  result:= i>-1;
//  if result then
//    Value:=Values[i]
//end;
//
//function TSortedKeyValueList<TK,TV>.ContainsKey(const key: TK): boolean;
//begin
//  result:=TCubeArr.BinSearch<TK>(Keys,key,Length(Keys))>=0;
//end;
//
//function TSortedKeyValueList<TK,TV>.IndexOfKey(const Key: TK): integer;
//begin
//  result:=TCubeArr.BinSearch<TK>(Keys,key,Length(Keys));
//end;
//
//procedure TSortedKeyValueList<TK, TV>.Add(const Key: TK; const AValue: TV);
//var i:integer;
//begin
//  i:=TCubeArr.BinSearch<TK>(Keys,key,Length(Keys));
//  if i<0 then
//    begin
//      i:=-(i+1);
//      Insert(key, Keys,i);
//      Insert(AValue,Values,i)
//    end
////  else
////      ValueList[i]:=AValue ;
//end;
//
//function TSortedKeyValueList<TK,TV>.AddOrUpdate(const Key: TK; const AValue: TV): integer;
//var i:integer;
//begin
//  i:=TCubeArr.BinSearch<TK>(Keys,key,Length(Keys));
//  if i<0 then
//    begin
//      i:=-(i+1);
//      Insert(key, Keys,i);
//      Insert(AValue,Values,i)
//    end
//  else
//    //begin
//      Values[i]:=AValue ;
//  result:=i
//    //end;
//end;
//
//{$endif}
{ TDataRecord2 }

function TDataRecord2<T>.GetCurrent: T;
begin
  result:=Data[Indicator]
end;

function TDataRecord2<T>.GetItem( index: integer): T;
begin
  result:=Data[Index]
end;

procedure TDataRecord2<T>.SetItem( index: integer; const AValue: T);
begin
  Data[index]:=AValue
end;

function TDataRecord2<T>.Push(const AValue: T): integer;
begin
  inc(Indicator);
  Data[indicator]:=AValue;
  result:=Indicator;
  //WriteLn(AValue);

end;

function TDataRecord2<T>.reduce(const func: TReduceCallback<T>): T;
begin
  result:=Reduce(data,func);
end;

procedure TDataRecord2<T>.Shrink;
begin
  setLength(Data,Indicator{$ifdef fpc}+1{$endif}) // TODO : quick workaround for weird behavior in delphi only - adding one to the final array
end;

constructor TDataRecord2<T>.Create(const ACapacity: integer);
begin
  setLength(Data,ACapacity)
end;

class operator TDataRecord2<T>.initialize({$ifdef fpc}var{$else}out{$endif} dest: TDataRecord2<T>);
begin
  dest.Indicator:=-1;
end;

class function TDataRecord2<T>.reduce(const self: TArray<T>; const func: TReduceCallback<T>): T;
var i,l:integer;
begin
  if High(Self)>-1 then
    result:=Self[0];
  for i:=1 to high(self) do
    result:=func(result,Self[i],i,Self)
end;

{ TVariantArray2DHelper }

function TVariantArray2DHelper.ToString(const Seperator: string; const quote: string; const Brackets:boolean): string;
var i:integer;
begin
  result:='';
  if length(Self)>0 then begin
    for i:=0 to High(Self) do
      result:=Result+Seperator+sLineBreak+Self[i].ToString(Seperator,Quote, Brackets) ;
    delete(result,1,Length(Seperator)+1);
  end;
  if Brackets then result:='['+result+']';
end;

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
  if (Source is TDimensionButton) then begin with TDimensionButton(Source) do
    if Self.Caption='Rows' then begin
      FMeasures.Hide;
      Align:=alTop ;
      Top:=y;
      Height:=defaultButtonHeight;
    end
    else if Self.Caption='Measures' then begin
      FMeasures.Top:=(Height-FMeasures.ClientHeight) div 2;
      FMeasures.Left:=Width-FMeasures.Width-4;
      FMeasures.Show;
      FMeasures.OnChange:=TPivotControl(Self.parent).UpdateMeasures;
      Left:=X;
      Width:=defaultButtonWidth;
      Align:=alLeft;
    end else  begin
     FMeasures.Hide;
     Left:=X;
     Width:=defaultButtonWidth;
     Align:=alLeft;
    end;
//    FSpace.Hide ;
    TDimensionButton(Source).Parent:=Self;
    {$ifdef fpc}SetControlIndex(TDimensionButton(Source),ControlCount-1); {$endif}
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
  Height:=100;
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
  if (Source is TDimensionButton) then begin with TDimensionButton(Source) do
    if TGroupBox(Parent).Caption='Rows' then begin
      FMeasures.Hide;
      Top:=Parent.Top+Y;
      Align:=alTop ;
      Height:=defaultButtonHeight;
    end
    else if TGroupBox(Parent).Caption='Measures' then begin
      FMeasures.top:=(Height-FMeasures.ClientHeight) div 2;
      FMeasures.Left:=Width-FMeasures.Width-4;
      FMeasures.Show;
      FMeasures.OnChange:=TPivotControl(Self.Parent.Parent).UpdateMeasures;
      Left:=Parent.Left+X;
      Align:=alLeft;
      Width:=defaultButtonWidth;

    end else  begin
      FMeasures.Hide;
      Left:=Parent.Left+X;
      Align:=alLeft;
      Width:=defaultButtonWidth;
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

procedure TDimensionButton.Resize;
begin
   inherited ;
//   if TCubeArr.min<integer>(Width,Height)>0 then
     FPng.SetSize(Width,Height);

end;

constructor TDimensionButton.Create(AOwner: TComponent);
var i:integer;
begin
  inherited ;
  FPng:=TBitmap.Create;
  Parent:=TWinControl(AOwner);
  FBackGroundColor:=clSilver;
{$ifdef fpc}  ChildSizing.SetGridSpacing(4);   {$endif}
  FMeasures:=TComboBox.Create(Self);
{$ifdef fpc}  FMeasures.BorderStyle:=bsNone;     {$endif}
  FMeasures.Visible:=false;
  FMeasures.Parent:=Self;
  for i:=0 to High(AggregationArray) do
    FMeasures.Items.Add(AggregationArray[i].Name);
  FMeasures.ItemIndex:=0;
  //FMeasures.Align:=alRight;
  //FMeasures.Align:=alNone;

  FMeasures.Width:=defaultButtonWidth-80;
  FMeasures.top:=8;

  Width := defaultButtonWidth;
  //Color:=clGray;//$ff8800;
//  AutoSize:=True;
  DragKind:=dkDrag;
  DragMode:=dmAutomatic;
  DragCursor:=crDrag;
  Cursor:=crSizeAll;



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

    {$ifdef fpc}
    Canvas.Erase;
    {$else}
    Canvas.FloodFill(1,1,TransparentColor,TFillStyle.fsBorder);
    {$endif}
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

    with Canvas.TextExtent(Caption) do
      Canvas.TextRect(FocusRect,4,(FPng.Height-cy) div 2,Caption);
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

procedure TPivotControl.SetDataObject(const AValue: PObjData);
begin
  //if FDataObject=AValue then Exit;
  FDataObject:=AValue;
  updateAvailableDimensions;
end;

procedure TPivotControl.updateAvailableDimensions;
var i:Integer;
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


  FFieldsPan      :=TDimensionPan.Create(AOwner);
  FFieldsPan.Caption:='Available Fields';
  FFieldsPan.top:=1000;
  //  {$ifdef fpc}FFieldsPan.ChildSizing.SetGridSpacing(panSpacing);  {$endif}
  FFieldsPan.Parent:=Self;

  FMeasuresPan:=TDimensionPan.Create(AOwner);
  FMeasuresPan.Caption:='Measures';
  FMeasuresPan.Top:=1001;
  //{$ifdef fpc}FMeasuresPan.ChildSizing.SetGridSpacing(panSpacing); {$endif}
  FMeasuresPan.Parent:=Self;

  FColumnsPan      :=TDimensionPan.Create(AOwner);
  FColumnsPan.Caption:='Columns';
  FColumnsPan.Top:=1002;
  //{$ifdef fpc}FColumnsPan.ChildSizing.SetGridSpacing(panSpacing);  {$endif}
  FColumnsPan.Parent:=Self;

  FRowsPan   :=TDimensionPan.Create(AOwner);
  FRowsPan.Caption:='Rows';
//  {$ifdef fpc}FRowsPan.ChildSizing.SetGridSpacing(panSpacing);   {$endif}
  FRowsPan.Parent:=Self;

  FFieldsPan.Align:=alTop;
  FMeasuresPan.Align:=alTop;
  FColumnsPan.Align:=alTop;
  FRowsPan.Align   :=alLeft;


end;

{ THeadersHelper }

function THeadersHelper.toString(): string;
var i:integer;
begin
  result:='';
  for i:=0 to High(Self) do
    result:=result+format('(Name: %s, Span: %d, size: %d, type:%s), ',[Self[i].Name, Self[i].Span, self[i].size, HeaderTypeStr[ord(Self[i].typ)]])+sLineBreak;

  result:='['+result+']'
end;

{ TResults }

constructor TResults.Create(const MeasureCount,RowCount:integer);
var i:integer;
begin
  setLength(TableData,MeasureCount);
  {.$ifdef fpc}
  for i:=0 to high(TableData) do
    setLength(TableData[i]{$ifdef fpc}.Data,RowCount{$else},0{$endif});
  {.$endif}
  setLength(DataRecord,MeasureCount);
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
var sl:TStringList;i:Integer;sp:TStringArray;
begin
  sl:=TStringList.Create;
  sl.LoadFromFile(FileName);
  setLength(Headers.columns,1);
  setLength(Headers.rows,0);
  if sl.count>0 then
    begin
      if LowerCase(ExtractFileExt(FileName))='.csv' then
        sp:=sl[0].split([','],'"')
      else if LowerCase(ExtractFileExt(FileName))='.tab' then
        sp:=sl[0].split([#9]);
      setLength(Headers.columns[0],length(sp));
      for i:=0 to High(sp) do with Headers.columns[0][i] do begin
        Name:=sp[i];
        span:=1;
        typ:=htUnknowen;
      end
    end;
  setLength(Data,SL.Count-1);
  if LowerCase(ExtractFileExt(FileName))='.csv' then
    for i:=1 to sl.Count-1 do
      Data[i]:=StringsToDataRec(sl[i].Split([','],'"'))
  else if LowerCase(ExtractFileExt(FileName))='.tab' then
    for i:=1 to sl.Count-1 do
      Data[i]:=StringsToDataRec(sl[i].Split([#9]));
  FreeAndNil(sl);
end;

procedure TObjData.SaveToFile(const FileName: TFileName);
var sl:TStringList;i,j:Integer;sp:TStringArray;
begin
  sl:=TStringList.Create;
  for i:=0 to High(Headers.columns) do begin
    setLength(sp,Length(Headers.columns[i]));
    for j:=0 to High(sp) do  sp[j]:=Headers.columns[i][j].Name;
    if LowerCase(ExtractFileExt(FileName))='.csv' then
      sl.add(string.Join(',',sp))
    else if LowerCase(ExtractFileExt(FileName))='.tab' then
      sl.add(string.Join(#9,sp))
  end;
  if LowerCase(ExtractFileExt(FileName))='.csv' then
    for i:=0 to High(Data) do
      sl.add(Self.Data[i].ToString(',','',false))
  else if LowerCase(ExtractFileExt(FileName))='.tab' then
    for i:=0 to High(Data) do
      sl.add(data[i].ToString(#9,'',false)) ;

  sl.SaveToFile(FileName);
  FreeAndNil(sl)
end;

class operator TObjData.Initialize({$ifdef fpc}var{$else}out{$endif} dest: TObjData);
begin
    dest.results:=TResultData.Create();
end;

class operator TObjData.Finalize(var dest: TObjData);
begin
  if assigned(dest.results) then
    FreeAndNil(dest.results)

end;


constructor THeader.Create(aName:string;const aTyp:THeaderType;const aSpan:integer;const aSize:integer);
begin
  Name:=aName;typ:=aTyp;Size:=aSize;span:=aSpan
end;

{ TTableOptions }

class operator TTotalsOptions.initialize({$ifdef fpc}var{$else}out{$endif} dest: TTotalsOptions);
begin
  dest.grandCols:=true;
  dest.grandRows:=true;
  dest.subCols:=true;
  dest.subRows:=true;
end;

function arrayComp(const a,b:Variant;const rev:boolean;const p:boolean):TValueRelationship;                 overload;
var rv:TValueRelationship; va,vb:TVarType;
begin
    //rv:=Integer(rev)*2-1;
    va:=TVarData(a).VType;vb:=TVarData(b).VType;
    if va=vb then
      if a=b then begin result:=0; exit end;
    if string(a)=arrayCompFiller then begin  result:=TCubeArr.ifthen<TValueRelationship>(p,-1,1);exit end;
    if string(b)=arrayCompFiller then begin  result:=TCubeArr.ifthen<TValueRelationship>(p,1,-1);exit end;//arrayCompFiller is larger than anything
    rv:=TCubeArr.ifthen<TValueRelationship>(rev=True,1,-1);

    if (a=b) and varisEmpty(a) then begin result:= 0;exit end;
    if varIsEmpty(a) then begin result:= 1*rv;exit end ;
    if varIsEmpty(b) then begin result:=-1*rv;exit end;
    if  (vb=va) and ((va in varsNumeric) or (va and varString=varString)) then
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
    rv:=TCubeArr.ifthen<TValueRelationship>(rev=True,1,-1);
    //rv:=Integer(rev)*2-1;
    m:=TCubeArr.Min<Integer>(High(a), High(b));
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
    m:=TCubeArr.Min<Integer>(High(a), High(b));
    for i:=0 to m do begin
      r:=arrayComp(a[i],b[i],rev[i],p);
      if r<>0 then begin result:=r;exit end;
    end;

    result:=-1;
    if Length(a)=Length(b) then begin result:= 0;exit end;
    if Length(a)<Length(b) then result:= 1
end;

class function TCubeArr.arrayLookup(const arr:array of TVariantArray;const e:TVariantArray;const reverse:array of boolean;const rp:boolean):integer;
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

class function TCubeArr.arrayLookup(const arr:array of TVariantArray;const e:TVariantArray;const reverse:boolean;const rp:boolean):integer;
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

class function TCubeArr.BinSearch<T>(const Arr: array of T; const Val: T; R: integer): integer;
var
  L, I: Integer;
  CompareRes: Integer;isFound:boolean;
begin
  isFound := false;
  result:=-1;

  // Use binary search.
  L := 0;
  R := R - 1;
  while (L<=R) do
  begin
    I := L + (R - L) shr 1;
    CompareRes := cmp<T>(Val, Arr[I]);
    if (CompareRes>0) then
      L := I+1
    else begin
      R := I-1;
      if (CompareRes=0) then begin
         isFound := true;
//         if (Duplicates<>dupAccept) then
            L := I; // forces end of while loop
      end;
    end;
  end;
  if isFound then result := L else result:=-L-1;
end;


class function TCubeArr.IfThen<T>(const cond: boolean; const whenTrue, whenFalse: T): T;
begin
  if cond then result:=whenTrue else result:=whenFalse
end;

class function TCubeArr.max<T>(const a, b: T): T;
begin
  {$ifdef fpc}
  if a>=b then result:=a else result:=b
  {$else}
  result:=ifthen<T>(TComparer<T>.Default().Compare(a,b)>0,a,b)
  {$endif}
end;

class function TCubeArr.min<T>(const a, b: T): T;
begin
  {$ifdef fpc}
  if a<=b then result:=a else result:=b
  {$else}
  result:=ifthen<T>(TComparer<T>.Default().Compare(a,b)<0,a,b)
  {$endif}
end;

class procedure TCubeArr.QuickSort<T>(var Arr: array of T; L, R : Longint; const Descending:boolean;const Compare:TComparefunc<T>);
var I,J ,neg :integer;
    P, Q :T;
begin
 //if not Assigned(Compare) then Compare:=@{$ifdef fpc}specialize{$endif}_Compare<T>;
 //if length(Arr)=0 then exit;
 neg:=ifthen<integer>(descending,-1,1);
 repeat
   I := L;
   J := R;
   P := Arr[ (L + R) div 2 ];
   repeat
     while neg*Compare(P, Arr[i]) > 0 do
       I := I + 1;
     while neg*Compare(P, Arr[J]) < 0 do
       J := J - 1;
     If I <= J then
     begin
       Q := Arr[I];
       Arr[I] := Arr[J];
       Arr[J] := Q;
       I := I + 1;
       J := J - 1;
     end;
   until I > J;
   if J - L < R - I then
   begin
     if L < J then
       QuickSort<T>(Arr, L, J, Descending, Compare);
     L := I;
   end
   else
   begin
     if I < R then
       QuickSort<T>(Arr, I, R, Descending, Compare);
     R := J;
   end;
 until L >= R;
end;

class function TCubeArr.cmp<T>(const a, b: T): integer;
begin
  {$ifdef fpc}
  result:=1;
  if a<b then result:=-1
  else if a=b then result:=0
  {$else}
  result:=TComparer<T>.Default.Compare(a,b)
  {$endif}
end;

function toString(const self:array of variant):string;
var i:integer;
begin
  for i := 0 to High(self) do
    result:=result+', '+self[i];
  if length(result)>0 then
    delete(result,1,1);
  result:='['+result+']'

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
  if length(a)>0 then m:=1 shl Length(a) else m:=TCubeArr.ifthen<integer>(mm>0,1,2);

  //m:=StrToInt('%'+TStringDynArray.fill(ifthen(length(a)>0,Length(a),ifthen(Length(colsIds>0),0,1)),1).ToString(''));
//    m:=parseInt(createFilled(a.length||(colsIds.length>0?0:1),1).join(''),2);
  setLength(n,m);
  for i:=0 to m-1 do begin
    n[i]:=TDataRecord.Fill(TCubeArr.max<Integer>(length(a),1),0);
    for j:=0 to high(n[i]) do
      if i and (1 shl j)>0 then
        n[i][j]:=v
      else
        if assigned(a) then n[i][j]:=a[j] //else n[i][j]:=v
  end ;
    //n.push(createFilled(a.length,0).join('')+i.toString(2)).slice(-a.length).split('').map(function(el,i){return (el==='1'?v:a[i])}));
  result := n
end;

function cube(const obj:TObjData;const options:TTotalsOptions):TObjData;
var
  i, j, k, rowNo, m_span:integer;
  rCube:string;
  row, col, r, c, Rw:TDataRecord;
  id:TResults;
  colStack, rowStack:array of TRecordStack;
  fieldNames :TStringDynArray;
  cub:TTableData;
  colsIds, rowsIds, filtIds, measIds:TIntegerDynArray;
  rSort, cSort: array of boolean;
//  rowAgg ,colAgg {,tmpAgg}:THeader;
  Aggs: THeaders;
  oprsIds:array of TAggregation;
  fieldTypes:array of THeaderType;
  filtRow:boolean;
 // dimensions:  TDimensions ;
//  data: TCubeData;
  //_result:TResultData;
  val:Variant;

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
  {$ifdef Profiling}  Profiler.start; {$endif}
    filtRow:=false;
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
    Insert(TDataRecord.fill(TCubeArr.ifthen<Integer>(options.grandRows,TCubeArr.ifthen<Integer>(length(rowsIds)>0,length(rowsIds),TCubeArr.ifthen<Integer>(length(colsIds)>0,0,1)),1),arrayCompFiller),result.rows,length(result.Rows)); //grand total columns
    Insert(TDataRecord.fill(TCubeArr.ifthen<Integer>(options.grandCols,length(colsIds),0),arrayCompFiller),result.cols,length(result.cols)); //grand total rows
    // add grand total row anyway (horizonal measures case)
    setLength(rSort,length(obj.dimensions.rows));for i:=0 to high(obj.dimensions.rows) do rSort[i]:=obj.descends.IndexOf(obj.dimensions.Rows[i])>=0;
    setLength(cSort,length(obj.dimensions.cols));for i:=0 to high(obj.dimensions.cols) do cSort[i]:=obj.descends.IndexOf(obj.dimensions.Cols[i])>=0;

    setLength(row,length(rowsIds));
    setLength(col,length(colsIds));
    {$ifdef Profiling} Profiler.Log('Cube initialization.');{$endif}

    for rowNo:=0 to high(obj.data) do begin
        for j:=0 to high(filtIds) do
            if obj.filters[j].data.Lookup(obj.Data[rowNo][filtIds[j]])<0 then
              begin filtRow:=true;break;end;
        if filtRow then begin filtRow:=false;continue end;                  // if filter list is inroduced then skip elements not in the filter list
        {$ifdef Profiling} Profiler.Log(0 {filter segment});{$endif}

        for i:=0 to high(rowsIds) do
          row[i]:= obj.Data[rowNo][rowsIds[i]];     //take values of selected rows into row[]
        {$ifdef Profiling} Profiler.Log(1 {row index });{$endif}

        for i:=0 to high(colsIds) do                //take values of selected columns into columns[]
          col[i]:= obj.Data[rowNo][colsIds[i]];
        {$ifdef Profiling} Profiler.Log(2 {col index });{$endif}

        // calculate subtotal rows
        for i:=TCubeArr.ifthen<Integer>(options.subRows, 1, length(rowsIds)) to length(rowsIds) do begin
          {$ifdef Profiling} Profiler.Log(3 {col index });{$endif}
            r:=copy(row,0,i).concat(TDataRecord.Fill(length(rowsIds)-i,arrayCompFiller));  // explore using TArray<FieldType> instead of TVariantArray?
            //r:=slice<TDataRecord>(row,0,i).concat(TDataRecord.Fill(length(rowsIds)-i,arrayCompFiller));  // explore using TArray<FieldType> instead of TVariantArray?
            {$ifdef Profiling} Profiler.Log(4 {col index });{$endif}
            j:=TCubeArr.arrayLookup(result.rows,r,rSort,options.br);
            if j<0 then
              insert(r,result.rows,-j-1);
              //splice<TTableData>(result.rows,-j-1,0,[r]);
           {$ifdef Profiling} Profiler.Log(5 {col index });{$endif}
        end;
        {$ifdef Profiling} Profiler.Log(6 {row Subtotal segment});{$endif}

        // calculate subtotal columns
        for i:=TCubeArr.ifthen<Integer>(options.subCols,1,length(colsIds)) to length(colsIds) do begin
            c:=copy(col,0,i).concat(TDataRecord.fill(length(colsIds)-i,arrayCompFiller));
            //c:=slice<TDataRecord>(col,0,i).concat(TDataRecord.fill(length(colsIds)-i,arrayCompFiller));
            k:=TCubeArr.arrayLookup(result.cols,c,cSort,options.bc);
            if k<0 then
              Insert(c,result.cols,-k-1);
              //splice<TTableData>(result.cols,-k-1,0,[c]);
        end;
        {$ifdef Profiling} Profiler.Log(7 {col Subtotal segment});{$endif}

        // ********************* check factorial array multiplication from here
        cub:=Dice(copy(row,0,length(rowsIds)).concat(copy(col,0,length(colsIds))),arrayCompFiller,length(colsIds));
        {$ifdef Profiling} Profiler.Log(8 {dicing segment});{$endif}

        for i:=0 to high(cub) do  begin
            rCube:=cub[i].toString(splitter,'',false);
            if not result.results.ContainsKey(rCube) then
              result.results.Add(rCube,TResults.Create(length(measIds),length(obj.data) ) );
            {$ifdef Profiling} Profiler.Log(9 {measures segment update});{$endif}
            for j:=0 to High(measIds) do begin
              //insert(obj.Data[rowNo][measIds[j]],result.results[rCube].TableData[j],length(result.results[rCube].TableData[j]));
              val:=obj.Data[rowNo][measIds[j]];
              if not varIsEmpty(val) then
                result.results[rCube].TableData[j].Push(val);
              // measIds.forEach(function(el,idx){result[rCube][idx].push(obj.data[rowNo][el])})
            {$ifdef Profiling} Profiler.Log(10 {measures segment push});
              id:=result.results[rCube];
//              Profiler.Log('['+rcube+']=>['+toString(id.TableData[j].Data)+']');

            {$endif}
            end;
        end;

    end;//end scanning table
    {$ifdef Profiling}
    Profiler.LogSegments(['Filtering','row id','col id','check subtotal','copy row','lookup & insert to row','row subtotal','col subtotal','dicing',' measures->updating',' measures->pushing','measure']);
    {$endif}
    {$ifdef fpc}
    for i:=0 to result.results.Count-1 do
      for j:=0 to high(result.results.Values{$ifdef fpc}.ToArray{$endif}[i].TableData) do
        result.results.Values{$ifdef fpc}.ToArray{$endif}[i].TableData[j].Shrink;
    {$endif}
    {$ifdef Profiling}
      Profiler.Log('Shrinking Results, scanning complete!');

    {$endif}

    if Length(result.cols)>0 then begin
        setLength(colStack,length(result.cols[0]));
        for j:=0 to high(result.cols[0]) do begin
            colStack[j].i:=0;
            colStack[j].v:=copy(result.cols[0],0,j+1)
        end;//i=0;
        for i:=0 to high(result.cols) do begin
            setLength(result.headers.columns,Length(result.cols[0]));
            for j:=0 to high(result.cols[0]) do begin
                if arrayComp(copy(result.cols[i],0,j+1),colStack[j].v,false,false)<>0 then begin
                    m_span:=i-colStack[j].i;
                    Insert(THeader.Create({name:=}colStack[j].v[j],{typ:=}htString,{span:=}m_span * length(measIds)),result.headers.columns[j],Length(result.headers.columns[j]));
                    colStack[j].i:=i;colStack[j].v:=copy(result.cols[i],0,j+1);
                end
            end;
            // column identify subtotals
            if result.cols[i].indexOf(arrayCompFiller)>-1 then
                for j:=0 to high(measIds) do
                      Insert(i*length(measIds)+j,result.colsHeadSubTotals,length(result.colsHeadSubTotals))
        end;
        for i:=0 to high(result.cols) do
          for j:=0 to high(obj.dimensions.measures) do begin
            //tmpAgg.Name:=ifthen<string>(obj.dimensions.measures[j]._Label<>'', obj.dimensions.measures[j]._Label, oprsIds[j].Name+' of '+obj.dimensions.measures[j].Field);
            //tmpAgg.typ:=Ifthen<THeaderType>(IndexOf<string>(['sum','count','distinct_count','distinct_sum','mean','average','mean','stddev','mode','median'], lowerCase(oprsIds[j].Name))>-1,htDouble,fieldTypes[measIds[j]]);
            insert(
              THeader.Create(
                TCubeArr.ifthen<string>(obj.dimensions.measures[j]._Label<>'', obj.dimensions.measures[j]._Label, oprsIds[j].Name+' of '+obj.dimensions.measures[j].Field),
                TCubeArr.Ifthen<THeaderType>(AggrStr.indexOf( lowerCase(oprsIds[j].Name))>-1,htDouble,fieldTypes[measIds[j]])
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
     for j:=0 to TCubeArr.ifthen<Integer>(length(rowsIds)>0,length(rowsIds),TCubeArr.ifthen<Integer>(length(colsIds)>0,0,1)) do begin
          setLength(rowStack,j+1);rowStack[j].i:=0; rowStack[j].v:=copy(result.rows[0],0,j+1)
     end;
     i:=0;
     setLength(result.headers.rows,length(result.rows[0]));
     for i:=0 to high(result.rows) do begin

         if result.rows[i].indexOf(arrayCompFiller)>-1 then insert(i,result.rowsHeadSubTotals,length(result.rowsHeadSubTotals));
         for j:= 0 to high(result.rows[i]) do begin
           if (j<length(rowStack)) and (arrayComp(copy(result.rows[i], 0 ,j+1),rowStack[j].v, false,false)<>0) then begin
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

              rowStack[j].i:=i;rowStack[j].v:=copy(result.rows[i],0,j+1);
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

   for i:=0 to result.results.Count-1 do begin
       for j:=0 to high(oprsIds) do begin
       if LowerCase(oprsIds[j].Name)='count' then
         result.results.Values.ToArray[i].DataRecord[j]:=length(result.results.Values.ToArray[i].TableData[j]{$ifdef fpc}.Data{$endif})//reduce<Variant>(result.results.ValueList[i].TableData[j], oprsIds[j].operation, 0)
       else if LowerCase(oprsIds[j].Name)='distinct_count' then
         result.results.Values.ToArray[i].DataRecord[j]:=Length(TVariantArray(result.results.Values.ToArray[i].TableData[j]{$ifdef fpc}.Data{$endif}).unique())//reduce<Variant>(result.results.ValueList[i].TableData[j], oprsIds[j].operation, 0)
       else if LowerCase(oprsIds[j].Name)='concat' then
         result.results.Values.ToArray[i].DataRecord[j]:=TVariantArray(result.results.Values.ToArray[i].TableData[j]{$ifdef fpc}.Data{$endif}).tostring(', ','',false)
       else if LowerCase(oprsIds[j].Name)='distinct_concat' then
         result.results.Values.ToArray[i].DataRecord[j]:=TVariantArray(result.results.Values.ToArray[i].TableData[j]{$ifdef fpc}.Data{$endif}).unique().toString(', ','',false)
       else
         result.results.Values.ToArray[i].DataRecord[j]:=TVariantArray(result.results.Values.ToArray[i].TableData[j]{$ifdef fpc}.Data{$endif}).reduce(oprsIds[j].operation)
       end;
   end;
   {$ifdef Profiling} Profiler.Log('Cube Reduction calculated');{$endif}

   for i:=0 to high(result.rows) do begin
     setLength(Rw,0);
     for j:=0 to high(result.cols) do begin
       if not result.results.TryGetValue(result.rows[i].concat(result.cols[j]).ToString(splitter,'',false),id) then
         id.DataRecord:=TDataRecord.Fill(length(measIds),'');
       for k:=0 to high(id.DataRecord) do
         insert(id.DataRecord[k],Rw,length(rw))
     end;
     insert(Rw,result.data,length(result.data));
   end;
   if length(colsIds)>0 then
     for i:=0 to high(measIds) do
       insert((TCubeArr.arrayLookup(result.cols ,TDataRecord.Fill(length(colsIds),arrayCompFiller)))*length(measIds)+i,result.colsHeadGrandTotals,length(result.colsHeadGrandTotals));
       //insert((result.cols.lookup(TDataRecord.Fill(length(colsIds),arrayCompFiller)))*length(measIds)+i,result.colsHeadGrandTotals,length(result.colsHeadGrandTotals));
   if length(rowsIds)>0 then
     Insert(TCubeArr.arrayLookup(result.rows,TDataRecord.Fill(length(rowsIds),arrayCompFiller)),result.rowsHeadGrandTotals,length(result.rowsHeadGrandTotals));
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
  result:=double(a)+double(b)
end;

function _max(const a, b: variant; const i: integer; const Arr: array of variant):variant;
begin
  result:=TCubeArr.ifthen<variant>(a>=b,a,b)
end;

function _min(const a, b: variant; const i: integer; const Arr: array of variant):variant;
begin
  result:=TCubeArr.ifthen<variant>(a<=b,a,b)
end;

function _first(const a, b: variant; const i: integer;const  Arr: array of variant):variant;
begin
  result:=a
end;

function _last(const a, b: variant; const i: integer; const Arr: array of variant):variant;
begin
  result:=b
end;

function _count(const a, b: variant; const i: integer; const Arr: array of variant):variant;
begin
  result:=TCubeArr.ifthen<variant>(varIsEmpty(a),0,a);
  if not VarIsEmpty(b) then
    Result:= Int64(Result)+1
end;

function _average(const a, b: variant; const i: integer; const Arr: array of variant):variant;
begin
   if i=high(Arr) then result:=double(a+b)/ length(Arr) else result:=a+b;
end;

function _median(const a, b: variant; const i: integer;const  Arr: array of variant):variant;
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

function _mode(const a, b: variant; const i: integer;const Arr: array of variant):Variant;
var m,j,k:integer;ar:Array of Variant;
begin
  if i<High(Arr) then exit;

  //{$ifdef fpc}
  //_QuickSort<Variant,PVariant>(@Arr[0],0,Length(Arr),false,@TVariantArray.cmp);
  //if High(Arr)>-1 then begin
  //  k:=1;
  //  j:=1;
  //  result:=Arr[0];
  //end;
  //
  //for m := 1 to High(Arr) do
  //  if result=Arr[m] then begin
  //    inc(j);
  //    if j>k then begin
  //      k:=j;
  //      result:=Arr[m]
  //    end
  //  end else begin
  //    j:=1
  //  end;
  //{$else}
  SetLength(Ar,Length(Arr));
  Move(Arr[0],Ar[0],SizeOf(Variant)*Length(Arr));
  {$ifdef fpc}TVariantArray(Ar).Sort{$else}TArray.Sort<Variant>(Ar){$endif};
  if High(Ar)>-1 then begin
    k:=1;
    j:=1;
    result:=Ar[0];
  end;

  for m := 1 to High(Ar) do
    if result=Ar[m] then begin
      inc(j);
      if j>k then begin
        k:=j;
        result:=Ar[m]
      end
    end else begin
      j:=1
    end;
  //{$endif}
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


{ TVariantArrayHelper }

function TVariantArrayHelper.Concat(const Arr: TSelf): TSelf;
begin
  result:={$ifdef fpc}Self+Arr{$else}TArray.Concat<Variant>([Self,Arr]){$endif}
end;

class function TVariantArrayHelper.Fill(ACount: integer; const AValue: Variant): TSelf;
var
  i: Integer;
begin
 SetLength(Result,ACount);
 for i := 0 to High(Result) do
   Result[i]:=AValue
end;

function TVariantArrayHelper.Filter(const func: TFilterFunc): TSelf;
var i,j,C:integer;
begin
  C:=Length(Self);
  j:=0;
  setLength(result,C);
  for i:=0 to C-1 do if func(Self[i],i,Self) then
    begin
      Result[j]:=Self[i];
      inc(j)
    end;
  setLength(result,j)
end;

function TVariantArrayHelper.IndexOf(const AVal: Variant): integer;
var i:integer;
begin
  result:=-1;
  for i:=0 to High(Self) do
    if TVarData(Self[i]).vType=TVarData(AVal).vType then
      if Self[i]=AVal then begin
        result:=i;
        exit
      end;
end;

function TVariantArrayHelper.Reduce(const func: TReduceCallback<Variant>): Variant;
var i:integer;
begin
  VarClear(Result);
  if High(Self)>-1 then
    result:=Self[0];
  for i:=1 to high(self) do
    result:=func(result,Self[i],i,Self)
end;

function TVariantArrayHelper.Sort: TSelf;
begin
  {$ifdef fpc}
  TCubeArr.QuickSort<Variant>(Self, 0, High(Self), false, compare)
  {$else}TArray.Sort<Variant>(Self){$endif};
  Result:=Self
end;

function TVariantArrayHelper.ToString(const Seperator: string;  const quote: string; const Brackets: boolean): string;
var i:integer;
begin
  result:='';
  if length(Self)>0 then begin
    for i:=0 to High(Self) do
      result:=Result+Seperator+quote+string(Self[i])+quote ;
    delete(result,1,Length(Seperator));
  end;
  if Brackets then result:='['+result+']';
end;

function TVariantArrayHelper.Unique: TSelf;
var i:integer;
begin
  if not Assigned(Self) then exit;// early exit
  setLength(result,0);
  {$ifdef fpc}Sort(){$else}TArray.Sort<Variant>(Self){$endif};
  if High(Self)>-1 then Insert(Self[0],Result,length(Result));

  for I := 1 to High(Self) do
  if Self[i]<>Self[i-1] then
    Insert(Self[i],Result,length(Result))

end;

function TVariantArrayHelper.Push(const AValue: Variant): Integer;
begin
  Insert(AValue,Self,Length(Self));
  result:=High(Self);
end;

class function TVariantArrayHelper.uniqueFilt(const a: TType; const i: integer; arr: TSelf): boolean;
begin
  result:=true;
  if i>0 then
    result:=a<>arr[i-1];
end;

class function TVariantArrayHelper.Compare(const a, b: Variant): integer;
begin
  result:=1;
  if a<b then result:=-1
  else if a=b then result:=0
end;


{ TStringArrayHelper }

function TStringArrayHelper.IndexOf(const str: string): integer;
var i:integer;
begin
  result:=-1;
  for i:=0 to High(Self) do
    if Self[i]=str then begin
      result:=i;
      exit
    end;
end;

function TStringArrayHelper.Lookup(const str: string; const CaseSensitive: boolean):{$ifdef fpc}int64{$else} integer{$endif};
begin

   if not {$ifdef fpc}
     (TArrayHelper<string>.BinarySearch(Self,str,result))
     {$else}
     TArray.BinarySearch<string>(Self,str,result)
     {$endif} then
     result:=-1
end;

var i:integer;
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
  for i:=0 to high(AggregationArray) do
    Insert(AggregationArray[i].Name,AggrStr,length(AggrStr))
end.

