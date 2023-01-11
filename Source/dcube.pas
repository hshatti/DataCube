unit dcube;
{$ifdef fpc}
  {$mode delphi}
  {$ModeSwitch advancedrecords}
  {$Macro on}
  {$ModeSwitch typehelpers}
{$else}
  {$POINTERMATH ON}
{$endif}

interface

uses
   {$ifdef fpc}LCLType, {$endif}
   Types, Classes, SysUtils, DB
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

//        HeaderTypeStr:array[0..12] of string= ('Unknowen', 'String', 'Boolean', 'Byte', 'Word', 'Integer', 'Int64', 'Single', 'Double', 'Currency', 'DateTime', 'SingleComplex', 'DoubleComplex');

type

{$ifndef fpc} SizeInt = {$ifdef CPU64BITS} int64{$else}longint{$endif} ; PSizeInt = ^SizeInt;{$endif}
  TFilterCallback<T,PT>=function (const a:T;const i:Integer; Arr:PT):boolean of object;

  TCompareFunc<T>=function (const a,b:T):integer ;

  THeaderType=TVarType;//( htUnknowen, htString, htBoolean, htByte, htWord, htInteger, htInt64, htSingle, htDouble, htCurrency, htDateTime, htSingleComplex, htDoubleComplex);

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
    procedure Clear;
    function toString():string;

  end;

  { TResultHeaders }

  TResultHeaders=record
    Columns,Rows:Array of THeaders;
    procedure Clear;
  end;



  TStringArrayHelper = record helper for TStringDynArray
    function IndexOf(const str:string):integer;
    function Lookup(const str:string;const CaseSensitive:boolean=true):{$ifdef fpc}SizeInt{$else}integer{$endif};
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
    function Reduce(const func:TReduceCallback<Variant>):Variant;
    function Lookup(const aValue:Variant):Integer;
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
    typ: THeaderType;  // result;
    operation:TMeasureFunction
  end;

  { TMeasure }

  TMeasure=record
    _Label, Field, Formula:string;
    typ:THeaderType;
    Decimals:word;
    Operation:TAggregation;
    constructor Create(const aField: string; const aOperation: TAggregation;
      const aLabel: string=''; const aTyp: THeaderType=varEmpty; const aDecimals: shortint=2);

  end;



  { TDataRecord }

  TDataRecord = TVariantArray;
  TTableData = TVariantArrayArray;

  { TDataRecord2 }
  {$ifdef TABLEDATA2}
  TDataRecord2<T>=record                 // allocated heap data for cacheing
  private
    function GetCurrent: T;
    function GetItem(index: SizeInt): T;
    procedure SetItem( index: SizeInt; const AValue: T);
  public
    Data:TArray<T>;
    Indicator:integer;
    function Count:integer;inline;
    property Current:T read GetCurrent;
    function Push(const AValue:T):SizeInt;
    procedure Concat(const src:TDataRecord2<T>);
    function reduce(const func: TReduceCallback<T>): T;       overload;
    class function reduce(const self:TArray<T>;const func:TReduceCallback<T>):T;  overload;static;
    procedure Shrink;
    constructor Create(const ACapacity:SizeInt);
    property Item[index:SizeInt]:T read GetItem write SetItem ;default;
    class operator initialize({$ifdef fpc}var{$else}out{$endif} dest:TDataRecord2<T>);
  end;

  TTableData2<T> = array of TDataRecord2<T>;
  {$endif}
  TRecordStack=record
    i:integer;
    v:TDataRecord;
  end;

  TDimensions=record
    Cols,Rows:TStringDynArray;
    Measures:array of TMeasure
  end;


  TFilter=record
    FieldName:string;
    Data:TVariantArray
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

//const defaultTotalsOptions:TTotalsOptions = (bc:false; br:false; grandCols:true; grandRows:true; subCols:true; subRows:true);

type

  { TResults }
  TResults=record
    TableData:{$ifdef TABLEDATA2}TTableData2<Variant>{$else}TTableData{$endif};
    ReducedMeasures:TDataRecord;
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

  PResultData = ^TResultData;
  TResultData=
  {$ifdef fpc}
     TDictionary<string,TResults>
  {$else}
      TDictionary<string,TResults>
  {$endif}



//  {$else}
//  TSortedKeyValueList<string,TResults>
//  {$endif}
  ;
  PDimensionSet = ^TDimensionSet;
  TDimensionSet=TDictionary<string,TDataRecord>;
  TDiceRollups = TDictionary<string,TTableData>;

  { TObjData }
  PObjData=^TObjData;


  { TCubeThread }

  TCubeThread = class (TThread)
    Id:integer;
    dictResults:TResultData;
    dictCols,dictRows:TDimensionSet;
    ObjData:PObjData;
    position, Count:SizeInt;
    constructor Create(const obj:PObjData;const aId:integer;const aPosition,aCount:SizeInt;const cols,rows:TDimensionSet;const res:TResultData);overload;
    procedure Execute;override;
    class procedure PrepareHeaders(const obj: PObjData; const aId: integer;
      const aPosition, aCount: SizeInt; const aCols, aRows: TDimensionSet);overload;static;
    class procedure ScanTable(const obj: PObjData; const aId: integer;
      const aPosition, aCount: SizeInt; const res: TResultData);overload;static;
    procedure PrepareHeaders; overload;
    procedure ScanTable;      overload;
  end;

  TObjData=record
  type
    TSizeIntArray = array of SizeInt;
    TParams = array of Pointer;
  var
    headers:TResultHeaders;
    data:TTableData;
    dimensions:TDimensions;
    filters:TFilters;
    descends:TStringDynArray;
    cols,rows:TDimensionSet;
    results:TResultData;
    totalsOptions:TTotalsOptions;
    resultCube:PObjData;
    procedure LoadFromFile(const FileName:TFileName);
    procedure SaveToFile(const FileName:TFileName);
    class operator Initialize({$ifdef fpc}var{$else}out{$endif} dest:TObjData);
    class operator Finalize(var dest:TObjData);
  private
    colsHeadSubTotals, colsHeadGrandTotals,rowsHeadSubTotals, rowsHeadGrandTotals:TIntegerDynArray;
    colsIds, rowsIds, filtIds, measIds:TIntegerDynArray;
    threadList:array of TCubeThread;
    _cols,_rows:TArray<TDimensionSet>;
    _results:TArray<TResultData>;
    function isFiltered(const checkRow: integer): boolean;
//    class procedure PrepareHeaders(aData: Pointer); static;
//    class procedure ScanTable(AData: pointer);static;
    class procedure dictResultsResolver(var dest:TResults;const source:TResults);static;
  public
    function cube(): TObjData; overload;
  end;



  TOLAPNotifyEvent<T> = procedure (Sender:T) of object;

  { TOLAPDimension }

  TOLAPDimension=class
  type
    TOLAPDimensionChange = TOLAPNotifyEvent<TOLAPDimension>;
  private
    FFieldNames:TStringDynArray;
    FOnChange: TOLAPDimensionChange;
    function GetFieldNames(Index: integer): string;
    procedure SetFieldNames(Index: integer; AValue: string);   overload;

    procedure SetOnChange(AValue: TOLAPDimensionChange);
  public
    function Add(const FieldName:string):integer;
    procedure Clear;
    function Count:integer;
    property FieldNames[Index:integer]:string read GetFieldNames write SetFieldNames ;default;
    procedure SetFieldNames(AValue: TStringDynArray);  overload;
    function IndexOf(FieldName:string):integer;
    property OnChange:TOLAPDimensionChange read FOnChange write SetOnChange;
    procedure Remove(const Index:integer);
  end;

  { TOLAPDimensions }

  TOLAPDimensions=class    // used only in TOLAP
  type TOLAPDimensionsChange = procedure (Sender :TOLAPDimensions; Dimension: TOLAPDimension; isRow:boolean) of object;

  private
    FCols,FRows:TOLAPDimension;
    FOnChange: TOLAPDimensionsChange;
    procedure SetOnChange(AValue: TOLAPDimensionsChange);
    procedure DoColChange(AValue: TOLAPDimension);
    procedure DoRowChange(AValue: TOLAPDimension);
  public
    constructor Create;
    procedure Clear;
    property OnChange: TOLAPDimensionsChange read FOnChange write SetOnChange;
    destructor Destroy; override;
  end;

  { TOLAPMeasures }

  TOLAPMeasures=class    // used only in TOLAP
  type
    TOLAPMeasureChange = TOLAPNotifyEvent<TOLAPMeasures>;
  private
    FMeasures:TArray<TMeasure>;
    FOnChange: TOLAPMeasureChange;
    function GetMeasures(Index: integer): TMeasure;
    procedure SetMeasures(Index: integer; AValue: TMeasure);
    procedure SetOnChange(AValue: TOLAPMeasureChange);
  public
    function Add(aMeasure:TMeasure):integer;
    procedure Clear;
    function Count:integer;

    property Measures[Index:integer]:TMeasure read GetMeasures write SetMeasures ;default;
    function IndexOf(FieldName:string):integer;
    property OnChange:TOLAPMeasureChange  read FOnChange write SetOnChange;
    procedure Remove(const Index:integer);
  end;

  { TOLAP }

  TOLAP = class(TComponent)
  private // used for processing

  private // used for results;
    FActive: boolean;
    FDataset:TDataset;
    FDimensions: TOLAPDimensions;
    FMeasures:TOLAPMeasures;
    FFilters:TFilters;
    FHeaders:TResultHeaders;
    FFieldTypes : TArray<TVarType>;
    FRowsIds, FColsIds, FMeasIds, FFiltIds:TIntegerDynArray;
    FOprsIds:TArray<TAggregation>;
    FRSort,FCSort:TArray<boolean>;
    FDescends:TStringDynArray;
    FCols,FRows:TDimensionSet;
    _Cols,_Rows:TTableData;
    FOptions: TTotalsOptions;
    FOnFilterChange: TOLAPNotifyEvent<TOLAP>;
    FColsHeadSubTotals,FColsHeadGrandTotals,FRowsHeadSubTotals, FRowsHeadGrandTotals:TIntegerDynArray;
    FData:TTableData;
    FOnDimnsionsChanged: TOLAPDimensions.TOLAPDimensionsChange;
    FOnMeasuersChanged: TOLAPNotifyEvent<TOLAPMeasures>;
    FResults:TResultData;
    procedure SetActive(AValue: boolean);
    procedure SetDataset(AValue: TDataset);
    procedure SetFilters(AValue: TFilters);
    procedure SetOnDimnsionsChanged(AValue: TOLAPDimensions.TOLAPDimensionsChange);
    procedure SetOnFilterChange(AValue: TOLAPNotifyEvent<TOLAP>);
    procedure SetOnMeasuersChanged(AValue: TOLAPNotifyEvent<TOLAPMeasures>);
    procedure InitAllocations;
    procedure PrepareHeaders;
    procedure ScanDataset;
    procedure ApplyHeaders;
    procedure ExecuteReduction;
    procedure FillData;
    procedure FillGrandTotals;
    procedure CalculateFormulas;
    function isFiltered:boolean;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    property Active:boolean read FActive write SetActive;
    property Data:TTableData read FData;
    property Dataset:TDataset read FDataset write SetDataset;
    property Dimensions:TOLAPDimensions read FDimensions; // no setter allowed
    property Filters:TFilters read FFilters write SetFilters;
    property Headers:TResultHeaders read FHeaders;
    property Measures:TOLAPMeasures read FMeasures;      // no setter allowed
    property Options:TTotalsOptions read FOptions;
    //    property Results:TResultData read FResults;
    property OnDimnsionsChanged:TOLAPDimensions.TOLAPDimensionsChange read FOnDimnsionsChanged write SetOnDimnsionsChanged;
    property OnFilterChange:TOLAPNotifyEvent<TOLAP> read FOnFilterChange write SetOnFilterChange;
    property OnMeasuersChanged:TOLAPNotifyEvent<TOLAPMeasures> read FOnMeasuersChanged write SetOnMeasuersChanged;


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


  //function cube(const obj:TObjData;const options:TTotalsOptions):TObjData; overload;
  //function cube(const obj:TObjData):TObjData; overload;

  TDictCollisionResolver<Value> = procedure(var dest:Value;const source:Value);

  { TServiceTools }

  TServiceTools=class
    class function min<T>(const a,b:T):T; static;
    class function max<T>(const a,b:T):T; static;
    class function arrayLookup(const arr:array of TVariantArray;const e:TVariantArray;const reverse:array of boolean;const rp:boolean):integer;   overload;
    class function arrayLookup(const arr:array of TVariantArray;const e:TVariantArray;const reverse:boolean=false;const rp:boolean=false):integer;   overload;
    class procedure arraySort(var arr:array of TVariantArray; L, R:Longint;const aDescendings:array of boolean;const rb:boolean=false);
    class function arrayUnique(const arr:array of TVariantArray):TArray<TVariantArray>;
    class function IndexOf<T>(const arr:array of T;const Val:T):integer;
    class function SparseValues<T>(const arr:array of T;const Indecies:array of Integer):TArray<T>;
    class function SparseIndecies<T>(const arr:array of T;const AValues:array of T):TArray<integer>;
    class function IfThen<T>(const cond:boolean;const whenTrue,whenFalse:T):T; static;
    class procedure QuickSort<T>(var Arr: array of T; L, R: Longint; const Descending: boolean; Compare: TComparefunc<T>);static;
    class function BinSearch<T>(const Arr:array of T;const Val:T; R:integer):integer;
    class function cmp<T>(const a, b: T): integer;
    class function TransposeData(const ATable: TTabledata): TTableData;
    class procedure mergeDictionaries<Key,Value>(const inDics:TArray<TDictionary<Key,Value>>;
      var outDic:TDictionary<Key,Value>;const resolver:TDictCollisionResolver<Value>);
  end;

//function _sum(const a, b: variant; const i: integer; const Arr: array of variant):variant;
//function _max(const a, b: variant; const i: integer; const Arr: array of variant):variant;
//function _min(const a, b: variant; const i: integer; const Arr: array of variant):variant;
//function _first(const a, b: variant; const i: integer;const  Arr: array of variant):variant;
//function _last(const a, b: variant; const i: integer; const Arr: array of variant):variant;
//function _count(const a, b: variant; const i: integer; const Arr: array of variant):variant;
//function _average(const a, b: variant; const i: integer; const Arr: array of variant):variant;
//function _median(const a, b: variant; const i: integer;const  Arr: array of variant):variant;
//function _mode(const a, b: variant; const i: integer;const Arr: array of variant):Variant;
//function _stddev(const a, b: variant; const i: integer; const Arr: array of variant):Variant;  function Eval(str:string):double;
  //function comp<T>(const a,b:T;const rev:boolean):integer;            overload;
  //function aComp<T>(const a,b:T;const rev:boolean):integer;           overload;
  //
  //function Lookup<T>(const Ar:array of T;const e:T;const reverse:boolean =false ;const p:boolean=false):integer;   overload;
  //function Lookup<T>(const Ar:array of T;const e:T;const reverse:array of boolean;const p:boolean=false):integer;   overload;
//  TIfthen = function (val:boolean;const iftrue:integer; const iffalse:integer) :integer ;
//var
//  ifthen :TIfthen;

var
  AggregationArray:array of TAggregation ;
  AggrStr :TStringDynArray;
  defaultTotalsOptions:TTotalsOptions ;
  function Dice(const a:TDataRecord;const v:Variant;const aSize:integer):TArray<TDataRecord>;
  (*
  function cube(const obj:TObjData;const options:TTotalsOptions):TObjData;    overload;
  function cube(const obj:TObjData):TObjData;                                 overload;
  *)
  function arrayComp(const a,b:Variant;const rev:boolean;const p:boolean):TValueRelationship;                   overload;
  function arrayComp(const a,b:TVariantArray;const rev:boolean;const p:boolean):TValueRelationship;             overload;
  function arrayComp(const a,b:TVariantArray;const rev:array of boolean;const p:boolean):TValueRelationship;    overload;

implementation

const splitter=#9;
      arrayCompFiller=#$ff+'Total';

      defaultButtonHeight=40;
      defaultButtonWidth =200;


const oprChars=['*','/','+','-'];

function HeaderTypeStr(const typ:TVarType):string;
// switch {the classic way }
begin
  case typ and varTypeMask of
    varEmpty      : result:='Empty'    ;
    varNull       : result:='Null'     ;
    varSmallInt   : result:='SmallInt' ;
    varInteger    : result:='Integer'  ;
    {$ifndef FPUNONE}
    varSingle     : result:='Single'   ;
    varDouble     : result:='Double'   ;
    varDate       : result:='Date'     ;
    {$endif}
    varCurrency   : result:='Currency' ;
    varOleStr     : result:='OleStr'   ;
    varDispatch   : result:='Dispatch' ;
    varError      : result:='Error'    ;
    varBoolean    : result:='Boolean'  ;
    varVariant    : result:='Variant'  ;
    varUnknown    : result:='Unknowen' ;
    //varDecimal    : result:='Decimal'  ;
    varShortInt   : result:='ShortInt' ;
    varByte       : result:='Byte'     ;
    varWord       : result:='Word'     ;
    varLongWord   : result:='LongWord' ;
    varInt64      : result:='Int64'    ;
    varUInt64     : result:='UInt64'    ;
    varRecord     : result:='Record'   ;
    varString     : result:='String'   ;
    varAny        : result:='Any'      ;
    varUString    : result:='UString'  ;
  end;
  if typ and varArray>0 then result:='Array('+result+')';
  if typ and varByRef>0 then result:='POINTER TO '+result;
end;

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
  while i<length(oprs) do begin     // BODMAS prioritises divisions
    if oprs[i]='/' then begin
      nums[i]:=nums[i]/nums[i+1];
      delete(oprs,i,1);
      delete(nums,i+1,1)
      //dec(len)
    end else inc(i);
  end;

  i:=0;
  while i<length(oprs) do begin     // then Multiplications
    if oprs[i]='*' then begin
      nums[i]:=nums[i]*nums[i+1];
      delete(oprs,i,1);
      delete(nums,i+1,1)
      //dec(len)
    end else inc(i);
  end;

  i:=0;
  while i<length(oprs) do begin    // then add and subtract
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

{$ifdef TABLEDATA2}
{ TDataRecord2 }

function TDataRecord2<T>.GetCurrent: T;
begin
  result:=Data[Indicator]
end;


function TDataRecord2<T>.GetItem( index: SizeInt): T;
begin
  result:=Data[Index]
end;

procedure TDataRecord2<T>.SetItem( index: SizeInt; const AValue: T);
begin
  Data[index]:=AValue
end;

function TDataRecord2<T>.Count: integer;
begin
  result:=Indicator+1
end;

function TDataRecord2<T>.Push(const AValue: T): SizeInt;
begin
  inc(Indicator);
  if Indicator>High(Data) then begin
    setLength(Data,abs(Indicator)*2)
  end;
  Data[indicator]:=AValue;
  result:=Indicator;
  //WriteLn(AValue);

end;

procedure TDataRecord2<T>.Concat(const src: TDataRecord2<T>);
var i, newSize:SizeInt;
begin
  newSize:=Count+src.Count;
  if newSize>High(Data) then
    setLength(Data,newSize);
  {$ifdef fpc}
  move(src.Data[0],Data[Count],src.Count*SizeOf(T));
  {$else}
//  moveArray(@Data[Count], @src.Data[0], TypeInfo(Variant),src.Count) ;    // buggy fast move in delphi, using PurePascal
  for i:=0 to high(src.Data) do
    Data[Count+i]:=src.Data[i];
  {$endif}
  inc(Indicator,src.Count)
end;

function TDataRecord2<T>.reduce(const func: TReduceCallback<T>): T;
begin
  result:=Reduce(data,func);
end;

procedure TDataRecord2<T>.Shrink;
begin
  setLength(Data,Indicator{.$ifdef fpc}+1{.$endif}) // TODO : quick workaround for weird behavior in delphi only - adding one to the final array
end;

constructor TDataRecord2<T>.Create(const ACapacity: SizeInt);
begin
  setLength(Data,ACapacity)
end;

class operator TDataRecord2<T>.initialize({$ifdef fpc}var{$else}out{$endif} dest: TDataRecord2<T>);
begin
  dest.Indicator:=-1;
end;

class function TDataRecord2<T>.reduce(const self: TArray<T>; const func: TReduceCallback<T>): T;
var i,l:SizeInt;
begin
  if High(Self)>-1 then
    result:=Self[0];
  for i:=1 to high(self) do
    result:=func(result,Self[i],i,Self)
end;

{ TCubeThread }

constructor TCubeThread.Create(const obj: PObjData; const aId: integer;
  const aPosition, aCount: SizeInt; const cols, rows: TDimensionSet;
  const res: TResultData);
begin
  ObjData:=Obj;
  Id:=aId;
  Position:=aPosition;
  Count:=aCount;
  dictRows:=rows;
  dictCols:=cols;
  dictResults:=res;
  inherited Create(false);
end;

procedure TCubeThread.Execute;
begin
  if Assigned(dictResults) then
    ScanTable
  else
    PrepareHeaders;
end;

class procedure TCubeThread.PrepareHeaders(const obj: PObjData;
  const aId: integer; const aPosition, aCount: SizeInt; const aCols,
  aRows: TDimensionSet);
var
  rowNo,i:SizeInt;
  row, col, ro, co:TDataRecord;
begin
  //writeln('PrepareHeaders Entring Thread [',aId,'] from [',aPosition,' count [',aCount,']');
  setLength(row,length(Obj.rowsIds));
  setLength(col,length(Obj.colsIds));
  with Obj^ do
  for rowNo:=aposition to aPosition+aCount-1 do begin
      if isFiltered(rowNo) then continue;
      for i:=0 to high(rowsIds) do
        row[i]:= Data[rowNo][rowsIds[i]];     //take values of selected rows into row[]
      for i:=0 to high(colsIds) do                //take values of selected columns into columns[]
        col[i]:= Data[rowNo][colsIds[i]];

      // calculate unique and subtotal rows
      // ToDo -cPerformance : explore using TArray<FieldType> instead of TVariantArray?
      for i:=TServiceTools.ifthen<Integer>(totalsOptions.subRows, 1, length(rowsIds)) to length(rowsIds) do begin
         {$ifdef Profiling2} Profiler.Log(0); {$endif}
          ro:=copy(row,0,i).concat(TDataRecord.Fill(length(rowsIds)-i,arrayCompFiller));
          {$ifdef Profiling2} Profiler.Log(1); {$endif}
          aRows.tryAdd(ro.toString(splitter),ro);
          {$ifdef Profiling2} Profiler.Log(2); {$endif}
      end;
      // calculate unique subtotal columns
      for i:=TServiceTools.ifthen<Integer>(totalsOptions.subCols,1,length(colsIds)) to length(colsIds) do begin
          {$ifdef Profiling2} Profiler.Log(0); {$endif}
          co:=copy(col,0,i).concat(TDataRecord.fill(length(colsIds)-i,arrayCompFiller));
          {$ifdef Profiling2} Profiler.Log(1); {$endif}
          aCols.tryAdd(co.toString(splitter),co);
          {$ifdef Profiling2} Profiler.Log(2); {$endif}
      end;
  end;
  //writeln('PrepareHeaders Exiting Thread [',aid,']');
end;

class procedure TCubeThread.ScanTable(const obj: PObjData; const aId:integer;
  const aPosition, aCount: SizeInt; const res: TResultData);
var
  rowNo,i,j,k:SizeInt;
  {$ifdef TABLEDATA2}resultValues:TArray<TResults>;{$endif}
  row, col, Rw:TDataRecord;
  initSize:SizeInt;
  rCube:string;
  cub:TTableData;
  rs:TResults;
  hashFound:boolean;
  val:Variant;
begin
  //writeln('ScanTable Entring Thread [',aId,']');
  if assigned(Obj.rowsIds) then
    setLength(row,length(Obj.rowsIds));
  if assigned(Obj.colsIds) then
    setLength(col,length(Obj.colsIds));

  with Obj^ do  begin
    initSize:=aCount div (resultCube.Cols.count * resultCube.Rows.count);
    inc(initSize); //avoid zero size allocation
    for rowNo:=aPosition to aPosition+aCount-1 do begin
      if isFiltered(rowNo) then continue;
      for i:=0 to high(rowsIds) do
        row[i]:= Data[rowNo][rowsIds[i]];     //take values of selected rows into row[]
      for i:=0 to high(colsIds) do                //take values of selected columns into columns[]
        col[i]:= Data[rowNo][colsIds[i]];

      // ********************* check factorial array multiplication from here
      {$ifdef Profiling2} Profiler.Log(0); {$endif}
      if Assigned(row) or assigned(col) then  begin
        rw:=copy(row,0,length(rowsIds)).concat(copy(col,0,length(colsIds)));
        k:=1 shl Length(rw)
      end else
        k:=TServiceTools.ifthen<integer>(length(colsIds)>0,1,2);
      {$ifdef Profiling2} Profiler.Log(1); {$endif}
      cub:=Dice(rw, arrayCompFiller, k);
      {$ifdef Profiling2} Profiler.Log(2); {$endif}
      for i:=0 to high(cub) do  begin
        rCube:=cub[i].toString(splitter,'',false);
        {$ifdef Profiling2} Profiler.Log(3); {$endif}
        hashFound:=res.tryGetValue(rCube,rs);
        {$ifdef Profiling2} Profiler.Log(4); {$endif}
        if not hashFound then begin
          rs:=TResults.Create(length(measIds), initSize );
          //setLength(rs.TableData,length(measIds));
          //setLength(rs.ReducedMeasures,length(measIds));
          //for j:=0 to high(rs.TableData) do
          //   setLength(rs.TableData[j].Data,initSize);
          res.Add(rCube,rs);
        end;
        {$ifdef Profiling2} Profiler.Log(5); {$endif}
        { ToDo -cPerformance : if the same measure column is added again consider referencing
          the added one inseated of pushing a new measure  }
        for j:=0 to High(measIds) do begin
          val:=Data[rowNo][measIds[j]];
          if not varIsEmpty(val) then
            {$ifdef TABLEDATA2}
            rs.TableData[j].Push(val)
            {$else}
            insert(val,id.TableData[j],length(id.TableData[j]))
            {$endif}
        end;
        {$ifdef Profiling2} Profiler.Log(6); {$endif}
      end;
    end;
  end;//end scanning table
  {$ifdef TABLEDATA2}
  resultValues:=res.values.toArray;
  for i:=0 to high(resultValues) do
    for j:=0 to high(resultValues[i].TableData) do
      resultValues[i].TableData[j].Shrink;
  {$ifdef Profiling2}
  Profiler.Log('Shrinking...');
  {$endif}
  {$endif}
  //writeln('ScanTable Exiting Thread [',aId,']');
end;

procedure TCubeThread.PrepareHeaders;
begin
  PrepareHeaders(ObjData, Id, Position, Count, dictCols, dictRows);
end;  // 9.7 seconds on {Rows':Sector', Columns:'Provider', Measure : Sum('Price')}

procedure TCubeThread.ScanTable;
begin
  ScanTable(ObjData, Id, Position, Count, dictResults);
end;

{$endif}
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

procedure THeadersHelper.Clear;
begin
  Self:=nil //  set self array out of scope
end;

function THeadersHelper.toString(): string;
var i:integer;
begin
  result:='';
  for i:=0 to High(Self) do
    result:=result+format('(Name: %s, Span: %d, size: %d, type:%s), ',[Self[i].Name, Self[i].Span, self[i].size, HeaderTypeStr(ord(Self[i].typ))])+sLineBreak;

  result:='['+result+']'
end;

{ TResults }

constructor TResults.Create(const MeasureCount,RowCount:integer);
var i:integer;
begin
  setLength(TableData,MeasureCount);
  {$ifdef TABLEDATA2}
  for i:=0 to high(TableData) do
    TableData[i]:=TDataRecord2<Variant>.Create(RowCount);
  {$endif}
  setLength(ReducedMeasures,MeasureCount);
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
  Decimals:=aDecimals
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
      else if (LowerCase(ExtractFileExt(FileName))= '.tab') or (LowerCase(ExtractFileExt(FileName))='.tsv') then
        sp:=sl[0].split([#9]);
      setLength(Headers.columns[0],length(sp));
      for i:=0 to High(sp) do with Headers.columns[0][i] do begin
        Name:=sp[i];
        span:=1;
        typ:=varEmpty;
      end
    end;
  setLength(Data,SL.Count-1);
  if LowerCase(ExtractFileExt(FileName))='.csv' then
    for i:=1 to sl.Count-1 do
      Data[i]:=StringsToDataRec(sl[i].Split([','],'"'))
  else if (LowerCase(ExtractFileExt(FileName))= '.tab') or (LowerCase(ExtractFileExt(FileName))='.tsv') then
    for i:=1 to sl.Count-1 do
      Data[i]:=StringsToDataRec(sl[i].Split([#9]));
  FreeAndNil(sl);
end;

procedure TObjData.SaveToFile(const FileName: TFileName);
var i,j:Integer;sp:TStringArray;
    fText:TextFile;
begin
  AssignFile(fText,FileName);
  try
    Rewrite(fText);
    for i:=0 to High(Headers.columns) do begin
      setLength(sp,Length(Headers.columns[i]));
      for j:=0 to High(sp) do  sp[j]:=Headers.columns[i][j].Name;
      if LowerCase(ExtractFileExt(FileName))='.csv' then
        WriteLn(fText,string.Join(',',sp))
      else if (LowerCase(ExtractFileExt(FileName))= '.tab') or (LowerCase(ExtractFileExt(FileName))='.tsv') then
        writeLn(fText,string.Join(#9,sp))
    end;
    if LowerCase(ExtractFileExt(FileName))='.csv' then
      for i:=0 to High(Data) do
        Writeln(fText,Self.Data[i].ToString(',','',false))
    else if (LowerCase(ExtractFileExt(FileName))= '.tab') or (LowerCase(ExtractFileExt(FileName))='.tsv') then
      for i:=0 to High(Data) do
        WriteLn(fText,data[i].ToString(#9,'',false)) ;
  finally
    CloseFile(fText)
  end
end;

class operator TObjData.Initialize({$ifdef fpc}var{$else}out{$endif} dest: TObjData);
begin
    dest.results:=TResultData.Create();
    dest.Cols:=TDimensionSet.Create;
    dest.Rows:=TDimensionSet.Create;

end;

class operator TObjData.Finalize(var dest: TObjData);
begin
  if assigned(dest.results) then
    FreeAndNil(dest.results);
  FreeAndNil(dest.Cols);
  FreeAndNil(dest.Rows);

end;


constructor THeader.Create(aName:string;const aTyp:THeaderType;const aSpan:integer;const aSize:integer);
begin
  Name:=aName;typ:=aTyp;Size:=aSize;span:=aSpan
end;

{ TTableOptions }

class operator TTotalsOptions.initialize({$ifdef fpc}var{$else}out{$endif} dest: TTotalsOptions);
begin
  dest.br:=false;
  dest.bc:=false;
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
    if string(a)=arrayCompFiller then begin  result:=TServiceTools.ifthen<TValueRelationship>(p,-1,1);exit end;
    if string(b)=arrayCompFiller then begin  result:=TServiceTools.ifthen<TValueRelationship>(p,1,-1);exit end;//arrayCompFiller is larger than anything
    rv:=TServiceTools.ifthen<TValueRelationship>(rev=True,1,-1);

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
    rv:=TServiceTools.ifthen<TValueRelationship>(rev=True,1,-1);
    //rv:=Integer(rev)*2-1;
    m:=TServiceTools.Min<Integer>(High(a), High(b));
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
    m:=TServiceTools.Min<Integer>(High(a), High(b));
    for i:=0 to m do begin
      r:=arrayComp(a[i],b[i],rev[i],p);
      if r<>0 then begin
        result:=r;
        exit
      end;
    end;

    result:=-1;
    if Length(a)=Length(b) then begin result:= 0;exit end;
    if Length(a)<Length(b) then result:= 1
end;

class function TServiceTools.arrayLookup(const arr:array of TVariantArray;const e:TVariantArray;const reverse:array of boolean;const rp:boolean):integer;
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

class function TServiceTools.arrayLookup(const arr:array of TVariantArray;const e:TVariantArray;const reverse:boolean;const rp:boolean):integer;
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

class procedure TServiceTools.arraySort(var arr: array of TVariantArray; L,
  R: Longint; const aDescendings: array of boolean; const rb: boolean);
var I,J :integer;
    P, Q :TVariantArray;
begin
 //if not Assigned(Compare) then Compare:=@{$ifdef fpc}specialize{$endif}_Compare<T>;
 if length(Arr)<2 then exit;
 repeat
   I := L;
   J := R;
   P := Arr[ (L + R) div 2 ];
   repeat
     while arrayComp(P, Arr[i],aDescendings,rb) > 0 do
       I := I + 1;
     while arrayComp(P, Arr[J],aDescendings,rb) < 0 do
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
       arraySort(Arr, L, J, aDescendings,rb);
     L := I;
   end
   else
   begin
     if I < R then
       arraySort(Arr, I, R, aDescendings,rb);
     R := J;
   end;
 until L >= R;
end;

class function TServiceTools.arrayUnique(const arr: array of TVariantArray): TArray<TVariantArray>;
var i:integer;
begin
  if length(arr)=0 then exit;// early exit
  setLength(result,0);
  if High(arr)>-1 then Insert(arr[0],Result,length(Result));
  for I := 1 to High(arr) do
    if arrayComp(arr[i],arr[i-1],false,false)<>0 then
      Insert(arr[i],Result,length(Result))
end;

class function TServiceTools.IndexOf<T>(const arr: array of T; const Val: T
  ): integer;
begin
  for result:=0 to high(arr) do
    if cmp<T>(arr[result],Val)=0 then exit;
  result:=-1;
end;

class function TServiceTools.SparseValues<T>(const arr: array of T;
  const Indecies: array of Integer): TArray<T>;
var i:integer;
begin
  setLength(result,Length(Indecies));
  for i:=0 to High(Indecies) do
    result[i]:=arr[Indecies[i]]
end;

class function TServiceTools.SparseIndecies<T>(const arr: array of T; const AValues: array of T): TArray<integer>;
var
   i: Integer;
begin
  setLength(result,length(AValues));
  for i:=0 to High(AValues) do
    result[i]:=TServiceTools.IndexOf<T>(arr,AValues[i]);
end;

class function TServiceTools.BinSearch<T>(const Arr: array of T; const Val: T; R: integer): integer;
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


class function TServiceTools.IfThen<T>(const cond: boolean; const whenTrue, whenFalse: T): T;
begin
  if cond then result:=whenTrue else result:=whenFalse
end;

class function TServiceTools.max<T>(const a, b: T): T;
begin
  {$ifdef fpc}
  if a>=b then result:=a else result:=b
  {$else}
  result:=ifthen<T>(TComparer<T>.Default().Compare(a,b)>0,a,b)
  {$endif}
end;

class function TServiceTools.min<T>(const a, b: T): T;
begin
  {$ifdef fpc}
  if a<=b then result:=a else result:=b
  {$else}
  result:=ifthen<T>(TComparer<T>.Default().Compare(a,b)<0,a,b)
  {$endif}
end;


class procedure TServiceTools.QuickSort<T>(var Arr: array of T; L, R : Longint; const Descending:boolean; Compare:TComparefunc<T>);
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

class function TServiceTools.cmp<T>(const a, b: T): integer;
begin
  {$ifdef fpc}
  result:=1;
  if a=b then result:=0
  else if a<b then result:=-1
  {$else}
  result:=TComparer<T>.Default.Compare(a,b)
  {$endif}
end;

class function TServiceTools.TransposeData(const ATable: TTabledata
  ): TTableData;
var i,j:Int64;
begin
  if not Assigned(Atable) then exit;
  if not Assigned(Atable[0]) then exit;
  setlength(Result,Length(ATable[0]));
  for i:=0 to high(ATable[0]) do
    setLength(result[i],Length(Atable));
  for j:=0 to high(result[i]) do for i:=0 to high(result) do
    result[i][j]:=ATable[j][i]
end;

class procedure TServiceTools.mergeDictionaries<Key, Value>(const inDics: TArray
  <TDictionary<Key, Value>>; var outDic: TDictionary<Key, Value>;
  const resolver: TDictCollisionResolver<Value>);
var i,j:SizeInt;
    kv:TPair<Key,Value>;
    val:Value;found:boolean;
begin
  if Assigned(resolver) then
    for i:=0 to High(inDics) do
      for kv in inDics[i] do begin
        found:=outDic.tryGetValue(kv.key,val);
        if found then begin
          resolver(val,kv.value);
          //outDic[kv.key]:=val;
        end else
          outDic.add(kv.key,kv.value);
      end
  else
    for i:=0 to High(inDics) do
      for kv in inDics[i] do begin
        outDic.tryAdd(kv.key,kv.value)
      end;
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

function Dice(const a:TDataRecord;const v:Variant;const aSize:integer):TArray<TDataRecord>;
var
    i,j:integer;
begin
  //if length(a)>0 then m:=1 shl Length(a) else m:=TServiceTools.ifthen<integer>(mm>0,1,2);

  //m:=StrToInt('%'+TStringDynArray.fill(ifthen(length(a)>0,Length(a),ifthen(Length(colsIds>0),0,1)),1).ToString(''));
//    m:=parseInt(createFilled(a.length||(colsIds.length>0?0:1),1).join(''),2);
  setLength(Result,aSize);
  for i:=0 to aSize-1 do begin
    Result[i]:=TDataRecord.Fill(TServiceTools.max<Integer>(length(a),1),0);
    for j:=0 to high(Result[i]) do
      if i and (1 shl j)>0 then
        Result[i][j]:=v
      else
        if assigned(a) then Result[i][j]:=a[j] //else n[i][j]:=v
  end ;
    //n.push(createFilled(a.length,0).join('')+i.toString(2)).slice(-a.length).split('').map(function(el,i){return (el==='1'?v:a[i])}));
  //result := n
end;

{$define NOEXPEREMENTAL1}

function TObjData.isFiltered(const checkRow:integer):boolean;
var jj:integer;
begin
  result:=false;
  for jj:=0 to high(filtIds) do
     if filters[jj].data.Lookup(Data[checkRow][filtIds[jj]])<0 then
       begin result:=true;break;end;
  //if filtRow then begin filtRow:=false;continue end;                  // if filter list is inroduced then skip elements not in the filter list
end;

(*
class procedure TObjData.PrepareHeaders(aData: Pointer);
var
  rowNo,i,idx:SizeInt;
  _range:PSizeInt;
  Params :TParams absolute aData;
  row, col, ro, co:TDataRecord;
  _Row,_Col:PDimensionSet;
begin
  _range:=PSizeInt(params[1]);
  writeln('PrepareHeaders Entring Thread [',_range[2],'] from [',_range[0],' count [',_range[1],']');
  idx:=_range[2];
  setLength(row,length(PObjData(params[0]).rowsIds));
  setLength(col,length(PObjData(params[0]).colsIds));
  _row:= PDimensionSet(params[2]);
  _col:= PDimensionSet(params[3]);
  inc(_row,idx);
  inc(_col,idx);
  with PObjData(params[0])^ do
  for rowNo:=_range[0] to _range[0]+_range[1]-1 do begin
      if isFiltered(rowNo) then continue;
      for i:=0 to high(rowsIds) do
        row[i]:= Data[rowNo][rowsIds[i]];     //take values of selected rows into row[]
      for i:=0 to high(colsIds) do                //take values of selected columns into columns[]
        col[i]:= Data[rowNo][colsIds[i]];

      // calculate unique and subtotal rows
      // ToDo -cPerformance : explore using TArray<FieldType> instead of TVariantArray?
      for i:=TServiceTools.ifthen<Integer>(totalsOptions.subRows, 1, length(rowsIds)) to length(rowsIds) do begin
         {$ifdef Profiling2} Profiler.Log(0); {$endif}
          ro:=copy(row,0,i).concat(TDataRecord.Fill(length(rowsIds)-i,arrayCompFiller));
          {$ifdef Profiling2} Profiler.Log(1); {$endif}
          _Row.tryAdd(ro.toString(splitter),ro);
          {$ifdef Profiling2} Profiler.Log(2); {$endif}
      end;
      // calculate unique subtotal columns
      for i:=TServiceTools.ifthen<Integer>(totalsOptions.subCols,1,length(colsIds)) to length(colsIds) do begin
          {$ifdef Profiling2} Profiler.Log(0); {$endif}
          co:=copy(col,0,i).concat(TDataRecord.fill(length(colsIds)-i,arrayCompFiller));
          {$ifdef Profiling2} Profiler.Log(1); {$endif}
          _Col.tryAdd(co.toString(splitter),co);
          {$ifdef Profiling2} Profiler.Log(2); {$endif}
      end;
  end;
  writeln('PrepareHeaders Exiting Thread [',idx,']');
end;  // 9.7 seconds on {Rows':Sector', Columns:'Provider', Measure : Sum('Price')}

class procedure TObjData.ScanTable(AData: pointer);
var

  Params:TParams absolute aData;
  _range:PSizeInt;
  rowNo,i,j,k,idx:SizeInt;
  {$ifdef TABLEDATA2}resultValues:TArray<TResults>;{$endif}
  row, col, Rw:TDataRecord;
  _result:PResultData;
  initSize:SizeInt;
  rCube:string;
  cub:TTableData;
  id:TResults;
  hashFound:boolean;
  val:Variant;
begin
  _range:=PSizeInt(Params[1]);
  writeln('ScanTable Entring Thread [',_range[2],']');
  idx:=_range[2];
  setLength(row,length(PObjData(params[0])^.rowsIds));
  setLength(col,length(PObjData(params[0])^.colsIds));
  _result:=PResultData(params[2]);
  inc(_result,idx);
  with PObjData(Params[0])^ do  begin
    initSize:=length(data) div (length(_Results)*(resultCube.Cols.count + resultCube.Rows.count));
    for rowNo:=_range[0] to _range[0]+_range[1]-1 do begin
      if isFiltered(rowNo) then continue;
      for i:=0 to high(rowsIds) do
       row[i]:= Data[rowNo][rowsIds[i]];     //take values of selected rows into row[]
      for i:=0 to high(colsIds) do                //take values of selected columns into columns[]
       col[i]:= Data[rowNo][colsIds[i]];

      // ********************* check factorial array multiplication from here
      {$ifdef Profiling2} Profiler.Log(0); {$endif}
      if assigned(col) or assigned(row) then begin
        rw:=copy(row,0,length(rowsIds)).concat(copy(col,0,length(colsIds)));
        //if length(rw)>0 then
        {$ifdef Profiling2} Profiler.Log(1); {$endif}
        k:=1 shl Length(rw);
        cub:=Dice(rw, arrayCompFiller, k);
      end
      else
        cub:=[[arrayCompFiller]];
      {$ifdef Profiling2} Profiler.Log(2); {$endif}
      for i:=0 to high(cub) do  begin
        rCube:=cub[i].toString(splitter,'',false);
        {$ifdef Profiling2} Profiler.Log(3); {$endif}
        hashFound:=_result.tryGetValue(rCube,id);
        {$ifdef Profiling2} Profiler.Log(4); {$endif}
        if not hashFound then begin
          //id:=TResults.Create(length(measIds), initSize );
          setLength(id.TableData,length(measIds));
          setLength(id.ReducedMeasures,length(measIds));
          for j:=0 to high(id.TableData) do
            setLength(id.TableData[j].Data,initSize);
          _result.Add(rCube,id);
        end;
        {$ifdef Profiling2} Profiler.Log(5); {$endif}
        { ToDo -cPerformance : if the same measure column is added again consider referencing
          the added one inseated of pushing a new measure  }
        for j:=0 to High(measIds) do begin
          val:=Data[rowNo][measIds[j]];
          if not varIsEmpty(val) then
            {$ifdef TABLEDATA2}
            id.TableData[j].Push(val)
            {$else}
            insert(val,id.TableData[j],length(id.TableData[j]))
            {$endif}
        end;
        {$ifdef Profiling2} Profiler.Log(6); {$endif}
      end;
    end;
  end;//end scanning table
  {$ifdef TABLEDATA2}
  resultValues:=_result.values.toArray;
  for i:=0 to high(resultValues) do
    for j:=0 to high(resultValues[i].TableData) do
      resultValues[i].TableData[j].Shrink;
  {$ifdef Profiling2}
  Profiler.Log('Shrinking...');
  {$endif}
  {$endif}
  writeln('ScanTable Exiting Thread [',idx,']');
end;
*)
class procedure TObjData.dictResultsResolver(var dest: TResults;
  const source: TResults);
var i,j:SizeInt;
begin
  for i:=0 to high(dest.TableData) do
    dest.TableData[i].concat(source.TableData[i]);
end;

function TObjData.cube():TObjData;
var
  i, j, k, rowNo:SizeInt;
  m_span:Integer;
  resultValues:TArray<TResults>;
  colStack, rowStack:array of TRecordStack;
  fieldNames :TStringDynArray;
  //row, col,
    ro, co, rw:TDataRecord;
  id:TResults;
  rSort, cSort: array of boolean;
//  rowAgg ,colAgg {,tmpAgg}:THeader;
  Aggs: THeaders;
  oprsIds:array of TAggregation;
  FieldTypes:array of THeaderType;
  FCols,FRows:TTableData;
  params:TParams;


 // dimensions:  TDimensions ;
//  data: TCubeData;
  //_result:TResultData;

  //ToDo : check why format measure won't apply in case of date type in row dimension?
  procedure calcVals;
  var m,r,c:integer;formula:string;v:TDataRecord;vv:Variant;
  begin
        for m:=0 to high(dimensions.Measures) do begin
            for r:=0 to High(result.data) do begin
                formula:=dimensions.measures[m].formula;
                if formula<>'' then
                  for c:=0 to High(FCols) do   //Done : Take Cols/Rows toArray variables before entring the loop
                    begin
                        v:=FCols[c];
                        if result.data[r][c*length(dimensions.measures)+m]='' then exit;
                        result.data[r][c*length(dimensions.measures)+m]:=eval(
                                formula.replace('{val}',result.data[r][c*length(dimensions.measures)+m],[rfReplaceAll])
                                .replace('{colTotal}',result.results[TDataRecord.Fill(Length(dimensions.rows),arrayCompFiller).concat(v).ToString(splitter,'',false)].ReducedMeasures[m],[rfReplaceAll])
                                .replace('{rowTotal}',result.results[FRows[r].concat(TDataRecord.Fill(length(dimensions.cols),arrayCompFiller)).ToString(splitter,'',false)].ReducedMeasures[m],[rfReplaceAll])
                                .replace('{grandTotal}',result.results[TDataRecord.Fill(length(dimensions.cols)+length(dimensions.rows),arrayCompFiller).ToString(splitter,'',false)].ReducedMeasures[m],[rfReplaceAll])
                        );
                    end;
                if dimensions.measures[m].decimals>=0 then
                    for c:=0 to High(FCols) do begin
                        vv:=result.data[r][c*Length(dimensions.measures)+m];
                        if (TVarData(vv).vtype in [varEmpty,varNull])
                          or VarIsStr(vv) then
                            exit;
                        if TVarData(vv).vtype in varsOrdinal then
                          result.data[r][c*Length(dimensions.measures)+m]:=format('%.0n',[double(vv)])
                        else if TVarData(vv).vtype in varsFloats then
                          result.data[r][c*Length(dimensions.measures)+m]:=format('%.'+dimensions.measures[m].decimals.ToString+'n',[double(vv)]) ;
                    end;
            end
        end
  end;

begin
  {$ifdef Profiling}  Profiler.start; {$endif}
    resultCube:=@result;
    setLength(fieldNames,length(Headers.Columns[0]));
    setLength(fieldTypes,length(fieldNames));

    for i:=0 to High(Headers.Columns[0]) do begin
        fieldNames[i]:=Headers.Columns[0][i].name;
        fieldTypes[i]:=Headers.Columns[0][i].typ;
    end;

    setLength(rowsIds,length(dimensions.rows));
    for i:=0 to High(dimensions.rows) do begin
      rowsIds[i]:=fieldNames.indexOf(dimensions.rows[i]);
      if rowsIds[i]<0 then raise Exception.CreateFmt('Field [%s] not found!',[dimensions.rows[i]]);
    end;
    setLength(colsIds,length(dimensions.cols));
    for i:=0 to High(dimensions.cols) do begin
      colsIds[i]:=fieldNames.indexOf(dimensions.cols[i]);
      if colsIds[i]<0 then raise Exception.CreateFmt('Field [%s] not found!',[dimensions.cols[i]]);
    end;
    setLength(measIds,length(dimensions.measures));
    setLength(oprsIds,length(dimensions.measures));
    for i:=0 to High(dimensions.measures) do begin
      measIds[i]:=fieldNames.indexOf(dimensions.measures[i].field);
      if measIds[i]<0 then raise Exception.CreateFmt('Field [%s] not found!',[dimensions.measures[i].field]);
      oprsIds[i]:=dimensions.measures[i].operation;
      if oprsIds[i].typ=varEmpty then
        oprsIds[i].typ:=FieldTypes[MeasIds[i]];
    end;
    setLength(filtIds,length(filters));
    for i:=0 to High(filters) do
      filtIds[i]:=fieldNames.indexOf(filters[i].fieldName);

    //Insert(TDataRecord.fill(TServiceTools.ifthen<Integer>(options.grandRows,TServiceTools.ifthen<Integer>(length(rowsIds)>0,length(rowsIds),TServiceTools.ifthen<Integer>(length(colsIds)>0,0,1)),1),arrayCompFiller),result.rows,length(result.Rows)); //grand total columns
    //Insert(TDataRecord.fill(TServiceTools.ifthen<Integer>(options.grandCols,length(colsIds),0),arrayCompFiller),result.cols,length(result.cols)); //grand total rows
    // ToDo -cMeasuresOnRows: add optional [Measure header] on rows instead of columns
    ro:=TDataRecord.fill(TServiceTools.ifthen<Integer>(totalsOptions.grandRows,TServiceTools.ifthen<Integer>(length(rowsIds)>0,length(rowsIds),TServiceTools.ifthen<Integer>(length(colsIds)>0,0,1)),1),arrayCompFiller);
    co:=TDataRecord.fill(TServiceTools.ifthen<Integer>(totalsOptions.grandCols,length(colsIds),0),arrayCompFiller);
    //if assigned(ro) then
      result.rows.tryAdd(ro.toString(splitter),ro);
    //if assigned(co) then
      result.cols.tryAdd(co.toString(splitter),co);
    // add grand total row anyway (horizonal measures case)
    setLength(rSort,length(dimensions.rows));for i:=0 to high(dimensions.rows) do rSort[i]:=descends.IndexOf(dimensions.Rows[i])>=0;
    setLength(cSort,length(dimensions.cols));for i:=0 to high(dimensions.cols) do cSort[i]:=descends.IndexOf(dimensions.Cols[i])>=0;

    {$ifdef Profiling} Profiler.Log('Initialization...'); {$endif}
    //if not assigned(Data) then exit;
    //if not assigned(Data[0]) then exit;
    {$ifndef NOEXPEREMENTAL1} setLength(r,length(rowsIds));setLength(c,length(colsIds)); {$endif}
    setLength(threadList,TServiceTools.max<Integer>(CPUCount div 2,1)); // don't Throttle my CPU please :);
    setLength(_Cols,length(threadList));
    for i:=0 to high(_Cols) do _Cols[i]:=TDimensionSet.Create;
    setLength(_Rows,length(threadList));
    for i:=0 to high(_Rows) do _Rows[i]:=TDimensionSet.Create;
    k:=Length(data) div length(threadList);
    j:=0;
    for i:=0 to High(threadList) do begin
      threadList[i]:=TCubeThread.Create(@Self,i,j,k,_Cols[i],_Rows[i],nil);
      inc(j,k);
    end;

    for i:=0 to High(threadList) do begin
      if assigned(threadlist[i]) then
//        WaitForThreadTerminate(threadList[i].Handle,0);
        threadlist[i].WaitFor;
    end;


    //is there any remaining records? then execute in main thread;
    if j<Length(Data) then
      TCubeThread.PrepareHeaders(@Self, Length(threadList), j, Length(Data)-j, result.cols, result.rows);
    {$ifdef Profiling}Profiler.Log('Headers Prepared...');{$endif}
//    {$ifdef Profiling} Profiler.LogSegments(['','Preparing Key...','Bin Lookup.., Dimension Insertion..'],true); {$endif}

    // ToDo : Merge Headers from threads
    TServiceTools.mergeDictionaries<string,TDataRecord>(_Cols,result.cols,nil);
    TServiceTools.mergeDictionaries<string,TDataRecord>(_rows,result.rows,nil);

    FCols:=result.Cols.values.toArray;
    FRows:=result.Rows.values.toArray;

    for i:=0 to high(_Cols) do FreeAndNil(_Cols[i]);
    for i:=0 to High(_Rows) do FreeAndNil(_Rows[i]);
    TServiceTools.arraySort(FCols,0,High(FCols),cSort,totalsOptions.bc);
    TServiceTools.arraySort(FRows,0,High(FRows),rSort,totalsOptions.br);
    //writeln('Rows : ',FRows.toString(',','"',true));
    //writeln('Columns: ',FCols.toString(',','"',true));
    {$ifdef Profiling}Profiler.Log('Dimensions Populated...');{$endif}
    //Result.Results.SetCapacity(length(Data));

    setLength(_Results,Length(threadList));
    for i:=0 to High(_Results) do
      _Results[i]:=TResultData.Create;
    j:=0;
    for i:=0 to high(threadList) do begin
      threadList[i]:=TCubeThread.Create(@Self,i,j,k,nil,nil,_Results[i]);
      inc(j,k)
    end;
    for i:=0 to High(threadList) do begin
      if assigned(threadlist[i]) then
//        WaitForThreadTerminate(threadList[i].Handle,0);
        threadlist[i].WaitFor;
    end;

    //is there any remaining records? then execute in main thread;
    if j<Length(Data) then
      TCubeThread.ScanTable(@Self, length(threadList),j, Length(Data)-j, result.results);
    {$ifdef Profiling}Profiler.Log('Table Scanned...');{$endif}

    //{$ifdef Profiling} Profiler.LogSegments(['','Preparing Dice Key...','Dicing...','Building hash key..','Hashing...','Creating Measure Array..','Push to measures..']); {$endif}


    // ToDo : merge resultValues from threads
    // ToDo : Implement a merge collision resolver function
    TServiceTools.mergeDictionaries<string,TResults>(_results,result.results,dictResultsResolver);
    resultValues:=result.results.Values.ToArray;
    for i:=0 to High(_Results) do FreeAndNil(_Results[i]);

    if Length(FCols)>0 then begin
        setLength(colStack,length(FCols[0]));
        for j:=0 to high(FCols[0]) do begin
            colStack[j].i:=0;
            colStack[j].v:=copy(FCols[0],0,j+1)
        end;//i=0;
        for i:=0 to High(FCols) do begin
            setLength(result.headers.columns,Length(FCols[0]));
            for j:=0 to high(FCols[0]) do begin
                if arrayComp(copy(FCols[i],0,j+1),colStack[j].v,false,false)<>0 then begin
                    m_span:=i-colStack[j].i;
                    Insert(THeader.Create({name:=}colStack[j].v[j],{typ:=}TVarData(colStack[j].v[j]).vtype,{span:=}m_span * length(measIds)),result.headers.columns[j],Length(result.headers.columns[j]));
                    colStack[j].i:=i;colStack[j].v:=copy(FCols[i],0,j+1);
                end
            end;
            // column identify subtotals
            if FCols[i].indexOf(arrayCompFiller)>-1 then
                for j:=0 to high(measIds) do
                      Insert(i*length(measIds)+j,result.colsHeadSubTotals,length(result.colsHeadSubTotals))
        end;
        for i:=0 to High(FCols) do
          for j:=0 to high(dimensions.measures) do begin
            //tmpAgg.Name:=ifthen<string>(dimensions.measures[j]._Label<>'', dimensions.measures[j]._Label, oprsIds[j].Name+' of '+dimensions.measures[j].Field);
            //tmpAgg.typ:=Ifthen<THeaderType>(IndexOf<string>(['sum','count','distinct_count','distinct_sum','mean','average','mean','stddev','mode','median'], lowerCase(oprsIds[j].Name))>-1,htDouble,fieldTypes[measIds[j]]);
            insert(
              THeader.Create(
                TServiceTools.ifthen<string>(dimensions.measures[j]._Label<>'', dimensions.measures[j]._Label, oprsIds[j].Name+' of '+dimensions.measures[j].Field),
                oprsIds[j].typ
              )
             ,Aggs,length(Aggs));
          end;
        Insert(Aggs,result.headers.columns,length(result.headers.columns));
        for j:=0 to high(FCols[0]) do begin
              m_span:=length(FCols) -colStack[j].i;
              //with tmpAgg do begin Name:=colStack[j].v[j]; typ:=htString; span:=m_span*length(measIds) end;
              insert(THeader.Create(colStack[j].v[j], TVarData(colStack[j].v[j]).vtype {FieldTypes[ColsIds[colStack[j].i]]}, m_span*length(measIds)),result.headers.columns[j],length(result.headers.columns[j]));
        end;

    end;
    {$ifdef Profiling} Profiler.Log('Result Cube Headers...');{$endif}

   if Length(FRows)>0 then begin
     for j:=0 to TServiceTools.ifthen<Integer>(length(rowsIds)>0,length(rowsIds),TServiceTools.ifthen<Integer>(length(colsIds)>0,0,1)) do begin
          setLength(rowStack,j+1);rowStack[j].i:=0; rowStack[j].v:=copy(FRows[0],0,j+1)
     end;
     i:=0;
     setLength(result.headers.rows,length(FRows[0]));
     for i:=0 to high(FRows) do begin
         if FRows[i].indexOf(arrayCompFiller)>-1 then insert(i,result.rowsHeadSubTotals,length(result.rowsHeadSubTotals));
         for j:= 0 to high(FRows[i]) do begin
           if (j<length(rowStack)) and (arrayComp(copy(FRows[i], 0 ,j+1),rowStack[j].v, false,false)<>0) then begin
              m_span:= i-rowStack[j].i;
              //if m_span=0 then
              //  beep;              // for debugging
              //setLength(result.headers.rows[j],length(result.headers.rows[j])+1);
              //with result.headers.rows[j][high(result.headers.rows[j])] do begin
              //  name := rowStack[j].v[j];
              //  span:=m_span;
              //  typ:=htString;
              //end;
              insert(THeader.create(rowStack[j].v[j], TVarData(rowStack[j].v[j]).VType, m_span),result.headers.rows[j],length(result.headers.rows[j]));
              setLength(result.headers.rows[j],high(result.headers.rows[j])+m_span) ;

              rowStack[j].i:=i;rowStack[j].v:=copy(FRows[i],0,j+1);
           end;
          end
     end;
     for j:=0 to high(FRows[0]) do begin
       m_span:=length(FRows) -rowStack[j].i;
       //if m_span=0 then
       //  beep;              // for debugging
       //setLength(result.headers.rows[j],length(row[j])+1);
       //with result.headers.rows[j][high(result.headers.rows[j])] do
       //  begin name:= rowStack[j].v[j]; typ:=htString; Span:=m_span end;
       insert(THeader.Create(rowStack[j].v[j], TVarData(rowStack[j].v[j]).vtype{FieldTypes[RowsIds[rowStack[j].i]]}, m_span),result.headers.rows[j],length(result.headers.rows[j]));
       setLength(result.headers.rows[j],high(result.headers.rows[j])+m_span);
     end;
   end;
   {$ifdef Profiling} Profiler.Log('Cube SubTotals ...');{$endif}
   //resultValues:=result.results.Values.ToArray;
   for i:=0 to result.results.Count-1 do begin
       for j:=0 to high(oprsIds) do begin
         //ToDo : Check why <zeros> show on count in some cells while it should be empty instead
       if LowerCase(oprsIds[j].Name)='count' then
         resultValues[i].ReducedMeasures[j]:=length(resultValues[i].TableData[j]{$ifdef TABLEDATA2}.Data{$endif})//reduce<Variant>(result.results.ValueList[i].TableData[j], oprsIds[j].operation, 0)
       else if LowerCase(oprsIds[j].Name)='distinct_count' then
         resultValues[i].ReducedMeasures[j]:=Length(resultValues[i].TableData[j]{$ifdef TABLEDATA2}.Data{$endif}.unique())//reduce<Variant>(result.results.ValueList[i].TableData[j], oprsIds[j].operation, 0)
       else if LowerCase(oprsIds[j].Name)='concat' then
         resultValues[i].ReducedMeasures[j]:=resultValues[i].TableData[j]{$ifdef TABLEDATA2}.Data{$endif}.tostring(', ','',false)
       else if LowerCase(oprsIds[j].Name)='distinct_concat' then
         resultValues[i].ReducedMeasures[j]:=resultValues[i].TableData[j]{$ifdef TABLEDATA2}.Data{$endif}.unique().toString(', ','',false)
       else
         resultValues[i].ReducedMeasures[j]:=resultValues[i].TableData[j]{$ifdef TABLEDATA2}.Data{$endif}.reduce(oprsIds[j].operation)
       end;
   end;
   {$ifdef Profiling} Profiler.Log('Cube Reduction calculated');{$endif}

   for i:=0 to High(FRows) do begin
     setLength(Rw,0);
     for j:=0 to high(FCols) do begin
       if not result.results.TryGetValue(FRows[i].concat(FCols[j]).ToString(splitter,'',false),id) then
         id.ReducedMeasures:=TDataRecord.Fill(length(measIds),'');
       for k:=0 to high(id.ReducedMeasures) do
         insert(id.ReducedMeasures[k],Rw,length(rw))
     end;
     insert(Rw,result.data,length(result.data));
   end;
   if length(colsIds)>0 then
     for i:=0 to high(measIds) do
       insert((TServiceTools.arrayLookup(FCols ,TDataRecord.Fill(length(colsIds),arrayCompFiller)))*length(measIds)+i,result.colsHeadGrandTotals,length(result.colsHeadGrandTotals));
       //insert((result.cols.lookup(TDataRecord.Fill(length(colsIds),arrayCompFiller)))*length(measIds)+i,result.colsHeadGrandTotals,length(result.colsHeadGrandTotals));
   if length(rowsIds)>0 then
     Insert(TServiceTools.arrayLookup(FRows,TDataRecord.Fill(length(rowsIds),arrayCompFiller)),result.rowsHeadGrandTotals,length(result.rowsHeadGrandTotals));
     //Insert(result.rows.lookup(TDataRecord.Fill(length(rowsIds),arrayCompFiller)),result.rowsHeadGrandTotals,length(result.rowsHeadGrandTotals));
   {$ifdef Profiling} Profiler.Log('Cube GrandTotals prepared');{$endif}
   calcVals;
   result.Dimensions:=Dimensions;
   {$ifdef Profiling} Profiler.Log('Cube finalized');{$endif}

end;

(*function cube(const obj:TObjData;const options:TTotalsOptions):TObjData;
var
  i, j, k, rowNo, m_span:integer;
  rCube:string;
  row, col, r, c, Rw:TDataRecord;
  id:TResults;
  resultValues:TArray<TResults>;
  colStack, rowStack:array of TRecordStack;
  fieldNames :TStringDynArray;
  cub:TTableData;
  colsIds, rowsIds, filtIds, measIds:TIntegerDynArray;
  rSort, cSort: array of boolean;
//  rowAgg ,colAgg {,tmpAgg}:THeader;
  Aggs: THeaders;
  oprsIds:array of TAggregation;
  FCols,FRows:TTabledata;
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
                  for c:=0 to High(FCols) do
                    begin
                        v:=FCols[c];
                        if result.data[r][c*length(obj.dimensions.measures)+m]='' then exit;
                        result.data[r][c*length(obj.dimensions.measures)+m]:=eval(
                                formula.replace('{val}',result.data[r][c*length(obj.dimensions.measures)+m],[rfReplaceAll])
                                .replace('{colTotal}',result.results[TDataRecord.Fill(Length(obj.dimensions.rows),arrayCompFiller).concat(v).ToString('#9','',false)].ReducedMeasures[m],[rfReplaceAll])
                                .replace('{rowTotal}',result.results[FRows[r].concat(TDataRecord.Fill(length(obj.dimensions.cols),arrayCompFiller)).ToString('#9','',false)].ReducedMeasures[m],[rfReplaceAll])
                                .replace('{grandTotal}',result.results[TDataRecord.Fill(length(obj.dimensions.cols)+length(obj.dimensions.rows),arrayCompFiller).ToString('#9','',false)].ReducedMeasures[m],[rfReplaceAll])
                        );
                    end;
                if obj.dimensions.measures[m].decimals>=0 then
                    for c:=0 to High(FCols) do begin
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

  function isFiltered(const checkRow:integer):boolean;
  var jj:integer;
  begin
    for jj:=0 to high(filtIds) do
       if obj.filters[jj].data.Lookup(obj.Data[checkRow][filtIds[jj]])<0 then
         begin result:=true;break;end;
    //if filtRow then begin filtRow:=false;continue end;                  // if filter list is inroduced then skip elements not in the filter list
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

    Insert(TDataRecord.fill(TServiceTools.ifthen<Integer>(options.grandRows,TServiceTools.ifthen<Integer>(length(rowsIds)>0,length(rowsIds),TServiceTools.ifthen<Integer>(length(colsIds)>0,0,1)),1),arrayCompFiller),FRows,length(FRows)); //grand total columns
    Insert(TDataRecord.fill(TServiceTools.ifthen<Integer>(options.grandCols,length(colsIds),0),arrayCompFiller),FCols,length(FCols)); //grand total rows
    // add grand total row anyway (horizonal measures case)
    setLength(rSort,length(obj.dimensions.rows));for i:=0 to high(obj.dimensions.rows) do rSort[i]:=obj.descends.IndexOf(obj.dimensions.Rows[i])>=0;
    setLength(cSort,length(obj.dimensions.cols));for i:=0 to high(obj.dimensions.cols) do cSort[i]:=obj.descends.IndexOf(obj.dimensions.Cols[i])>=0;

    setLength(row,length(rowsIds));
    setLength(col,length(colsIds));
    {$ifdef Profiling} Profiler.Log('Initialization...'); {$endif}
    for rowNo:=0 to High(obj.Data) do begin
        if isFiltered(rowNo) then continue;
        for i:=0 to high(rowsIds) do
          row[i]:= obj.Data[rowNo][rowsIds[i]];     //take values of selected rows into row[]

        for i:=0 to high(colsIds) do                //take values of selected columns into columns[]
          col[i]:= obj.Data[rowNo][colsIds[i]];

        // calculate subtotal rows
        for i:=TServiceTools.ifthen<Integer>(options.subRows, 1, length(rowsIds)) to length(rowsIds) do begin
            r:=copy(row,0,i).concat(TDataRecord.Fill(length(rowsIds)-i,arrayCompFiller));  // explore using TArray<FieldType> instead of TVariantArray?
            //r:=slice<TDataRecord>(row,0,i).concat(TDataRecord.Fill(length(rowsIds)-i,arrayCompFiller));  // explore using TArray<FieldType> instead of TVariantArray?
            j:=TServiceTools.arrayLookup(FRows,r,rSort,options.br);
            if j<0 then
              insert(r,FRows,-j-1);
              //splice<TTableData>(FRows,-j-1,0,[r]);
        end;

        // calculate subtotal columns
        for i:=TServiceTools.ifthen<Integer>(options.subCols,1,length(colsIds)) to length(colsIds) do begin
            c:=copy(col,0,i).concat(TDataRecord.fill(length(colsIds)-i,arrayCompFiller));
            //c:=slice<TDataRecord>(col,0,i).concat(TDataRecord.fill(length(colsIds)-i,arrayCompFiller));
            k:=TServiceTools.arrayLookup(FCols,c,cSort,options.bc);
            if k<0 then
              Insert(c,FCols,-k-1);
              //splice<TTableData>(FCols,-k-1,0,[c]);
        end;
    end;
    {$ifdef Profiling} Profiler.Log('Preparing Columns, Rows ...'); {$endif}


    for rowNo:=0 to high(obj.data) do begin
        if isFiltered(rowNo) then continue;
        {$ifdef Profiling}
                    Profiler.Log(0);
        {$endif}
        for i:=0 to high(rowsIds) do
          row[i]:= obj.Data[rowNo][rowsIds[i]];     //take values of selected rows into row[]

        for i:=0 to high(colsIds) do                //take values of selected columns into columns[]
          col[i]:= obj.Data[rowNo][colsIds[i]];
        {$ifdef Profiling}
                    Profiler.Log(1);
        {$endif}
        // ********************* check factorial array multiplication from here
        rw:=copy(row,0,length(rowsIds)).concat(copy(col,0,length(colsIds)));
        if length(rw)>0 then
          k:=1 shl Length(rw)
        else
          k:=TServiceTools.ifthen<integer>(length(colsIds)>0,1,2);
        cub:=Dice(rw,arrayCompFiller,k);
        {$ifdef Profiling}
                    Profiler.Log(2);
        {$endif}
        for i:=0 to high(cub) do  begin
            rCube:=cub[i].toString(splitter,'',false);
            if not result.results.tryGetValue(rCube,id) then begin
              id:=TResults.Create(length(measIds),length(obj.data) div (length(FCols) + length(FRows)) );
              result.results.Add(rCube,id);
            end;
            {$ifdef Profiling}Profiler.Log(4);{$endif}
            for j:=0 to High(measIds) do begin
              val:=obj.Data[rowNo][measIds[j]];
              if not varIsEmpty(val) then
                {$ifdef TABLEDATA2}
                id.TableData[j].Push(val);
                {$else}
                insert(val,id.TableData[j],length(id.TableData[j]));
                {$endif}
              //id:=result.results[rCube];
              {$ifdef Profiling}Profiler.Log(5); {$endif}
            end;
        end;
        {$ifdef Profiling}
                    Profiler.Log(3);
        {$endif}
    end;//end scanning table
    {$ifdef Profiling}
      Profiler.LogSegments(['Filetering','Making Key','Dicing','Adding Measures','  --Lookng for hash','  --Pushing to array']);
      Profiler.Log('Scanning Complete');
    {$endif}
    {$ifdef TABLEDATA2}
    for i:=0 to result.results.Count-1 do
      for j:=0 to high(result.results.Values{$ifdef fpc}.ToArray{$endif}[i].TableData) do
        result.results.Values{$ifdef fpc}.ToArray{$endif}[i].TableData[j].Shrink;
      {$ifdef Profiling}
      Profiler.Log('Shrinking...');
      {$endif}
    {$endif}

    if Length(FCols)>0 then begin
        setLength(colStack,length(FCols[0]));
        for j:=0 to high(FCols[0]) do begin
            colStack[j].i:=0;
            colStack[j].v:=copy(FCols[0],0,j+1)
        end;//i=0;
        for i:=0 to high(FCols) do begin
            setLength(result.headers.columns,Length(FCols[0]));
            for j:=0 to high(FCols[0]) do begin
                if arrayComp(copy(FCols[i],0,j+1),colStack[j].v,false,false)<>0 then begin
                    m_span:=i-colStack[j].i;
                    Insert(THeader.Create({name:=}colStack[j].v[j],{typ:=}varString,{span:=}m_span * length(measIds)),result.headers.columns[j],Length(result.headers.columns[j]));
                    colStack[j].i:=i;colStack[j].v:=copy(FCols[i],0,j+1);
                end
            end;
            // column identify subtotals
            if FCols[i].indexOf(arrayCompFiller)>-1 then
                for j:=0 to high(measIds) do
                      Insert(i*length(measIds)+j,result.colsHeadSubTotals,length(result.colsHeadSubTotals))
        end;
        for i:=0 to high(FCols) do
          for j:=0 to high(obj.dimensions.measures) do begin
            //tmpAgg.Name:=ifthen<string>(obj.dimensions.measures[j]._Label<>'', obj.dimensions.measures[j]._Label, oprsIds[j].Name+' of '+obj.dimensions.measures[j].Field);
            //tmpAgg.typ:=Ifthen<THeaderType>(IndexOf<string>(['sum','count','distinct_count','distinct_sum','mean','average','mean','stddev','mode','median'], lowerCase(oprsIds[j].Name))>-1,htDouble,fieldTypes[measIds[j]]);
            insert(
              THeader.Create(
                TServiceTools.ifthen<string>(obj.dimensions.measures[j]._Label<>'', obj.dimensions.measures[j]._Label, oprsIds[j].Name+' of '+obj.dimensions.measures[j].Field),
                TServiceTools.Ifthen<THeaderType>(AggrStr.indexOf( lowerCase(oprsIds[j].Name))>-1,varDouble,fieldTypes[measIds[j]])
              )
             ,Aggs,length(Aggs));
          end;
        Insert(Aggs,result.headers.columns,length(result.headers.columns));
        for j:=0 to high(FCols[0]) do begin
              m_span:=length(FCols) -colStack[j].i;
              //with tmpAgg do begin Name:=colStack[j].v[j]; typ:=htString; span:=m_span*length(measIds) end;
              insert(THeader.Create(colStack[j].v[j], varString, m_span*length(measIds)),result.headers.columns[j],length(result.headers.columns[j]));
        end;

    end;
    {$ifdef Profiling} Profiler.Log('Cube Headers...');{$endif}

   if length(FRows)>0 then begin
     for j:=0 to TServiceTools.ifthen<Integer>(length(rowsIds)>0,length(rowsIds),TServiceTools.ifthen<Integer>(length(colsIds)>0,0,1)) do begin
          setLength(rowStack,j+1);rowStack[j].i:=0; rowStack[j].v:=copy(FRows[0],0,j+1)
     end;
     i:=0;
     setLength(result.headers.rows,length(FRows[0]));
     for i:=0 to high(FRows) do begin

         if FRows[i].indexOf(arrayCompFiller)>-1 then insert(i,result.rowsHeadSubTotals,length(result.rowsHeadSubTotals));
         for j:= 0 to high(FRows[i]) do begin
           if (j<length(rowStack)) and (arrayComp(copy(FRows[i], 0 ,j+1),rowStack[j].v, false,false)<>0) then begin
              m_span:= i-rowStack[j].i;
              //if m_span=0 then
              //  beep;              // for debugging
              //setLength(result.headers.rows[j],length(result.headers.rows[j])+1);
              //with result.headers.rows[j][high(result.headers.rows[j])] do begin
              //  name := rowStack[j].v[j];
              //  span:=m_span;
              //  typ:=htString;
              //end;
              insert(THeader.create(rowStack[j].v[j],varString, m_span),result.headers.rows[j],length(result.headers.rows[j]));
              setLength(result.headers.rows[j],high(result.headers.rows[j])+m_span) ;

              rowStack[j].i:=i;rowStack[j].v:=copy(FRows[i],0,j+1);
           end;
          end
     end;
     for j:=0 to high(FRows[0]) do begin
       m_span:=length(FRows) -rowStack[j].i;
       //if m_span=0 then
       //  beep;              // for debugging
       //setLength(result.headers.rows[j],length(row[j])+1);
       //with result.headers.rows[j][high(result.headers.rows[j])] do
       //  begin name:= rowStack[j].v[j]; typ:=htString; Span:=m_span end;
       insert(THeader.Create(rowStack[j].v[j], varString, m_span),result.headers.rows[j],length(result.headers.rows[j]));
       setLength(result.headers.rows[j],high(result.headers.rows[j])+m_span);
     end;
   end;
   {$ifdef Profiling} Profiler.Log('Cube SubTotals ...');{$endif}
   resultValues:=result.results.Values.ToArray;
   for i:=0 to high(resultValues) do begin
       for j:=0 to high(oprsIds) do begin
       if LowerCase(oprsIds[j].Name)='count' then
         resultValues[i].reducedMeasures[j]:=length(resultValues[i].TableData[j]{$ifdef TABLEDATA2}.Data{$endif})//reduce<Variant>(result.results.ValueList[i].TableData[j], oprsIds[j].operation, 0)
       else if LowerCase(oprsIds[j].Name)='distinct_count' then
         resultValues[i].reducedMeasures[j]:=Length(resultValues[i].TableData[j]{$ifdef TABLEDATA2}.Data{$endif}.unique())//reduce<Variant>(result.results.ValueList[i].TableData[j], oprsIds[j].operation, 0)
       else if LowerCase(oprsIds[j].Name)='concat' then
         resultValues[i].reducedMeasures[j]:=resultValues[i].TableData[j]{$ifdef TABLEDATA2}.Data{$endif}.tostring(', ','',false)
       else if LowerCase(oprsIds[j].Name)='distinct_concat' then
         resultValues[i].reducedMeasures[j]:=resultValues[i].TableData[j]{$ifdef TABLEDATA2}.Data{$endif}.unique().toString(', ','',false)
       else
         resultValues[i].reducedMeasures[j]:=resultValues[i].TableData[j]{$ifdef TABLEDATA2}.Data{$endif}.reduce(oprsIds[j].operation)
       end;
   end;
   {$ifdef Profiling} Profiler.Log('Cube Reduction calculated');{$endif}

   for i:=0 to high(FRows) do begin
     setLength(Rw,0);
     for j:=0 to high(FCols) do begin
       if not result.results.TryGetValue(FRows[i].concat(FCols[j]).ToString(splitter,'',false),id) then
         id.reducedMeasures:=TDataRecord.Fill(length(measIds),'');
       for k:=0 to high(id.reducedMeasures) do
         insert(id.reducedMeasures[k],Rw,length(rw))
     end;
     insert(Rw,result.data,length(result.data));
   end;
   if length(colsIds)>0 then
     for i:=0 to high(measIds) do
       insert((TServiceTools.arrayLookup(FCols ,TDataRecord.Fill(length(colsIds),arrayCompFiller)))*length(measIds)+i,result.colsHeadGrandTotals,length(result.colsHeadGrandTotals));
       //insert((result.cols.lookup(TDataRecord.Fill(length(colsIds),arrayCompFiller)))*length(measIds)+i,FColsHeadGrandTotals,length(FColsHeadGrandTotals));
   if length(rowsIds)>0 then
     Insert(TServiceTools.arrayLookup(FRows,TDataRecord.Fill(length(rowsIds),arrayCompFiller)),result.rowsHeadGrandTotals,length(result.rowsHeadGrandTotals));
     //Insert(FRows.lookup(TDataRecord.Fill(length(rowsIds),arrayCompFiller)),result.rowsHeadGrandTotals,length(result.rowsHeadGrandTotals));
   {$ifdef Profiling} Profiler.Log('Cube GrandTotals prepared');{$endif}
   calcVals;
   result.Dimensions:=obj.Dimensions;
   {$ifdef Profiling} Profiler.Log('Cube finalized');{$endif}

end;

function cube(const obj:TObjData):TObjData;
begin
   result:=cube(obj,defaultTotalsOptions)
end;
*)

function _sum(const a, b: variant; const i: integer; const Arr: array of variant):variant;
begin
  result:=double(a)+double(b)
end;

function _max(const a, b: variant; const i: integer; const Arr: array of variant):variant;
begin
  result:=TServiceTools.ifthen<variant>(a>=b,a,b)
end;

function _min(const a, b: variant; const i: integer; const Arr: array of variant):variant;
begin
  result:=TServiceTools.ifthen<variant>(a<=b,a,b)
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
  result:=TServiceTools.ifthen<variant>(varIsEmpty(a),0,a);
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

function TVariantArrayHelper.Lookup(const aValue: Variant): Integer;
begin
  result:=TServiceTools.BinSearch<Variant>(Self,aValue,Length(Self));
end;

function TVariantArrayHelper.Sort: TSelf;
begin
  {$ifdef fpc}
  TServiceTools.QuickSort<Variant>(Self, 0, High(Self), false, compare)
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

function TStringArrayHelper.Lookup(const str: string; const CaseSensitive: boolean):{$ifdef fpc}SizeInt{$else}integer{$endif};
begin

   if not {$ifdef fpc}
     (TArrayHelper<string>.BinarySearch(Self,str,result))
     {$else}
     TArray.BinarySearch<string>(Self,str,result)
     {$endif} then
     result:=-1
end;

const
    aggSum            :TAggregation=    (name:'Sum'              ;typ:varEmpty; operation:_sum);
    aggMax            :TAggregation=    (name:'Max'              ;typ:varEmpty; operation:_max);
    aggMin            :TAggregation=    (name:'Min'              ;typ:varEmpty; operation:_min);
    aggFirst          :TAggregation=    (name:'First'            ;typ:varEmpty; operation:_first);
    aggLast           :TAggregation=    (name:'Last'             ;typ:varEmpty; operation:_last);
    aggCount          :TAggregation=    (name:'Count'            ;typ:varInt64; operation:nil);
    aggDistinctCount  :TAggregation=    (name:'Distinct_Count'   ;typ:varInt64; operation:nil);
    aggConcat         :TAggregation=    (name:'Concat'           ;typ:varString; operation:nil);
    aggDistinctConcat :TAggregation=    (name:'Distinct_Concat'  ;typ:varString; operation:nil);
    aggAverage        :TAggregation=    (name:'Mean'             ;typ:varDouble; operation:_average);
    aggMedian         :TAggregation=    (name:'Median'           ;typ:varDouble; operation:_median);
    aggMode           :TAggregation=    (name:'Mode'             ;typ:varEmpty; operation:_mode);
    aggStdDev         :TAggregation=    (name:'StdDev'           ;typ:varDouble; operation:_stddev);


{ TOLAPDimensions }


procedure TOLAPDimensions.SetOnChange(AValue: TOLAPDimensionsChange);
begin
  //if FOnChange=AValue then Exit;
  FOnChange:=AValue;
end;

procedure TOLAPDimensions.DoColChange(AValue: TOLAPDimension);
begin
  if assigned(FOnChange) then
    FOnChange(Self,FCols,False)
end;

procedure TOLAPDimensions.DoRowChange(AValue: TOLAPDimension);
begin
  if assigned(FOnChange) then
    FOnChange(Self,FRows,True)
end;

constructor TOLAPDimensions.Create;
begin
  FCols:=TOLAPDimension.Create;
  FCols.FOnChange:=DoColChange;
  FRows:=TOLAPDimension.Create;
  FRows.OnChange:=DoRowChange;
end;

procedure TOLAPDimensions.Clear;
begin
  FRows.Clear;
  FCols.Clear;
end;

destructor TOLAPDimensions.Destroy;
begin
  FreeAndNil(FCols);
  FreeAndNil(FRows);
  inherited Destroy;
end;

{ TOLAPDimension }

function TOLAPDimension.GetFieldNames(Index: integer): string;
begin
  result:=FFieldNames[Index];
end;

procedure TOLAPDimension.SetFieldNames(Index: integer; AValue: string);
begin
  FFieldNames[Index]:=AValue;
  if assigned(FOnChange) then FOnChange(Self)
end;

procedure TOLAPDimension.SetFieldNames(AValue: TStringDynArray);
begin
  if FFieldNames=AValue then Exit;
  FFieldNames:=AValue;
    if assigned(FOnChange) then FOnChange(Self)
end;

procedure TOLAPDimension.SetOnChange(AValue: TOLAPDimensionChange);
begin
  //if FOnChange=AValue then Exit;
  if assigned(FOnChange) then FOnChange:=AValue;
end;

function TOLAPDimension.Add(const FieldName: string): integer;
begin
  Insert(FieldName,FFieldNames,length(FFieldNames));
  if assigned(FOnChange) then FOnChange(Self);
end;

procedure TOLAPDimension.Clear;
begin
  FFieldNames:=nil;
  if Assigned(FOnChange) then FOnChange(Self)
end;

function TOLAPDimension.Count: integer;
begin
  result:=Length(FFieldNames)
end;

function TOLAPDimension.IndexOf(FieldName: string): integer;
begin
  result:=TServiceTools.IndexOf<string>(FFieldNames,FieldName);
end;

procedure TOLAPDimension.Remove(const Index: integer);
begin
  Delete(FFieldNames,Index,1);
  if assigned(FOnChange) then FOnChange(Self)
end;

{ TOLAPMeasures }

function TOLAPMeasures.GetMeasures(Index: integer): TMeasure;
begin
  result:=FMeasures[Index]
end;

procedure TOLAPMeasures.SetMeasures(Index: integer; AValue: TMeasure);
begin
  FMeasures[Index]:=AValue;
  if assigned(FOnChange) then FOnChange(Self)
end;

procedure TOLAPMeasures.SetOnChange(AValue: TOLAPMeasureChange);
begin
  //if FOnChange=AValue then Exit;
    FOnChange:=AValue;
end;

function TOLAPMeasures.Add(aMeasure: TMeasure): integer;
begin
  Insert(AMeasure,FMeasures,Length(FMeasures));
  result:=Length(FMeasures);
  if assigned(FOnChange) then
    FOnChange(Self)  ;
end;

procedure TOLAPMeasures.Clear;
begin
  FMeasures:=nil;
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TOLAPMeasures.Count: integer;
begin
  Result:=Length(FMeasures)
end;

function TOLAPMeasures.IndexOf(FieldName: string): integer;
begin
  for result:=0 to High(FMeasures) do
    if FieldName=FMeasures[result].Field then
      exit;
  result:=1;
end;

procedure TOLAPMeasures.Remove(const Index: integer);
begin
  Delete(FMeasures,Index,1);
  if assigned(FOnChange) then FOnChange(Self)
end;

{ TOLAP }

procedure TOLAP.SetDataset(AValue: TDataset);
begin
  if FDataset=AValue then Exit;
  FDataset:=AValue;
end;

procedure TOLAP.SetActive(AValue: boolean);
begin
  if FActive=AValue then Exit;
  if AValue then Open else Close;
  FActive:=AValue;
end;

procedure TOLAP.SetFilters(AValue: TFilters);
begin
  if FFilters=AValue then Exit;
  FFilters:=AValue;
end;

procedure TOLAP.SetOnDimnsionsChanged(
  AValue: TOLAPDimensions.TOLAPDimensionsChange);
begin
  //if FOnDimnsionsChanged=AValue then Exit;
  FOnDimnsionsChanged:=AValue;
end;

procedure TOLAP.SetOnFilterChange(AValue: TOLAPNotifyEvent<TOLAP>);
begin
  //if FOnFilterChange=AValue then Exit;
  FOnFilterChange:=AValue;
end;

procedure TOLAP.SetOnMeasuersChanged(AValue: TOLAPNotifyEvent<TOLAPMeasures>);
begin
  //if FOnMeasuersChanged=AValue then Exit;
  FOnMeasuersChanged:=AValue;
end;

const FieldTypetoVarType:array[ftUnknown..ftWideMemo] of TVarType =
  ( varUnknown,    varString,   varSmallint,    varInteger, varWord,
    varBoolean,    varDouble,    varDouble,    varDouble,     varDate,          varDate,    varDate,
    varArray or varByte,      varArray or varByte, varInt64,     varString,    varString,          varString, varString,
    varUnknown, varUnknown, varUnknown, varUnknown,  varString,
    varUString, varInt64, varUnknown,         varUnknown,   varUnknown,
    varUnknown,    varString,  varUnknown,     varVariant, varUnknown,
    varDispatch,  varUnknown,     varDate,   varUnknown,  varUString, varUString)  ;

procedure TOLAP.InitAllocations;
var i:integer;
  FieldNames:TStringDynarray;
  co,ro:TDataRecord;
begin
  setLength(FieldNames,length(Headers.Columns[0]));
  setLength(FFieldTypes,length(FieldNames));

  for i:=0 to FDataset.FieldCount-1 do begin
     FieldNames[i]:=FDataset.Fields[i].FieldName;
     FFieldTypes[i]:=FieldTypetoVarType[FDataset.Fields[i].DataType]
  end;

  setLength(FRowsIds,FDimensions.FRows.Count);for i:=0 to FDimensions.FRows.Count-1 do begin
   FRowsIds[i]:=FieldNames.indexOf(FDimensions.FRows[i]);
   if FRowsIds[i]<0 then raise Exception.CreateFmt('Field [%s] not found!',[FDimensions.FRows[i]]);
  end;
  setLength(FColsIds,FDimensions.FCols.Count);for i:=0 to FDimensions.FCols.Count-1 do begin
   FColsIds[i]:=FieldNames.indexOf(FDimensions.FCols[i]);
   if FColsIds[i]<0 then raise Exception.CreateFmt('Field [%s] not found!',[FDimensions.FCols[i]]);
  end;
  setLength(FMeasIds,FMeasures.Count); setLength(FOprsIds,FMeasures.Count);
  for i:=0 to FMeasures.Count-1 do begin
   FMeasIds[i]:=FieldNames.indexOf(FMeasures[i].Field);
   if FMeasIds[i]<0 then raise Exception.CreateFmt('Field [%s] not found!',[FMeasures[i].Field]);
   FOprsIds[i]:=Fmeasures[i].Operation;
   if FOprsIds[i].typ=varEmpty then
     FOprsIds[i].typ:=FFieldTypes[FMeasIds[i]];
  end;
  setLength(FFiltIds,length(FFilters));
  for i:=0 to High(FFilters) do
   FFiltIds[i]:=FieldNames.indexOf(FFilters[i].FieldName);

  //Insert(TDataRecord.fill(TServiceTools.ifthen<Integer>(FOptions.grandRows,TServiceTools.ifthen<Integer>(length(FRowsIds)>0,length(FRowsIds),TServiceTools.ifthen<Integer>(length(FColsIds)>0,0,1)),1),arrayCompFiller),FRows,length(FRows)); //grand total columns
  //Insert(TDataRecord.fill(TServiceTools.ifthen<Integer>(FOptions.grandCols,length(FColsIds),0),arrayCompFiller),FCols,length(FCols)); //grand total Frows
  // ToDo : make case for Measures headers as rows instead of columns;
  ro:=TDataRecord.fill(TServiceTools.ifthen<Integer>(FOptions.grandRows,TServiceTools.ifthen<Integer>(length(FRowsIds)>0,length(FRowsIds),TServiceTools.ifthen<Integer>(length(FColsIds)>0,0,1)),1),arrayCompFiller);
  co:=TDataRecord.fill(TServiceTools.ifthen<Integer>(FOptions.grandCols,length(FColsIds),0),arrayCompFiller);
  FRows.tryAdd(ro.toString(),ro);
  FCols.tryAdd(co.ToString(),co);
  // add grand total row anyway (horizonal measures case)
  setLength(FRSort,FDimensions.FRows.Count);for i:=0 to FDimensions.FRows.Count-1 do FRSort[i]:=FDescends.IndexOf(FDimensions.FRows[i])>=0;
  setLength(FCSort,FDimensions.FCols.Count);for i:=0 to FDimensions.FCols.Count-1 do FCSort[i]:=FDescends.IndexOf(FDimensions.FCols[i])>=0;

end;

procedure TOLAP.PrepareHeaders;
var
  i,j,k:integer;
  bm:TBookMark;
  Row,Col,r,c:TDataRecord;
begin
  setLength(Row,length(FRowsIds));
  setLength(Col,Length(FColsIds));
  FDataset.DisableControls;
  bm:=Dataset.GetBookmark;
  FDataset.first;
  while not FDataset.EOF do begin
     if isFiltered then continue;

     for i:=0 to high(FRowsIds) do
       row[i]:= FDataset.Fields[FRowsIds[i]].Value;     //take values of selected rows into row[]

     for i:=0 to high(FColsIds) do                //take values of selected columns into columns[]
       col[i]:= FDataset.Fields[FColsIds[i]].Value;

     // calculate subtotal rows
     for i:=TServiceTools.ifthen<Integer>(FOptions.subRows, 1, length(FRowsIds)) to length(FRowsIds) do begin
         r:=copy(row,0,i).concat(TDataRecord.Fill(length(FRowsIds)-i,arrayCompFiller));
         // ToDo -cPerformance : explore using TArray<FieldType> instead of TVariantArray?
         //r:=slice<TDataRecord>(row,0,i).concat(TDataRecord.Fill(length(FRowsIds)-i,arrayCompFiller));  // explore using TArray<FieldType> instead of TVariantArray?

         FRows.tryAdd(r.toString(splitter),r);
         //j:=TServiceTools.arrayLookup(FRows,r,FRSort,FOptions.br);
         //if j<0 then
         //  insert(r,FRows,-j-1);
           //splice<TTableData>(result.rows,-j-1,0,[r]);
     end;

     // calculate subtotal columns
     for i:=TServiceTools.ifthen<Integer>(FOptions.subCols,1,length(FColsIds)) to length(FColsIds) do begin
         c:=copy(col,0,i).concat(TDataRecord.fill(length(FColsIds)-i,arrayCompFiller));
         //c:=slice<TDataRecord>(col,0,i).concat(TDataRecord.fill(length(FColsIds)-i,arrayCompFiller));
         FCols.tryAdd(c.toString(splitter),c);
         //k:=TServiceTools.arrayLookup(FCols,c,FCSort,FOptions.bc);
         //if k<0 then
         //  Insert(c,FCols,-k-1);
           //splice<TTableData>(result.cols,-k-1,0,[c]);
     end;
     FDataset.Next
  end;
  _Rows:=FRows.Values.ToArray;
  _Cols:=FCols.Values.ToArray;
  TServiceTools.arraySort(_Rows,0,High(_Rows),FRSort,FOptions.br);
  TServiceTools.arraySort(_Cols,0,High(_Cols),FCSort,FOptions.bc);
  FDataset.GotoBookmark(bm);
  FDataset.EnableControls
end;

procedure TOLAP.ScanDataset;
var
  i, j, k: Integer;
  row,col,rw:TDataRecord;
  id:TResults;
  cub:TTableData;rCube:string;
  bm:TBookmark;
  resultValues:TArray<TResults>;
  val:Variant;
begin
  setLength(Row,length(FRowsIds));
  setLength(Col,Length(FColsIds));
  FDataset.DisableControls;
  bm:=Dataset.GetBookmark;
  FDataset.First;
  while not FDataset.EOF do begin
     if isFiltered then continue;
     {$ifdef Profiling}
                 Profiler.Log(0);
     {$endif}
     for i:=0 to high(FRowsIds) do
       row[i]:= FDataset.Fields[FRowsIds[i]].Value;     //take values of selected rows into row[]

     for i:=0 to high(FColsIds) do                //take values of selected columns into columns[]
       col[i]:= FDataset.Fields[FColsIds[i]].Value;
     {$ifdef Profiling}
                 Profiler.Log(1);
     {$endif}
     // ********************* check factorial array multiplication from here

     rw:=copy(row,0,length(FRowsIds)).concat(copy(col,0,length(FColsIds)));
     if length(rw)>0 then
       k:=1 shl Length(rw)
     else
       k:=TServiceTools.ifthen<integer>(length(FColsIds)>0,1,2);

     cub:=Dice(rw,arrayCompFiller,k);
     {$ifdef Profiling}
     Profiler.Log(2);
     {$endif}
     for i:=0 to high(cub) do  begin
         rCube:=cub[i].toString(splitter,'',false);
         if not FResults.tryGetValue(rCube,id) then begin
           id:=TResults.Create(length(FMeasIds),FDataset.RecordCount div (length(_Cols) + length(_Rows)) );
           FResults.Add(rCube,id);
         end;
         {$ifdef Profiling}Profiler.Log(4);{$endif}
         for j:=0 to High(FMeasIds) do begin
           val:=FDataset.Fields[FMeasIds[j]].Value;
           if not varIsEmpty(val) then
             {$ifdef TABLEDATA2}
             id.TableData[j].Push(val);
             {$else}
             insert(val,id.TableData[j],length(id.TableData[j]));
             {$endif}
           //id:=result.results[rCube];
           {$ifdef Profiling}Profiler.Log(5);{$endif}
         end;
     end;
     {$ifdef Profiling}
                 Profiler.Log(3);
     {$endif}
    FDataset.Next
  end;//end scanning table
  FDataset.GotoBookmark(bm);
  FDataset.EnableControls;
  {$ifdef TABLEDATA2}
  resultValues:=FResults.Values.ToArray;
  for i:=0 to high(resultValues) do
    for j:=0 to high(resultValues[i].TableData) do
      resultValues[i].TableData[j].Shrink;
  {$ifdef Profiling}
  Profiler.Log('Shrinking...');
  {$endif}
  {$endif}
end;

procedure TOLAP.ApplyHeaders;
var
  i,j,m_span: Integer;
  Aggs:THeaders;
  colStack, rowStack:TArray<TRecordStack>;
begin
  if Length(_Cols)>0 then begin
      setLength(colStack,length(_Cols[0]));
      for j:=0 to high(_Cols[0]) do begin
          colStack[j].i:=0;
          colStack[j].v:=copy(_Cols[0],0,j+1)
      end;//i=0;
      for i:=0 to high(_Cols) do begin
          setLength(Fheaders.columns,Length(_Cols[0]));
          for j:=0 to high(_Cols[0]) do begin
              if arrayComp(copy(_Cols[i],0,j+1),colStack[j].v,false,false)<>0 then begin
                  m_span:=i-colStack[j].i;
                  Insert(THeader.Create({name:=}colStack[j].v[j],{typ:=}TVarData(colStack[j].v[j]).vtype,{span:=}m_span * length(FMeasIds)),Fheaders.columns[j],Length(FHeaders.columns[j]));
                  colStack[j].i:=i;colStack[j].v:=copy(_Cols[i],0,j+1);
              end
          end;
          // column identify subtotals
          if _Cols[i].indexOf(arrayCompFiller)>-1 then
              for j:=0 to high(FmeasIds) do
                    Insert(i*length(FmeasIds)+j,FcolsHeadSubTotals,length(FcolsHeadSubTotals))
      end;
      for i:=0 to high(_Cols) do
        for j:=0 to Fmeasures.Count-1 do begin
          //tmpAgg.Name:=ifthen<string>(dimensions.measures[j]._Label<>'', dimensions.measures[j]._Label, oprsIds[j].Name+' of '+dimensions.measures[j].Field);
          //tmpAgg.typ:=Ifthen<THeaderType>(IndexOf<string>(['sum','count','distinct_count','distinct_sum','mean','average','mean','stddev','mode','median'], lowerCase(oprsIds[j].Name))>-1,htDouble,fieldTypes[measIds[j]]);
          insert(
            THeader.Create(
              TServiceTools.ifthen<string>(FMeasures[j]._Label<>'', FMeasures[j]._Label, FOprsIds[j].Name+' of '+FMeasures[j].Field),
              FMeasures[j].typ
              //TServiceTools.Ifthen<THeaderType>(AggrStr.indexOf( lowerCase(FOprsIds[j].Name))>-1,htDouble,FfieldTypes[FMeasIds[j]])
            )
           ,Aggs,length(Aggs));
        end;
      Insert(Aggs,FHeaders.columns,length(FHeaders.columns));
      for j:=0 to high(_Cols[0]) do begin
            m_span:=length(_Cols) -colStack[j].i;
            //with tmpAgg do begin Name:=colStack[j].v[j]; typ:=htString; span:=m_span*length(measIds) end;
            insert(THeader.Create(colStack[j].v[j], TVarData(colStack[j].v[j]).vtype, m_span*length(FMeasIds)),FHeaders.columns[j],length(FHeaders.columns[j]));
      end;

  end;
  {$ifdef Profiling} Profiler.Log('Cube Columns subtotals...');{$endif}

 if length(_Rows)>0 then begin
   for j:=0 to TServiceTools.ifthen<Integer>(length(FrowsIds)>0,length(FrowsIds),TServiceTools.ifthen<Integer>(length(FcolsIds)>0,0,1)) do begin
        setLength(rowStack,j+1);rowStack[j].i:=0; rowStack[j].v:=copy(_Rows[0],0,j+1)
   end;
   i:=0;
   setLength(FHeaders.rows,length(_Rows[0]));
   for i:=0 to high(_Rows) do begin

       if _Rows[i].indexOf(arrayCompFiller)>-1 then insert(i,FRowsHeadSubTotals,length(FRowsHeadSubTotals));
       for j:= 0 to high(_Rows[i]) do begin
         if (j<length(rowStack)) and (arrayComp(copy(_Rows[i], 0 ,j+1),rowStack[j].v, false,false)<>0) then begin
            m_span:= i-rowStack[j].i;
            //if m_span=0 then
            //  beep;              // for debugging
            //setLength(result.headers.rows[j],length(result.headers.rows[j])+1);
            //with result.headers.rows[j][high(result.headers.rows[j])] do begin
            //  name := rowStack[j].v[j];
            //  span:=m_span;
            //  typ:=htString;
            //end;
            insert(THeader.create(rowStack[j].v[j],TVarData(rowStack[j].v[j]).vtype, m_span),FHeaders.rows[j],length(FHeaders.rows[j]));
            setLength(FHeaders.rows[j],high(FHeaders.rows[j])+m_span) ;

            rowStack[j].i:=i;rowStack[j].v:=copy(_Rows[i],0,j+1);
         end;
        end
   end;
   for j:=0 to high(_Rows[0]) do begin
     m_span:=length(_Rows) -rowStack[j].i;
     //if m_span=0 then
     //  beep;              // for debugging
     //setLength(result.headers.rows[j],length(row[j])+1);
     //with result.headers.rows[j][high(result.headers.rows[j])] do
     //  begin name:= rowStack[j].v[j]; typ:=htString; Span:=m_span end;
     insert(THeader.Create(rowStack[j].v[j], TVarData(rowStack[j].v[j]).vType, m_span),FHeaders.rows[j],length(FHeaders.rows[j]));
     setLength(FHeaders.rows[j],high(FHeaders.rows[j])+m_span);
   end;
 end;
 {$ifdef Profiling} Profiler.Log('Cube Rows SubTotals ...');{$endif}

end;

procedure TOLAP.ExecuteReduction;
var
  i,j:Integer;
  resultValues:TArray<TResults>;
begin
  resultValues:=FResults.Values.ToArray;
  for i:=0 to High(resultValues) do begin
     for j:=0 to high(FOprsIds) do begin
     if LowerCase(FOprsIds[j].Name)='count' then
       resultValues[i].ReducedMeasures[j]:=length(resultValues[i].TableData[j]{$ifdef TABLEDATA2}.Data{$endif})//reduce<Variant>(result.results.ValueList[i].TableData[j], FOprsIds[j].operation, 0)
     else if LowerCase(FOprsIds[j].Name)='distinct_count' then
       resultValues[i].ReducedMeasures[j]:=Length(resultValues[i].TableData[j]{$ifdef TABLEDATA2}.Data{$endif}.unique())//reduce<Variant>(result.results.ValueList[i].TableData[j], FOprsIds[j].operation, 0)
     else if LowerCase(FOprsIds[j].Name)='concat' then
       resultValues[i].ReducedMeasures[j]:=resultValues[i].TableData[j]{$ifdef TABLEDATA2}.Data{$endif}.tostring(', ','',false)
     else if LowerCase(FOprsIds[j].Name)='distinct_concat' then
       resultValues[i].ReducedMeasures[j]:=resultValues[i].TableData[j]{$ifdef TABLEDATA2}.Data{$endif}.unique().toString(', ','',false)
     else
       resultValues[i].ReducedMeasures[j]:=resultValues[i].TableData[j]{$ifdef TABLEDATA2}.Data{$endif}.reduce(FOprsIds[j].operation)
     end;
  end;
  {$ifdef Profiling} Profiler.Log('Cube Reduction calculated');{$endif}
end;

procedure TOLAP.FillData;
var i,j,k:Integer;Rw:TDataRecord;Id:TResults;
begin
 for i:=0 to high(_Rows) do begin
   setLength(Rw,0);
   for j:=0 to high(_Cols) do begin
     if not Fresults.TryGetValue(_Rows[i].concat(_Cols[j]).ToString(splitter,'',false),id) then
       id.ReducedMeasures:=TDataRecord.Fill(length(FMeasIds),'');
     for k:=0 to high(id.ReducedMeasures) do
       insert(id.ReducedMeasures[k],Rw,length(rw))
   end;
   insert(Rw,FData,length(FData));
 end;
 {$ifdef Profiling} Profiler.Log('Data Filled...');{$endif}

end;

procedure TOLAP.FillGrandTotals;
var i,j:integer;
begin
  if length(FColsIds)>0 then
    for i:=0 to high(FMeasIds) do
      insert((TServiceTools.arrayLookup(_Cols ,TDataRecord.Fill(length(FColsIds),arrayCompFiller)))*length(FMeasIds)+i,FColsHeadGrandTotals,length(FColsHeadGrandTotals));
      //insert((result.cols.lookup(TDataRecord.Fill(length(FColsIds),arrayCompFiller)))*length(FMeasIds)+i,result.colsHeadGrandTotals,length(result.colsHeadGrandTotals));
  if length(FRowsIds)>0 then
    Insert(TServiceTools.arrayLookup(_Rows,TDataRecord.Fill(length(FRowsIds),arrayCompFiller)),FRowsHeadGrandTotals,length(FRowsHeadGrandTotals));
    //Insert(result.rows.lookup(TDataRecord.Fill(length(rowsIds),arrayCompFiller)),result.rowsHeadGrandTotals,length(result.rowsHeadGrandTotals));
end;

procedure TOLAP.CalculateFormulas;
var m,r,c:integer;formula:string;v:TDataRecord;vv:Variant;
begin
  for m:=0 to FMeasures.Count-1 do begin
      for r:=0 to High(FData) do begin
          formula:=FMeasures[m].formula;
          if formula<>'' then
            for c:=0 to High(_Cols) do
              begin
                  v:=_Cols[c];
                  if FData[r][c*FMeasures.Count+m]='' then exit;
                  FData[r][c*FMeasures.Count+m]:=eval(
                          formula.replace('{val}',FData[r][c*FMeasures.Count+m],[rfReplaceAll])
                          .replace('{colTotal}',FResults[TDataRecord.Fill(FDimensions.FRows.Count,arrayCompFiller).concat(v).ToString('#9','',false)].ReducedMeasures[m],[rfReplaceAll])
                          .replace('{rowTotal}',FResults[_Rows[r].concat(TDataRecord.Fill(FDimensions.FCols.Count,arrayCompFiller)).ToString('#9','',false)].ReducedMeasures[m],[rfReplaceAll])
                          .replace('{grandTotal}',FResults[TDataRecord.Fill(FDimensions.FCols.Count+FDimensions.FRows.Count,arrayCompFiller).ToString('#9','',false)].ReducedMeasures[m],[rfReplaceAll])
                  );
              end;
          if FMeasures[m].decimals>=0 then
              for c:=0 to High(_Cols) do begin
                  vv:=FData[r][c*FMeasures.Count+m];
                  if (TVarData(vv).vtype in [varEmpty,varNull])
                    or VarIsStr(vv) then
                      exit;
                  if TVarData(vv).vtype in varsOrdinal then
                    FData[r][c*FMeasures.Count+m]:=format('%.0n',[double(vv)])
                  else if TVarData(vv).vtype in varsFloats then
                    FData[r][c*FMeasures.Count+m]:=format('%.'+FMeasures[m].decimals.ToString+'n',[double(vv)]) ;
              end;
      end
  end;

end;

function TOLAP.isFiltered: boolean;
var
  i: Integer;
begin
  Result:=False;
  for i:=0 to high(FFiltIds) do
    if FFilters[i].data.Lookup(FDataset.Fields[FFiltIds[i]].Value)<0 then
      begin result:=true;break;end;
//if filtRow then begin filtRow:=false;continue end;                  // if filter list is inroduced then skip elements not in the filter list
end;

constructor TOLAP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDimensions:=TOLAPDimensions.Create;
  FRows:=TDimensionSet.Create;
  FCols:=TDimensionSet.Create;
  FMeasures:=TOLAPMeasures.Create;
  FOnDimnsionsChanged:=FDimensions.OnChange;
end;

destructor TOLAP.Destroy;
begin
  if Assigned(FDimensions) then
    FreeAndnil(FDimensions);
  if Assigned(FRows) then
    FreeAndnil(FRows);
  if Assigned(FCols) then
    FreeAndnil(FCols);
  if Assigned(FMeasures) then
    FreeAndnil(FDimensions);
  inherited Destroy;
end;

procedure TOLAP.Open;
begin
  FResults:=TResultData.Create;
  InitAllocations;
  PrepareHeaders;
  ApplyHeaders;
  ScanDataset;
  ExecuteReduction;
  FillData;
  FillGrandTotals;
  CalculateFormulas;


 //cube(TObjData,defaultTotalsOptions);
end;

procedure TOLAP.Close;
begin
  if Assigned(FResults) then
    FreeandNil(FResults);
  FHeaders.Clear;
  FCols:=nil;
  FRows:=nil;
  FData:=nil

end;

var i:integer;

{ TResultHeaders }

procedure TResultHeaders.Clear;
begin
  Columns:=nil; //Setting arrays out of scope is enough to release the allocated memory
  Rows:=nil
end;


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
    Insert(AggregationArray[i].Name,AggrStr,length(AggrStr))   ;


end.

