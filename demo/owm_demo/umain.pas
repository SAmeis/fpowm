unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Grids, owmurloptions, owmdata, urloptionsedit;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnQuery: TButton;
    OptFind: TOWMFindOptions;
    OptFC3h: TOWMForecast3hOptions;
    OptFCDaily: TOWMForecastDailyOptions;
    OptGroup: TOWMGroupOptions;
    OptHistoryCity: TOWMHistoryCityOptions;
    OptWeather: TOWMWeatherOptions;
    OptHistoryStation: TOWMHistoryStationOptions;
    P3hHead: TPanel;
    pgOutput: TPageControl;
    SgForecast3h: TStringGrid;
    tsForecastDaily: TTabSheet;
    tsForecast3h: TTabSheet;
    tsHistoryCity: TTabSheet;
    tsWeather: TTabSheet;
    tsFind: TTabSheet;
    tsGroup: TTabSheet;
    tsHistoryStation: TTabSheet;
    PropGridOptions: TTIPropertyGrid;
    procedure btnQueryClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pgOutputChange(Sender: TObject);
  private
    fForecast3h: TForecast3hData;
    procedure SetForecast3h(aValue: TForecast3hData);
  private
    property Forecast3h: TForecast3hData read fForecast3h write SetForecast3h;
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  LCLProc, typinfo, fphttpclient;
{ TfrmMain }

procedure TfrmMain.pgOutputChange(Sender: TObject);
begin
  case pgOutput.PageIndex of
    0: PropGridOptions.TIObject := OptFC3h;
    1: PropGridOptions.TIObject := OptFCDaily;
    2: PropGridOptions.TIObject := OptWeather;
    3: PropGridOptions.TIObject := OptFind;
    4: PropGridOptions.TIObject := OptGroup;
    5: PropGridOptions.TIObject := OptHistoryStation;
    6: PropGridOptions.TIObject := OptHistoryCity;
  end;
end;

procedure TfrmMain.btnQueryClick(Sender: TObject);
var
  urlOpts: TOWMCustomOptions;
  Response: String;
  url: String;
begin
  urlOpts := TOWMCustomOptions(PropGridOptions.TIObject);
  if not Assigned(urlOpts) then exit;
  url := urlOpts.GetURL;
  Response := TFPHTTPClient.SimpleGet(url);
  case pgOutput.PageIndex of
    0: Forecast3h := StrToForcast3h(Response)
    //1:
    //2:
    //3:
    //4:
    //5:
    //6:
  end;

end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  OptFC3h.Query.Value := 'Odenthal';
  btnQuery.Click;
end;

procedure TfrmMain.SetForecast3h(aValue: TForecast3hData);

procedure SetPropToGrid(aRow, aCol: Integer; aObj: TObject; aProp: PPropInfo);
var
  s: String;
begin
  case aProp^.PropType^.Kind of
    tkChar,
    tkSString,
    tkLString,
    tkAString,
    tkWString,
    tkUString,
    tkWChar,
    tkUChar: s := GetStrProp(aObj, aProp);
    tkEnumeration,
    tkSet,
    tkInteger,
    tkInt64,
    tkQWord: s := IntToStr(GetOrdProp(aObj, aProp));
    tkFloat: s := FloatToStr(GetFloatProp(aObj, aProp));
    tkBool: s := BoolToStr(Boolean(GetOrdProp(aObj, aProp)));
    tkUnknown,
    tkMethod,tkVariant,tkArray,tkRecord,tkInterface,
    tkClass,tkObject,
    tkDynArray,tkInterfaceRaw,tkProcVar,
    tkHelper,tkFile,tkClassRef,tkPointer: s := '<unsupported>';
  end;
  SgForecast3h.Rows[aRow].Strings[aCol] := s;
end;
const
  rounds: Integer = 0;
var
  fci: TForecastItem;
  i: Integer;
  rowIndex: Integer;
  PropList: PPropList;
  PropCount: Integer;
  PropInfo: PPropInfo;
  PropIndex: Integer;
  ColIndex: Integer;
  SubPropList: PPropList;
  SubPropCount: Integer;
  SubPropInfo: PPropInfo;
  SubPropIndex: Integer;
begin
  if fForecast3h = aValue then Exit;
  fForecast3h.Free;
  fForecast3h := aValue;
  SgForecast3h.RowCount := aValue.list.Count;
  for i := 0 to aValue.Count - 1 do
  begin
    rowIndex := i;
    TCollectionItem(fci) := aValue.list.Items[i];

    PropCount := GetPropList(fci, PropList);
    if PropCount < SgForecast3h.ColCount then
      SgForecast3h.ColCount := PropCount;

    if PropCount = 0 then continue;

    PropIndex := 0;
    ColIndex := PropIndex + 1;
    repeat
      PropInfo := PropList^[PropIndex];

      if PropInfo^.PropType^.Kind = tkClass then
      begin
        SubPropCount := GetPropList(GetObjectProp(fci, PropInfo), SubPropList);
        if SubPropCount > 0 then
        begin
          SgForecast3h.ColCount := SgForecast3h.ColCount + SubPropCount -1;
          SubPropIndex := 0;
          repeat
            SubPropInfo := SubPropList^[SubPropIndex];
            if i = 0 then
              SgForecast3h.Rows[0].Strings[ColIndex] := PropInfo^.Name+'.'+SubPropInfo^.Name
            else
              SetPropToGrid(rowIndex, ColIndex, GetObjectProp(fci, PropInfo), SubPropInfo);

            inc(ColIndex);
            inc(SubPropIndex);
          until SubPropIndex = SubPropCount;
        end;
      end else
      begin
        if i = 0 then
          SgForecast3h.Rows[0].Strings[ColIndex] := PropInfo^.Name
        else
          SetPropToGrid(rowIndex, ColIndex, fci, PropInfo);
        inc(ColIndex );
      end;

      inc(PropIndex);
    until PropIndex = Propcount;

  end;
  SgForecast3h.AutoAdjustColumns;
end;

end.

