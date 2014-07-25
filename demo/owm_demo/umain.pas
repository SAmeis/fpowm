unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, owmurloptions, owmdata, urloptionsedit;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnQuery: TButton;
    OptFind: TOWMFindOptions;
    OptFC3h: TOWMForecastOptions;
    OptFCDaily: TOWMForecastOptions;
    OptGroup: TOWMGroupOptions;
    OptHistoryCity: TOWMHistoryOptions;
    OptWeather: TOWMWeatherOptions;
    OptHistoryStation: TOWMHistoryOptions;
    pgOutput: TPageControl;
    tsForecastDaily: TTabSheet;
    tsForecast3h: TTabSheet;
    tsHistoryCity: TTabSheet;
    tsWeather: TTabSheet;
    tsFind: TTabSheet;
    tsGroup: TTabSheet;
    tsHistoryStation: TTabSheet;
    PropGridOptions: TTIPropertyGrid;
    procedure pgOutputChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  LCLProc;
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

end.

