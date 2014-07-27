unit ufrmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Spin, owmurloptions, owmdata, typinfo, dateutils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    EQuery: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LFCD_clouds: TLabel;
    LFC3_0_dt: TLabel;
    LFCD_dt: TLabel;
    LFCD_eve: TLabel;
    LFCD_morn: TLabel;
    LFCD_humidity: TLabel;
    LFCD_max: TLabel;
    LFCD_min: TLabel;
    LFCD_pressure: TLabel;
    LFCD_night: TLabel;
    LFC3_0_temp: TLabel;
    LFC3_0_clouds: TLabel;
    LFCD_day: TLabel;
    LFC3_0_wdescr: TLabel;
    LFC3_0_min: TLabel;
    LFCD_wdescr: TLabel;
    LFC3_0_winddeg: TLabel;
    LFCD_deg: TLabel;
    LFCD_speed: TLabel;
    LFC3_0_wmain: TLabel;
    LFC3_0_humidity: TLabel;
    LFC3_0_grnd: TLabel;
    LFC3_0_sea: TLabel;
    LFC3_0_pressure: TLabel;
    LFC3_0_max: TLabel;
    LFC3_0_windspeed: TLabel;
    LFCD_wmain: TLabel;
    LFCD_id: TLabel;
    LFCD_lat: TLabel;
    LFCD_lon: TLabel;
    LFC3_name: TLabel;
    LFC3_id: TLabel;
    LFC3_lon: TLabel;
    LFC3_lat: TLabel;
    LFCD_name: TLabel;
    MeInput: TMemo;
    MeResult: TMemo;
    Panel2: TPanel;
    PcDisplay: TPageControl;
    Panel1: TPanel;
    RgEndPoint: TRadioGroup;
    RgEndPoint1: TRadioGroup;
    SeFC3H: TSpinEdit;
    SeFCD: TSpinEdit;
    TsForecastDaily: TTabSheet;
    TsForecast3h: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SeFC3HChange(Sender: TObject);
    procedure SeFCDChange(Sender: TObject);
  private
    fForecast3hData: TForecast3hData;
    fForecastDailyData: TForecastDailyData;
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses fphttpclient;
{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  options: TOWMCustomOptions;
  fco: TOWMForecastOptions absolute options;
  wo: TOWMWeatherOptions absolute options;
begin
  case RgEndPoint.ItemIndex of
    -1: exit;
      0: begin
        options := TOWMForecastOptions.Create(Self);
        fco.Query.Value := EQuery.Text;
        fco.Mode.Value := moJSON;
        fco.Units.Value := uoMetric;
        MeResult.Text := TFPHTTPClient.SimpleGet(OWM_BASE_URL+OWM_ENDPOINT_FORECAST+'?'+fco.ToString);
      end;
      1: begin
        options := TOWMForecastOptions.Create(Self);
        fco.Query.Value := EQuery.Text;
        fco.Mode.Value := moJSON;
        fco.Units.Value := uoMetric;
        MeResult.Text := TFPHTTPClient.SimpleGet(OWM_BASE_URL+OWM_ENDPOINT_FORECAST_DAILY+'?'+fco.ToString);
      end;
      2: begin
        options := TOWMWeatherOptions.Create(Self);
        wo.Query.Value := EQuery.Text;
        wo.Mode.Value := moJSON;
        wo.Units.Value := uoMetric;
        MeResult.Text := TFPHTTPClient.SimpleGet(OWM_BASE_URL+OWM_ENDPOINT_WEATHER+'?'+fco.ToString);
      end;
  end;
  options.Destroy;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  case RgEndPoint1.ItemIndex of
      0: begin
        FreeAndNil(fForecast3hData);
        fForecast3hData := StrToForcast3h(MeInput.Text);
        LFC3_name.Caption := fForecast3hData.city.name;
        LFC3_ID.Caption := fForecast3hData.city.id;
        LFC3_lon.Caption := FloatToStr(fForecast3hData.city.coord.lon);
        LFC3_lat.Caption := FloatToStr(fForecast3hData.city.coord.lat);
        SeFC3H.MaxValue := fForecast3hData.list.Count - 1;
        SeFC3H.Value := 0;
        SeFC3HChange(Self);
        PcDisplay.PageIndex := 0;
      end;
      1: begin
        FreeAndNil(fForecastDailyData);
        fForecastDailyData := StrToForcastDaily(MeInput.Text);
        LFCD_name.Caption := fForecastDailyData.city.name;
        LFCD_ID.Caption := fForecastDailyData.city.id;
        LFCD_lon.Caption := FloatToStr(fForecastDailyData.city.coord.lon);
        LFCD_lat.Caption := FloatToStr(fForecastDailyData.city.coord.lat);
        SeFCD.MaxValue := fForecastDailyData.list.Count - 1;
        SeFCDChange(Self);
        PcDisplay.PageIndex := 1;
      end;
  else
    exit;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  MeInput.Text := MeResult.Text;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fForecast3hData);
  FreeAndNil(fForecastDailyData);
end;

procedure TForm1.SeFC3HChange(Sender: TObject);
var
  fci: TForecastItem;
begin
  if not Assigned(fForecast3hData) then exit;
  if SeFC3H.Value > fForecast3hData.list.Count - 1 then exit;

  fci := TForecastItem(fForecast3hData.list.Items[SeFC3H.Value]);
  LFC3_0_dt.Caption := fci.dt_txt;
  LFC3_0_temp.Caption := FloatToStr(fci.main.temp);
  LFC3_0_clouds.Caption := IntToStr(fci.clouds.all);
  LFC3_0_wdescr.Caption := TWeather(fci.weather.Items[0]).description;
  LFC3_0_min.Caption := FloatToStr(fci.main.temp_min);
  LFC3_0_max.Caption := FloatToStr(fci.main.temp_max);
  LFC3_0_winddeg.Caption := FloatToStr(fci.wind.deg);
  LFC3_0_wmain.Caption := TWeather(fci.weather.Items[0]).main;
  LFC3_0_humidity.Caption := IntToStr(fci.main.humidity);
  LFC3_0_grnd.Caption := FloatToStr(fci.main.grnd_level);
  LFC3_0_sea.Caption := FloatToStr(fci.main.sea_level);
  LFC3_0_pressure.Caption := FloatToStr(fci.main.pressure);
  LFC3_0_windspeed.Caption := FloatToStr(fci.wind.speed);
end;

procedure TForm1.SeFCDChange(Sender: TObject);
var
  fcdi: TDailyForecastItem;
begin
  if not Assigned(fForecastDailyData) then exit;
  if SeFCD.Value > fForecastDailyData.list.Count - 1 then exit;

  fcdi := TDailyForecastItem(fForecastDailyData.list.Items[SeFCD.Value]);

  LFCD_clouds.Caption := FloatToStr(fcdi.clouds);
  LFCD_day.Caption := FloatToStr(fcdi.temp.day);
  LFCD_deg.Caption := FloatToStr(fcdi.deg);
  LFCD_dt.Caption :=  FormatDateTime('yyyy-mm-dd', UnixToDateTime(fcdi.dt), []);
  LFCD_eve.Caption := FloatToStr(fcdi.temp.eve);
  LFCD_humidity.Caption := FloatToStr(fcdi.humidity);
  LFCD_max.Caption := FloatToStr(fcdi.temp.max);
  LFCD_min.Caption := FloatToStr(fcdi.temp.min);
  LFCD_morn.Caption := FloatToStr(fcdi.temp.morn);
  LFCD_night.Caption := FloatToStr(fcdi.temp.night);
  LFCD_pressure.Caption := FloatToStr(fcdi.pressure);
  LFCD_speed.Caption := FloatToStr(fcdi.speed);
  LFCD_wdescr.Caption := TWeather(fcdi.weather.Items[0]).description;
  LFCD_wmain.Caption := TWeather(fcdi.weather.Items[0]).main;

end;


end.

