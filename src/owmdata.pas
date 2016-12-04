unit owmdata;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}
interface

uses
  Classes, SysUtils, fpjson;

type

  { TBaseData }

  TBaseData = class(TPersistent)
  public
    constructor Create; virtual;  // make constructor virtual
  end;
  TBaseDataClass = class of TBaseData;

  IOWMStreaming = interface
    procedure LoadFromJSON(aJSON: TJSONObject);
    function SaveToJSON: TJSONObject;
  end;

  IClearable = interface
    procedure Clear;
  end;

  TPercent = 0..100;
  THumidity = type TPercent;
  TCloudiness = type TPercent;

  { TCoordinates }

  TCoordinates = class(TPersistent, IClearable)
  private
    fLatitude: Double;
    fLongitude: Double;
  public
    procedure Clear;
  published
    property lon: Double read fLongitude write fLongitude;
    property lat: Double read fLatitude write fLatitude;
  end;

  { TCity }

  TCity = class(TPersistent)
  private
    fCoordinates: TCoordinates;
    fcountry: String;
    fid: String;
    fname: String;
    fpopulation: Int64;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property id: String read fid write fid;
    property name: String read fname write fname;
    property country: String read fcountry write fcountry;
    property population: Int64 read fpopulation write fpopulation;
    property coord: TCoordinates read fCoordinates write fCoordinates;
  end;

  { TMainForecast }

  { TCustomMain }

  TCustomMain = class(TPersistent, IClearable)
  private
    fgrnd_level: double;
    fhumidity: THumidity;
    fpressure: Double;
    fsea_level: double;
    ftemp: Double;
    ftemp_kf: Double;
    ftemp_max: Double;
    ftemp_min: Double;
  public
    procedure Clear;
  protected
    property temp: Double read ftemp write ftemp;
    property temp_min: Double read ftemp_min write ftemp_min;
    property temp_max: Double read ftemp_max write ftemp_max;
    property pressure: Double read fpressure write fpressure;
    property sea_level: double read fsea_level write fsea_level;
    property grnd_level: double read fgrnd_level write fgrnd_level;
    property humidity: THumidity read fhumidity write fhumidity;
    property temp_kf: Double read ftemp_kf write ftemp_kf;
  end;

  TMainForecast = class(TCustomMain)
  published
    property temp;
    property temp_min;
    property temp_max;
    property pressure;
    property sea_level;
    property grnd_level;
    property humidity;
    property temp_kf;
  end;

  TMainWeather = class(TCustomMain)
  published
    property temp;
    property humidity;
    property pressure;
    property temp_min;
    property temp_max;
  end;

  { THistoryStationMain }

  THistoryStationMain = class(TPersistent, IClearable)
  private
    fhumidity: Double;
    fpressure: Double;
    ftemp: Double;
  public
    procedure Clear;
  published
    property temp: Double read ftemp write ftemp;
    property pressure: Double read fpressure write fpressure;
    property humidity: Double read fhumidity write fhumidity; // another data tpye here ...
  end;

  { TClouds }

  TClouds = class(TPersistent, IClearable)
  private
    fAll: TCloudiness;
  public
    procedure Clear;
  published
    property all: TCloudiness read fAll write fAll;
  end;

  { TWind }

  TWind = class(TPersistent, IClearable)
  private
    fDegree: Double;
    fSpeed: Double;
  public
    procedure Clear;
  published
    property speed: Double read fSpeed write fSpeed;
    property deg: Double read fDegree write fDegree;
  end;

  { TRain }

  TRain = class(TPersistent)
  private
    f3h: Integer;
  published
    property Rain3h: Integer read f3h write f3h;
  end;

  { TSnow }

  TSnow = class(TPersistent)
  private
    f3h: Integer;
  published
    property Snow3h: Integer read f3h write f3h;
  end;

  { TForecastSys }

  TForecastSys = class(TPersistent)
  private
    fPod: String;
  published
    property pod: String read fPod write fPod;
  end;

  { TWeatherSys }

  TWeatherSys = class(TPersistent, IClearable)
  private
    fCountry: String;
    fMessage: Double;
    fSunrise: Int64;
    fSunset: Int64;
    function GetSunriseDateTime: TDateTime;
    function GetSunsetDateTime: TDateTime;
  public
    property SunriseDateTime: TDateTime read GetSunriseDateTime;
    property SunsetDateTime: TDateTime read GetSunsetDateTime;
    procedure Clear;
  published
    property message: Double read fMessage write fMessage;
    property country: String read fCountry write fCountry;
    property sunrise: Int64 read fSunrise write fSunrise;
    property sunset: Int64 read fSunset write fSunset;
  end;

  { TFindSys }

  TFindSys = class(TPersistent)
  private
    fCountry: String;
  published
    property country: String read fCountry write fCountry;
  end;

  { TWeather }

  TWeather = class(TCollectionItem)
  private
    fDescription: String;
    fIcon: String;
    fID: Integer;
    fMain: String;
  published
    property id: Integer read fID write fID;  // rename for reading/writing?
    property main: String read fMain write fMain;
    property description: String read fDescription write fDescription;
    property icon: String read fIcon write fIcon;
  end;

  { TForecastItem }

  TForecastItem = class(TCollectionItem)
  private
    fClouds: TClouds;
    fDT: Int64;
    fDT_txt: String;
    fmain: TMainForecast;
    fRain: TRain;
    fSnow: TSnow;
    fsys: TForecastSys;
    fWeather: TCollection;
    fWind: TWind;
    function GetDateTime: TDateTime;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property DateTime: TDateTime read GetDateTime;
  published
    property dt: Int64 read fDT write fDT;
    property dt_txt: String read fDT_txt write fDT_txt;
    property main: TMainForecast read fmain write fmain;
    property weather: TCollection read fWeather write fWeather;
    property clouds: TClouds read fClouds write fClouds;
    property wind: TWind read fWind write fWind;
    property rain: TRain read fRain write fRain;
    property snow: TSnow read fSnow write fSnow;
    property sys: TForecastSys read fsys write fsys;
  end;

  { TForecast3hData }
  TForecast3hData = class(TBaseData)
  private
    fCity: TCity;
    fCod: String;
    fCount: Integer;
    fmessage: Double;
    fForecastItems: TCollection; // TForecastItem
    function GetCount: Integer;
    function GetForecastItem(Index: Integer): TForecastItem;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property ForecastItems[Index: Integer]: TForecastItem read GetForecastItem;
  published
    property cod: String read fCod write fCod;
    property message: Double read fmessage write fmessage;
    property city: TCity read fCity write fCity;
    property cnt: Integer read fCount write fCount;
    property list: TCollection read fForecastItems write fForecastItems;  // TForecastItem
  end;

  { TTemperature }

  TTemperature = class(TPersistent)
  private
    fDay: Double;
    fEve: Double;
    fMax: Double;
    fMin: Double;
    fMorn: Double;
    fNight: Double;
  published
    property day: Double read fDay write fDay;
    property min: Double read fMin write fMin;
    property max: Double read fMax write fMax;
    property night: Double read fNight write fNight;
    property eve: Double read fEve write fEve;
    property morn: Double read fMorn write fMorn;
  end;

  { TDailyForecastItem }

  TDailyForecastItem = class(TCollectionItem)
  private
    fClouds: Double;
    fDT: Int64;
    fHumidity: THumidity;
    fPressure: Double;
    fRain: Double;
    fSnow: Double;
    fTemperature: TTemperature;
    fWeather: TCollection;
    fWind: TWind;
    function GetDateTime: TDateTime;
    function GetDegree: Double;
    function GetSpeed: Double;
    function GetWeather(aIndex: Integer): TWeather;
    function GetWeatherCount: Integer;
    procedure SetDegree(aValue: Double);
    procedure SetSpeed(aValue: Double);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property wind: TWind read fWind;
    property WeatherItem[aIndex: Integer]: TWeather read GetWeather; default;
    property Count: Integer read GetWeatherCount;
    property DateTime: TDateTime read GetDateTime;
  published
    property dt: Int64 read fDT write fDT;
    property temp: TTemperature read fTemperature write fTemperature;
    property pressure: Double read fPressure write fPressure;
    property humidity: THumidity read fHumidity write fHumidity;
    property weather: TCollection read fWeather write fWeather;
    property speed: Double read GetSpeed write SetSpeed;
    property deg: Double read GetDegree write SetDegree;
    property clouds: Double read fClouds write fClouds;
    property rain: Double read fRain write fRain;
    property snow: Double read fSnow write fSnow;
  end;

  { TForecastDailyData }

  TForecastDailyData = class(TBaseData)
  private
    fCity: TCity;
    fcnt: Integer;
    fCod: String;
    fList: TCollection; // TDailyForecastItem
    fMessage: Double;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property cod: String read fCod write fCod;
    property message: Double read fMessage write fMessage;
    property city: TCity read fCity write fCity;
    property cnt: Integer read fcnt write fcnt;
    property list: TCollection read fList write fList;  // TDailyForecastItem
  end;

  { TWeatherData }

  TWeatherData = class(TCollectionItem, IClearable)
  private
    fBase: String;
    fClouds: TClouds;
    fCod: Integer;
    fCoordinates: TCoordinates;
    fDT: Int64;
    fID: Int64;
    fMain: TMainForecast;
    fName: String;
    fSys: TWeatherSys;
    fWeather: TCollection;
    fWind: TWind;
    function GetCount: Integer;
    function GetWeather(aIndex: Integer): TWeather;
  public
    constructor Create(ACollection: TCollection); override;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property WeatherCount: Integer read GetCount;
    property WeatherItems[aIndex: Integer]: TWeather read GetWeather;
  published
    property coord: TCoordinates read fCoordinates write fCoordinates;
    property sys: TWeatherSys read fSys write fSys;
    property weather: TCollection read fWeather write fWeather;
    property base: String read fBase write fBase;
    property main: TMainForecast read fMain write fMain;
    property wind: TWind read fWind write fWind;
    property clouds: TClouds read fClouds write fClouds;
    property dt: Int64 read fDT write fDT;
    property id: Int64 read fID write fID;
    property name: String read fName write fName;
    property cod: Integer read fCod write fCod;
  end;

  { TGroupData }

  TGroupData = class(TBaseData, IClearable)
  private
    fcnt: Integer;
    flist: TCollection;
    function GetCount: Integer;
    function GetWeatherData(Index: Integer): TWeatherData;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    property Count: Integer read GetCount;
    property WeatherData[Index: Integer]: TWeatherData read GetWeatherData;
  published
    property cnt: Integer read fcnt write fcnt;
    property list: TCollection read flist write flist;
  end;

  { TFindItem }

  TFindItem = class(TCollectionItem)
  private
    fClouds: TClouds;
    fCoordinates: TCoordinates;
    fDT: Int64;
    fID: Int64;
    fMain: TMainWeather;
    fName: String;
    fsys: TFindSys;
    fweather: TCollection;
    fwind: TWind;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property LocationID: Int64 read fID write fID;
    property name: String read fName write fName;
    property coord: TCoordinates read fCoordinates write fCoordinates;
    property main: TMainWeather read fMain write fMain;
    property dt: Int64 read fDT write fDT;
    property wind: TWind read fwind write fwind;
    property sys: TFindSys read fsys write fsys;
    property clouds: TClouds read fClouds write fClouds;
    property weather: TCollection read fweather write fweather;
  end;

  { TFindData }

  TFindData = class(TBaseData, IClearable)
  private
    fCod: String;
    fCount: Integer;
    fList: TCollection;
    fMessage: String;
    function GetFindCount: Integer;
    function GetItem(aIndex: Integer): TFindItem;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    property FindItems[aIndex: Integer]: TFindItem read GetItem;
    property FindCount: Integer read GetFindCount;
  published
    property message: String read fMessage write fMessage;
    property cod: String read fCod write fCod;
    property count: Integer read fCount write fCount;
    property list: TCollection read fList write fList;
  end;

  { THistoryCityItem }

  THistoryCityItem = class(TCollectionItem, IClearable)
  private
    fClouds: TClouds;
    fDT: Int64;
    fMain: TMainWeather;
    fWeather: TCollection;
    fWind: TWind;
    function GetDateTime: TDateTime;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Clear;
    property DateTime: TDateTime read GetDateTime;
  published
    property weather: TCollection read fWeather write fWeather; // TWeather
    property main: TMainWeather read fMain write fMain;
    property wind: TWind read fWind write fWind;
    property clouds: TClouds read fClouds write fClouds;
    property dt: Int64 read fDT write fDT;
  end;

  { TCustomHistoryData }

  TCustomHistoryData = class(TBaseData, IClearable)
  private
    fCnt: Integer;
    fCod: String;
    fMessage: String;
    fTickType: String;
    function GetCount: Integer;
  protected
    fList: TCollection;
    function GetListClass: TCollectionItemClass; virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; virtual;
    property Count: Integer read GetCount;
  published
    property message: String read fMessage write fMessage;
    property cod: String read fCod write fCod;
    property cnt: Integer read fCnt write fCnt;
    property list: TCollection read fList write fList; // THistoryCityItem
    property TickType: String read fTickType write fTickType;  // original "type"
  end;

  { THistoryCityData }

  THistoryCityData = class(TCustomHistoryData)
  private
    fCalctime: Double;
    fCity_id: Int64;
    function GetHistoryItem(Index: Integer): THistoryCityItem;
  protected
    function GetListClass: TCollectionItemClass; override;
  public
    procedure Clear; override;
    property HistoryItem[Index: Integer]: THistoryCityItem read GetHistoryItem;
  published
    property city_id: Int64 read fCity_id write fCity_id;
    property calctime: Double read fCalctime write fCalctime;
  end;

  { THistoryStationItem }

  THistoryStationItem = class(TCollectionItem, IClearable)
  private
    fDT: Int64;
    fHumidity: Double;
    fMain: THistoryStationMain;
    fPressuer: Double;
    fPressure: Double;
    fTemperature: Double;
    function GetDateTime: TDateTime;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Clear;
    property DateTime: TDateTime read GetDateTime;
  published
    property temp: Double read fTemperature write fTemperature;
    property main: THistoryStationMain read fMain write fMain;
    property humidity: Double read fHumidity write fHumidity;
    property pressure: Double read fPressure write fPressuer;
    property dt: Int64 read fDT write fDT;
  end;

  { THistoryStationData }

  THistoryStationData = class(TCustomHistoryData, IClearable)
  private
    fCalctime: String;
    fStation_id: Int64;
    function GetHistoryItem(Index: Integer): THistoryStationItem;
  protected
    function GetListClass: TCollectionItemClass; override;
  public
    procedure Clear; override;
    property HistoryItem[Index: Integer]: THistoryStationItem read GetHistoryItem;
  published
    property station_id: Int64 read fStation_id write fStation_id;
    property calctime: String read fCalctime write fCalctime;
  end;

  function StrToForcast3h(const aStr: String): TForecast3hData; inline;
  function StrToForcastDaily(const aStr: String): TForecastDailyData; inline;
  function StrToWeather(const aStr: String): TWeatherData; inline;
  function StrToFind(const aStr: String): TFindData; inline;
  function StrToHistoryCity(const aStr: String): THistoryCityData; inline;
  function StrToHistoryStation(const aStr: String): THistoryStationData; inline;

//  procedure LoadForecast3h(const aStr: String);

  function JSONToForcast3h(aJSON: TJSONObject): TForecast3hData; inline;
  function JSONToForcastDaily(aJSON: TJSONObject): TForecastDailyData; inline;
  function JSONToWeather(aJSON: TJSONObject): TWeatherData; inline;
  function JSONToFind(aJSON: TJSONObject): TFindData; inline;
  function JSONToHistoryCity(aJSON: TJSONObject): THistoryCityData; inline;
  function JSONToHistoryStation(aJSON: TJSONObject): THistoryStationData; inline;

  function Forcast3hToJSON(aData: TForecast3hData): TJSONObject; inline;
  function ForcastDailyToJSON(aData: TForecastDailyData): TJSONObject; inline;
  function WeatherToJSON(aData: TWeatherData): TJSONObject; inline;
  function FindToJSON(aData: TFindData): TJSONObject; inline;
  function HistoryCityToJSON(aData: THistoryCityData): TJSONObject; inline;
  function HistoryStationToJSON(aData: THistoryStationData): TJSONObject; inline;

  function StrToJSON(const aStr: String): TJSONObject;

implementation

uses dateutils, fpjsonrtti, jsonparser, jsonscanner;

type
  TJSONStreamModify = procedure(aJSON: TJSONObject);

// forward declarations for internal functions
  procedure JSONToOWMData(aJSON: TJSONObject; aDataObject: TPersistent; modify: TJSONStreamModify); forward;
  function JSONToOWMData(aJSON: TJSONObject; aDataClass: TBaseDataClass; modify: TJSONStreamModify = nil): TPersistent; forward; overload;
  function JSONToOWMData(aJSON: TJSONObject; aDataClass: TCollectionItemClass; modify: TJSONStreamModify = nil): TPersistent; forward; overload;
  function OWMDataToJSON(aOWMData: TPersistent; modify: TJSONStreamModify = nil): TJSONObject; forward;
  procedure PreDeStreamForecast(root: TJSONObject); forward;
  procedure PreDeStreamFind(root: TJSONObject); forward;
  procedure PreDeStreamHistory(root: TJSONObject); forward;
  procedure PostStreamForecast(root: TJSONObject); forward;
  procedure PostStreamFind(root: TJSONObject); forward;
  procedure PostStreamHistory(root: TJSONObject); forward;


procedure JSONToOWMData(aJSON: TJSONObject; aDataObject: TPersistent; modify: TJSONStreamModify);
var
  s: TJSONDeStreamer;
  j: TJSONObject;
begin
  j := nil;
  s := TJSONDeStreamer.Create(nil);
  try
    if Assigned(modify) then
    begin
      j := TJSONObject(aJSON.Clone);
      modify(j);
      s.JSONToObject(j, aDataObject);
    end
    else
      s.JSONToObject(aJSON, aDataObject);
  finally
    j.Free;
    s.Destroy;
  end;
end;

function JSONToOWMData(aJSON: TJSONObject; aDataClass: TBaseDataClass;
  modify: TJSONStreamModify): TPersistent;
begin
  Result := aDataClass.Create;
  try
    JSONToOWMData(aJSON, Result, modify);
  except
    Result.Destroy;
    Result := nil;
  end;
end;

function JSONToOWMData(aJSON: TJSONObject; aDataClass: TCollectionItemClass;
  modify: TJSONStreamModify): TPersistent;
begin
  Result := aDataClass.Create(nil);
  try
    JSONToOWMData(aJSON, Result, modify);
  except
    Result.Destroy;
    Result := nil;
  end;
end;

function OWMDataToJSON(aOWMData: TPersistent; modify: TJSONStreamModify = nil): TJSONObject;
var
  s: TJSONStreamer;
begin
  s := TJSONStreamer.Create(nil);
  try
    try
      Result := s.ObjectToJSON(aOWMData);
      if Assigned(modify) then
        modify(Result);
    finally
      s.Destroy;
    end;
  except
    Result.Destroy;
    Result := nil;
  end;
end;

(*----------------------------------------------------------------------------*
 * Begin of PreDeStream                                                       *
 * apply to JSON objects before reading from stream                           *
 *----------------------------------------------------------------------------*)

procedure PreDeStreamForecast(root: TJSONObject);
var
  ListObject, rain, snow: TJSONObject;
  i: Integer;
  forecastObjects: TJSONArray;
begin
  // find "list"
  forecastObjects := root.Arrays['list'];
  for i := 0 to forecastObjects.Count - 1 do  // "rain" is containd in each "list" object
  begin
    ListObject := TJSONObject(forecastObjects.Items[i]);

    rain := TJSONObject(ListObject.Find('rain', jtObject)); // get rain object
    if Assigned(rain) then
      rain.Add('3h', rain.Extract('Rain3h'));  // move to 3h

    snow := TJSONObject(ListObject.Find('snow', jtObject));
    if Assigned(snow) then
      snow.Add('3h', snow.Extract('Snow3h'));
  end;
end;

procedure PreDeStreamFind(root: TJSONObject);
var
  ListObject: TJSONObject;
  i: Integer;
  forecastObjects: TJSONArray;
begin
  // find "list"
  forecastObjects := root.Arrays['list'];
  for i := 0 to forecastObjects.Count - 1 do
  begin
    ListObject := TJSONObject(forecastObjects.Items[i]);

    if Assigned(ListObject.Find('id', jtObject)) then
      ListObject.Add('LocationID', ListObject.Extract('id'));  // find
  end;
end;

procedure PreDeStreamHistory(root: TJSONObject);
begin
  if Assigned(root.Find('type', jtString)) then
    root.Add('TickType', root.Extract('type'));
end;

(*----------------------------------------------------------------------------*
 * End of PreStream                                                           *
 *----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 * Begin of PostStream                                                        *
 * apply to JSON objects after streaming                                      *
 *----------------------------------------------------------------------------*)

procedure PostStreamForecast(root: TJSONObject);
var
  ListObject, rain, snow: TJSONObject;
  i: Integer;
  forecastObjects: TJSONArray;
begin
  // find "list"
  forecastObjects := root.Arrays['list'];
  for i := 0 to forecastObjects.Count - 1 do  // "rain" is containd in each "list" object
  begin
    ListObject := TJSONObject(forecastObjects.Items[i]);

    rain := TJSONObject(ListObject.Find('rain', jtObject)); // get rain object
    if Assigned(rain) then
      rain.Add('Rain3h', rain.Extract('3h'));  // move to 3h

    snow := TJSONObject(ListObject.Find('snow', jtObject));
    if Assigned(snow) then
      snow.Add('Snow3h', snow.Extract('3h'));
  end;
end;

procedure PostStreamFind(root: TJSONObject);
var
  ListObject: TJSONObject;
  i: Integer;
  forecastObjects: TJSONArray;
begin
  // find "list"
  forecastObjects := root.Arrays['list'];
  for i := 0 to forecastObjects.Count - 1 do
  begin
    ListObject := TJSONObject(forecastObjects.Items[i]);

    if Assigned(ListObject.Find('LocationID', jtObject)) then
      ListObject.Add('id', ListObject.Extract('LocationID'));  // find
  end;
end;

procedure PostStreamHistory(root: TJSONObject);
begin
  if Assigned(root.Find('TickType', jtString)) then
    root.Add('type', root.Extract('TickType'));
end;

(*----------------------------------------------------------------------------*
 * end of PostStream                                                          *
 *----------------------------------------------------------------------------*)

function StrToForcast3h(const aStr: String): TForecast3hData;
begin
  Result := JSONToForcast3h(StrToJSON(aStr));
end;

function StrToForcastDaily(const aStr: String): TForecastDailyData;
begin
  Result := JSONToForcastDaily(StrToJSON(aStr));
end;

function StrToWeather(const aStr: String): TWeatherData;
begin
  Result := JSONToWeather(StrToJSON(aStr));
end;

function StrToFind(const aStr: String): TFindData;
begin
  Result := JSONToFind(StrToJSON(aStr));
end;

function StrToHistoryCity(const aStr: String): THistoryCityData;
begin
  Result := JSONToHistoryCity(StrToJSON(aStr));
end;

function StrToHistoryStation(const aStr: String): THistoryStationData;
begin
  Result := JSONToHistoryStation(StrToJSON(aStr));
end;

function JSONToForcast3h(aJSON: TJSONObject): TForecast3hData;
begin
  Result := TForecast3hData(JSONToOWMData(aJSON, TForecast3hData, @PreDeStreamForecast));
end;

function JSONToForcastDaily(aJSON: TJSONObject): TForecastDailyData;
begin
  Result := TForecastDailyData(JSONToOWMData(aJSON, TForecastDailyData, @PreDeStreamForecast));
end;

function JSONToWeather(aJSON: TJSONObject): TWeatherData;
begin
  Result := TWeatherData(JSONToOWMData(aJSON, TWeatherData));
end;

function JSONToFind(aJSON: TJSONObject): TFindData;
begin
  Result := TFindData(JSONToOWMData(aJSON, TFindData, @PreDeStreamFind));
end;

function JSONToHistoryCity(aJSON: TJSONObject): THistoryCityData;
begin
  Result := THistoryCityData(JSONToOWMData(aJSON, THistoryCityData, @PreDeStreamHistory));
end;

function JSONToHistoryStation(aJSON: TJSONObject): THistoryStationData;
begin
  Result := THistoryStationData(JSONToOWMData(aJSON, THistoryStationData, @PreDeStreamHistory));
end;

function Forcast3hToJSON(aData: TForecast3hData): TJSONObject;
begin
  Result := OWMDataToJSON(aData, @PostStreamForecast);
end;

function ForcastDailyToJSON(aData: TForecastDailyData): TJSONObject;
begin
  Result := OWMDataToJSON(aData, @PostStreamForecast);
end;

function WeatherToJSON(aData: TWeatherData): TJSONObject;
begin
  Result := OWMDataToJSON(aData);
end;

function FindToJSON(aData: TFindData): TJSONObject;
begin
  Result := OWMDataToJSON(aData, @PostStreamFind);
end;

function HistoryCityToJSON(aData: THistoryCityData): TJSONObject;
begin
  Result := OWMDataToJSON(aData, @PostStreamHistory);
end;

function HistoryStationToJSON(aData: THistoryStationData): TJSONObject;
begin
  Result := OWMDataToJSON(aData, @PostStreamHistory);
end;

function StrToJSON(const aStr: String): TJSONObject;
var
  p: TJSONParser;
begin
  p := TJSONParser.Create(aStr, DefaultOptions);
  try
    Result := TJSONObject(p.Parse);
  finally
    p.Destroy;
  end;
end;

{ TBaseData }

constructor TBaseData.Create;
begin
  inherited Create;
end;

{ TCoordinates }

procedure TCoordinates.Clear;
begin
  fLatitude:=0;
  fLongitude:=0;
end;

{ TClouds }

procedure TClouds.Clear;
begin
  fAll := 0;
end;

{ TCustomMain }

procedure TCustomMain.Clear;
begin
  fgrnd_level:=0;
  fhumidity:=0;
  fpressure:=0;
  fsea_level:=0;
  ftemp:=0;
  ftemp_kf:=0;
  ftemp_max:=0;
  ftemp_min:=0;
end;

{ TWind }

procedure TWind.Clear;
begin
  fDegree := 0;
  fSpeed := 0;
end;

{ THistoryStationMain }

procedure THistoryStationMain.Clear;
begin
  fhumidity:=0;
  fpressure:=0;
  ftemp:=0;
end;

{ THistoryStationItem }

function THistoryStationItem.GetDateTime: TDateTime;
begin
  Result := UnixToDateTime(fDT);
end;

constructor THistoryStationItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fMain := THistoryStationMain.Create;
end;

destructor THistoryStationItem.Destroy;
begin
  fMain.Free;
  inherited Destroy;
end;

procedure THistoryStationItem.Clear;
begin
  fDT:=0;
  fHumidity:=0;
  fMain.Clear;
  fPressuer:=0;
  fPressure:=0;
  fTemperature:=0;
end;

{ THistoryStationData }

function THistoryStationData.GetHistoryItem(Index: Integer
  ): THistoryStationItem;
begin
  Result := THistoryStationItem(fList.Items[Index]);
end;

function THistoryStationData.GetListClass: TCollectionItemClass;
begin
  Result := THistoryStationItem;
end;

procedure THistoryStationData.Clear;
begin
  inherited Clear;
  fCalctime:='';
  fStation_id:=0;
end;

function THistoryCityData.GetHistoryItem(Index: Integer): THistoryCityItem;
begin
  Result := THistoryCityItem(fList.Items[Index]);
end;

function THistoryCityData.GetListClass: TCollectionItemClass;
begin
  Result := THistoryCityItem;
end;

procedure THistoryCityData.Clear;
begin
  inherited Clear;

  fCalctime := 0;
  fCity_id := 0;
end;

{ TCustomHistoryData }

function TCustomHistoryData.GetCount: Integer;
begin
  Result := fList.Count;
end;

constructor TCustomHistoryData.Create;
begin
  inherited;
  fList := TCollection.Create(GetListClass);
end;

destructor TCustomHistoryData.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

procedure TCustomHistoryData.Clear;
begin
  fCnt := 0;
  fCod := '';
  fMessage := '';
  fTickType := '';
  fList.Clear;
end;

{ THistoryCityItem }

function THistoryCityItem.GetDateTime: TDateTime;
begin
  Result := UnixToDateTime(fDT);
end;

constructor THistoryCityItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fClouds := TClouds.Create;
  fMain := TMainWeather.Create;
  fWeather := TCollection.Create(TWeather);
  fWind := TWind.Create;
end;

destructor THistoryCityItem.Destroy;
begin
  fClouds.Free;
  fMain.Free;
  fWeather.Free;
  fWind.Free;
  inherited Destroy;
end;

procedure THistoryCityItem.Clear;
begin
  fClouds.Clear;
  fDT:=0;
  fMain.Clear;
  fWeather.Clear;
  fWind.Clear;
end;

{ TFindItem }

constructor TFindItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fClouds := TClouds.Create;
  fCoordinates := TCoordinates.Create;
  fMain := TMainWeather.Create;
  fsys := TFindSys.Create;
  fweather := TCollection.Create(TWeather);
  fwind := TWind.Create;
end;

destructor TFindItem.Destroy;
begin
  fClouds.Free;
  fCoordinates.Free;
  fMain.Free;
  fsys.Free;
  fweather.Free;
  fwind.Free;
  inherited Destroy;
end;

{ TFindData }

function TFindData.GetFindCount: Integer;
begin
  Result := fList.Count;
end;

function TFindData.GetItem(aIndex: Integer): TFindItem;
begin
  Result := TFindItem(fList.Items[aIndex]);
end;

constructor TFindData.Create;
begin
  inherited;
  fList := TCollection.Create(TFindItem);
end;

destructor TFindData.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

procedure TFindData.Clear;
begin
  fCod:='';
  fCount:=0;
  fList.Clear;
  fMessage:='';
end;

{ TForecastDailyData }

constructor TForecastDailyData.Create;
begin
  inherited;
  fCity := TCity.Create;
  fList := TCollection.Create(TDailyForecastItem);
end;

destructor TForecastDailyData.Destroy;
begin
  fCity.Free;
  fList.Free;
  inherited Destroy;
end;

{ TGroupData }

function TGroupData.GetCount: Integer;
begin
  Result := flist.Count;
end;

function TGroupData.GetWeatherData(Index: Integer): TWeatherData;
begin
  Result := TWeatherData(flist.Items[Index]);
end;

constructor TGroupData.Create;
begin
  inherited;
  flist := TCollection.Create(TWeatherData);
end;

destructor TGroupData.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

procedure TGroupData.Clear;
begin
  fcnt:=0;
  flist.Clear;
end;

{ TDailyForecastItem }

function TDailyForecastItem.GetDegree: Double;
begin
  Result := fWind.deg;
end;

function TDailyForecastItem.GetDateTime: TDateTime;
begin
  Result := UnixToDateTime(fDT);
end;

function TDailyForecastItem.GetSpeed: Double;
begin
  Result := fWind.speed;
end;

function TDailyForecastItem.GetWeather(aIndex: Integer): TWeather;
begin
  Result := TWeather(fWeather.Items[aIndex]);
end;

function TDailyForecastItem.GetWeatherCount: Integer;
begin
  Result := fWeather.Count;
end;

procedure TDailyForecastItem.SetDegree(aValue: Double);
begin
  fwind.deg := aValue;
end;

procedure TDailyForecastItem.SetSpeed(aValue: Double);
begin
  fWind.speed := aValue;
end;

constructor TDailyForecastItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fWind := TWind.Create;
  fTemperature := TTemperature.Create;
  fWeather := TCollection.Create(TWeather);
end;

destructor TDailyForecastItem.Destroy;
begin
  fWind.Free;
  inherited Destroy;
end;

{ TWeatherData }

function TWeatherData.GetCount: Integer;
begin
  Result := fWeather.Count;
end;

function TWeatherData.GetWeather(aIndex: Integer): TWeather;
begin
  Result := TWeather(fWeather.Items[aIndex]);
end;

constructor TWeatherData.Create(ACollection: TCollection);
begin
  inherited;
  fClouds := TClouds.Create;
  fCoordinates := TCoordinates.Create;
  fMain := TMainForecast.Create;
  fSys := TWeatherSys.Create;
  fWeather := TCollection.Create(TWeather);
  fWind := TWind.Create;
end;

constructor TWeatherData.Create;
begin
  Create(nil);
end;

destructor TWeatherData.Destroy;
begin
  fClouds.Free;
  fCoordinates.Free;
  fMain.Free;
  fSys.Free;
  fWeather.Free;
  fWind.Free;
  inherited Destroy;
end;

procedure TWeatherData.Clear;
begin
  fBase:='';
  fClouds.Clear;
  fCod:=0;
  fCoordinates.Clear;
  fDT:=0;
  fID:=0;
  fMain.Clear;
  fName:='';
  fSys.Clear;
  fWeather.Clear;
  fWind.Clear;
end;

{ TWeatherSys }

function TWeatherSys.GetSunriseDateTime: TDateTime;
begin
  Result := UnixToDateTime(fSunrise);
end;

function TWeatherSys.GetSunsetDateTime: TDateTime;
begin
  Result := UnixToDateTime(fSunset);
end;

procedure TWeatherSys.Clear;
begin
  fCountry:='';
  fMessage:=0;
  fSunrise:=0;
  fSunset:=0;
end;

{ TForecastItem }

function TForecastItem.GetDateTime: TDateTime;
begin
  Result := UnixToDateTime(fDT);
end;

constructor TForecastItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fmain := TMainForecast.Create;
  fClouds := TClouds.Create;
  fWind := TWind.Create;
  fRain := TRain.Create;
  fSnow := TSnow.Create;
  fWeather := TCollection.Create(TWeather);
  fsys := TForecastSys.Create;
end;

destructor TForecastItem.Destroy;
begin
  fsys.Free;
  fWeather.Free;
  fRain.Free;
  fWind.Free;
  fClouds.Free;
  fSnow.Free;
  fmain.Free;
  inherited Destroy;
end;

{ TCity }

constructor TCity.Create;
begin
  fCoordinates := TCoordinates.Create;
end;

destructor TCity.Destroy;
begin
  fCoordinates.Free;
  inherited Destroy;
end;

{ TForecast3hData }

function TForecast3hData.GetCount: Integer;
begin
  Result := fForecastItems.Count;
end;

function TForecast3hData.GetForecastItem(Index: Integer): TForecastItem;
begin
  Result := TForecastItem(fForecastItems.Items[Index]);
end;

constructor TForecast3hData.Create;
begin
  inherited Create;
  fCity := TCity.Create;
  fForecastItems := TCollection.Create(TForecastItem);
end;

destructor TForecast3hData.Destroy;
begin
  fCity.Free;
  fForecastItems.Free;
  inherited Destroy;
end;

end.

