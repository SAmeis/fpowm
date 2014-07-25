{ GET parameters for OpenWeatherMap web API

  Copyright (C) 2014, Simon Ameis <simon.ameis@web.de>

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit owmurloptions;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, fgl;

const
  OWM_BASE_URL                 = 'http://api.openweathermap.org/data/2.5/';
  OWM_ENDPOINT_WEATHER         = 'weather';
	OWM_ENDPOINT_GROUP           = 'group';
	OWM_ENDPOINT_FORECAST        = 'forecast';
	OWM_ENDPOINT_FORECAST_DAILY  = 'forecast/daily';
	OWM_ENDPOINT_FIND            = 'find';
  OWM_ENDPOINT_HISTORY_STATION = 'history/station';
  OWM_ENDPOINT_HISTORY_CITY    = 'history/city';
  INotifyChangeGUID: TGUID = '{49B226C6-2DB7-40B3-92CE-5F732CB62E53}';

type
  INotifyChange = interface
    ['{49B226C6-2DB7-40B3-92CE-5F732CB62E53}']
    procedure NotifyChange(Sender: TObject);
  end;

  { TURLParamBase }

  TURLParamBase = class(TComponent)
  private
    procedure SetIsSet(aValue: Boolean);
  protected
    fIsSet: Boolean;
    function GetAsText: UTF8String; virtual;
    function GetValueAsString: UTF8String; virtual; abstract;
    class function GetParamName: UTF8String; virtual; abstract;
    procedure NotifyChangeOwner;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear; virtual;
    function ToString: String; override;
    property AsText: UTF8String read GetAsText;
    property ValueAsText: UTF8String read GetValueAsString;
  published
    property IsSet: Boolean read fIsSet write SetIsSet default False; // may be overridden by Value property; setting to false may change Value property
  end;

  TURLParamClass = class of TURLParamBase;
  TURLParamArray = array of TURLParamBase;

  { TURLParam }

  generic TURLParam<_URLType> = class(TURLParamBase)
  protected
    fValue: _URLType;
    procedure SetValue(aValue: _URLType); virtual;
  public
    property Value: _URLType read fValue write SetValue stored False;
  end;

  { TURLParamMode }
  TOpenWeatherMapModeOption = (moXML, moJSON, moHTML);
  TURLParamModeAbstract = specialize TURLParam<TOpenWeatherMapModeOption>;
  TURLParamMode = class(TURLParamModeAbstract)
  protected
    class function GetParamName: UTF8String; override;
    function GetValueAsString: UTF8String; override;
  published
    property Value stored fIsSet;
  end;

  { TURLParamUnits }

  TOpenWeatherMapUnitsOption = (uoImperial, uoMetric);
  TURLParamUnitsAbstract = specialize TURLParam<TOpenWeatherMapUnitsOption>;
  TURLParamUnits = class(TURLParamUnitsAbstract)
  protected
    class function GetParamName: UTF8String; override;
    function GetValueAsString: UTF8String; override;
  published
    property Value stored fIsSet;
  end;

  { TURLParamLanguage }

  TOpenWeatherMapLanguageOption = (loEnglish, loRussian, loItalian, loSpanish,
    loUkrainian, loGerman, loPortuguese, loRomanian, loPolish, loFinnish,
    loDutch, loFrench, loBulgarian, loSwedish, loChineseTraditional,
    loChineseSimplified, loTurkish, loCzech, loGalician, loVietnamese,
    loArabic, loMacedonian, loSlovak);
  TURLParamLanguageAbstract = specialize TURLParam<TOpenWeatherMapLanguageOption>;
  TURLParamLanguage = class(TURLParamLanguageAbstract)
  protected
    class function GetParamName: UTF8String; override;
    function GetValueAsString: UTF8String; override;
  published
    property Value stored fIsSet;
  end;

  { TURLParamCoordinate }

  TCoordinate = record
    Latitude: Double;
    Longitude: Double;
  end;
  TURLParamCoordinateAbstract = specialize TURLParam<TCoordinate>;
  TURLParamCoordinate = class(TURLParamCoordinateAbstract)
  private
    procedure SetLatitude(aValue: Double);
    procedure SetLongitude(aValue: Double);
  protected
    function GetAsText: UTF8String; override;
    class function GetParamName: UTF8String; override;
  published
    property Latitude: Double read fValue.Latitude write SetLatitude stored fIsSet;
    property Longitude: Double read fValue.Longitude write SetLongitude stored fIsSet;
  end;

  { TURLParamString }

  TURLParamStringAbstract = specialize TURLParam<UTF8String>;
  TURLParamString = class(TURLParamStringAbstract)
  protected
    function GetValueAsString: UTF8String; override;
  published
    property Value stored fIsSet;
  end;

  { TURLParamQuery }

  TURLParamQuery = class(TURLParamString)
  protected
    class function GetParamName: UTF8String; override;
  end;

  { TURLParamCallback }

  TURLParamCallback = class(TURLParamString)
  protected
    class function GetParamName: UTF8String; override;
  end;

  { TURLParamAPIKey }

  TURLParamAPIKey = class(TURLParamString)
  protected
    class function GetParamName: UTF8String; override;
  end;

  { TURLParamInt }

  TURLParamIntAbstract = specialize TURLParam<Int64>;
  TURLParamInt = class(TURLParamIntAbstract)
  protected
    function GetValueAsString: UTF8String; override;
    procedure SetValue(aValue: _URLType); override;
    function GetMinValue: Int64; virtual;
    function GetMaxValue: Int64; virtual;
  published
    property Value stored fIsSet;
  end;

  { TURLParamID }

  TURLParamID = class(TURLParamInt)
  protected
    class function GetParamName: UTF8String; override;
  end;

  { TUrlParamCount }

  TURLParamCount = class(TURLParamInt)
  protected
    class function GetParamName: UTF8String; override;
  end;

  { TURLParamFindType }

  TOpenWeatherMapFindTypeOption = (ftoAccurate, ftoLike);
  TURLParamFindTypeAbstract = specialize TURLParam<TOpenWeatherMapFindTypeOption>;
  TURLParamFindType = class(TURLParamFindTypeAbstract)
  protected
    class function GetParamName: UTF8String; override;
    function GetValueAsString: UTF8String; override;
  published
    property Value stored fIsSet;
  end;

  { TURLParamHistoryType }

  TOpenWeatherMapHistoryTypeOption = (htoTick, htoHour, htoDay);
  TURLParamHistoryTypeAbstract = specialize TURLParam<TOpenWeatherMapHistoryTypeOption>;
  TURLParamHistoryType = class(TURLParamHistoryTypeAbstract)
  protected
    class function GetParamName: UTF8String; override;
    function GetValueAsString: UTF8String; override;
  published
    property Value stored fIsSet;
  end;

  TURLParamDateTimeAbstract = specialize TURLParam<TDateTime>;

  { TURLParamDateTime }

  TURLParamDateTime = class(TURLParamDateTimeAbstract)
  protected
    function GetValueAsString: UTF8String; override;
  published
    property Value stored fIsSet;
  end;

  { TURLParamStartTime }

  TURLParamStartTime = class(TURLParamDateTime)
  protected
    class function GetParamName: UTF8String; override;
  end;

  { TURLParamEndTime }

  TURLParamEndTime = class(TURLParamDateTime)
  protected
    class function GetParamName: UTF8String; override;
  end;

  { TURLParamMultipleID }
  TMultipleIDS = specialize TFPGList<Int64>;
  TURLParamMultipleIDAbstract = specialize TURLParam<TMultipleIDS>;
  TURLParamMultipleID = class(TURLParamMultipleIDAbstract)
  private
    function GetIsSet: Boolean;
  protected
    class function GetParamName: UTF8String; override;
    function GetValueAsString: UTF8String; override;
    procedure SetValue(aValue: _URLType); override;
    procedure ReadIDs(Reader: TReader);
    procedure WriteIDs(Writer: TWriter);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(const aID: Int64);
    procedure Clear; override;
    procedure DefineProperties(Filer: TFiler); override;
  published
    property IsSet: Boolean read GetIsSet;
    property Value stored fIsSet;
  end;

   { TOWMCustomOptions }

  TOWMCustomOptions = class(TComponent, INotifyChange)
  private
    fAPIKey: TURLParamAPIKey;
    fCallback   : TURLParamCallback;
    fCoordinates: TURLParamCoordinate;
    fCount      : TURLParamCount;
    fEndTime    : TURLParamEndTime;
    fFindType   : TURLParamFindType;
    fHistoryType: TURLParamHistoryType;
    fID         : TURLParamID;
    fLanguage   : TURLParamLanguage;
    fMode       : TURLParamMode;
    fMultipleIDs: TURLParamMultipleID;
    fQuery      : TURLParamQuery;
    fStartTime  : TURLParamStartTime;
    fUnits      : TURLParamUnits;
    function GetLocation: TURLParamBase;
    procedure NotifyChange(Sender: TObject);
  protected
    class function GetFirstSetParam(const a: array of TURLParamBase): TURLParamBase;
    class procedure UnsetParams(const a: array of TURLParamBase);
    class function ParamsToString(const a: array of TURLParamBase): UTF8String;
  protected
    property Mode       : TURLParamMode        read fMode;
    property Units      : TURLParamUnits       read fUnits;
    property Language   : TURLParamLanguage    read fLanguage;
    property Count      : TURLParamCount       read fCount;

    property Location   : TURLParamBase        read GetLocation;  // currently active location or nil, if nothing set
    property Query      : TURLParamQuery       read fQuery;
    property ID         : TURLParamID          read fID;
    property Coordinates: TURLParamCoordinate  read fCoordinates;
    property MultipleIDs: TURLParamMultipleID  read fMultipleIDs;
    property FindType   : TURLParamFindType    read fFindType;
    property HistoryType: TURLParamHistoryType read fHistoryType;
    property StartTime  : TURLParamStartTime   read fStartTime;
    property EndTime    : TURLParamEndTime     read fEndTime;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetAsText: UTF8String;
    function ToString: String; override;
    procedure DefineProperties(Filer: TFiler); override;
  published
    property APIKey     : TURLParamAPIKey      read fAPIKey;
    property Callback   : TURLParamCallback    read fCallback;
    property AsText     : UTF8String           read GetAsText;
  end;

  TOWMWeatherOptions = class(TOWMCustomOptions)
  published
    property Mode       ;
    property Units      ;
    property Language   ;
    property Query      ;
    property ID         ;
    property Coordinates;
  end;

  TOWMGroupOptions = class(TOWMCustomOptions)
  published
    property Mode       ;
    property Units      ;
    property Language   ;
    property MultipleIDs;
  end;

  TOWMForecastOptions = class(TOWMWeatherOptions)
  published
    property Count;
  end;

  TOWMFindOptions = class(TOWMCustomOptions)
  published
    property Query      ;
    property Coordinates;
    property Count      ;
    property FindType   ;
  end;

  TOWMHistoryOptions = class(TOWMCustomOptions)
  published
    property ID         ;
    property Count      ;
    property HistoryType;
    property StartTime  ;
    property EndTime    ;
    property Units      ;
  end;

  procedure Register;

implementation

uses fphttpclient, math, dateutils, sysutils, LResources;

procedure Register;
begin
  RegisterComponents('OpenWeatherMap', [TOWMForecastOptions, TOWMWeatherOptions, TOWMFindOptions, TOWMGroupOptions, TOWMHistoryOptions]);
end;

{ TURLParamMultipleID }

function TURLParamMultipleID.GetIsSet: Boolean;
begin
  Result := fIsSet or (fValue.Count > 0);
end;

class function TURLParamMultipleID.GetParamName: UTF8String;
begin
  Result := 'id';
end;

function TURLParamMultipleID.GetValueAsString: UTF8String;
var
  i: SizeInt;
begin
  if fValue.Count > 0 then
  begin
    Result := IntToStr(fValue.Items[0]);
    for i  := 1 to fValue.Count - 1 do
      Result := ',' + IntToStr(fValue.Items[i]);
  end
  else
    Result := '';
end;

procedure TURLParamMultipleID.SetValue(aValue: _URLType);
begin
  fValue.Assign(aValue);
  fIsSet := fValue.Count > 0;
  NotifyChangeOwner;
end;

procedure TURLParamMultipleID.ReadIDs(Reader: TReader);
begin
  Reader.ReadListBegin;
  while Reader.NextValue <> vaNull do
    fValue.Add(Reader.ReadInt64);
  Reader.ReadListEnd;
end;

procedure TURLParamMultipleID.WriteIDs(Writer: TWriter);
var
  i: Int64;
begin
  Writer.WriteListEnd;
  for i in fValue do
    Writer.WriteInteger(i);
  Writer.WriteListEnd;
end;

constructor TURLParamMultipleID.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fValue := TMultipleIDS.Create;
end;

destructor TURLParamMultipleID.Destroy;
begin
  fValue.Free;
  inherited Destroy;
end;

procedure TURLParamMultipleID.Add(const aID: Int64);
begin
  fValue.Add(aID);
  fIsSet := True;
  NotifyChangeOwner;
end;

procedure TURLParamMultipleID.Clear;
begin
  fValue.Clear;
  inherited Clear;
end;

procedure TURLParamMultipleID.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Value', @ReadIDs, @WriteIDs, IsSet);
end;

{ TURLParamEndTime }

class function TURLParamEndTime.GetParamName: UTF8String;
begin
  Result := 'end';
end;

{ TURLParamStartTime }

class function TURLParamStartTime.GetParamName: UTF8String;
begin
  Result := 'start';
end;

{ TURLParamDateTime }

function TURLParamDateTime.GetValueAsString: UTF8String;
begin
  Result := IntToStr(DateTimeToUnix(fValue));
end;

{ TURLParamHistoryType }

class function TURLParamHistoryType.GetParamName: UTF8String;
begin
  Result := 'type';
end;

function TURLParamHistoryType.GetValueAsString: UTF8String;
begin
  case fValue of
    htoTick: Result := 'tick';
    htoHour: Result := 'hour';
    htoDay : Result := 'day';
  end;
end;

{ TURLParamFindType }

class function TURLParamFindType.GetParamName: UTF8String;
begin
  Result := 'type';
end;

function TURLParamFindType.GetValueAsString: UTF8String;
begin
  case fValue of
    ftoAccurate: Result := 'accurate';
    ftoLike    : Result := 'like';
  end;
end;

{ TURLParamBase }

procedure TURLParamBase.SetIsSet(aValue: Boolean);
begin
  if aValue then
    fIsSet := aValue
  else
    Clear;
end;

function TURLParamBase.GetAsText: UTF8String;
begin
  if IsSet then
    Result := GetParamName + '=' + EncodeURLElement(GetValueAsString)
  else
    Result := '';
end;

function TURLParamBase.ToString: String;
begin
  Result := GetAsText;
end;

procedure TURLParamBase.NotifyChangeOwner;
var
  i: INotifyChange;
begin
  if Owner.GetInterface(INotifyChangeGUID, i) then
    i.NotifyChange(Self);
end;

constructor TURLParamBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetSubComponent(True);
end;

procedure TURLParamBase.Clear;
begin
  fIsSet := False;
  NotifyChangeOwner;
end;

{ TOWMCustomOptions }

procedure TOWMCustomOptions.NotifyChange(Sender: TObject);
begin
  if not TURLParamBase(Sender).IsSet then exit;
  // only one location should be used
  // they may all be specified and they override each other
  // but this is not a clean way
  if Sender = fQuery then
    UnsetParams([fCoordinates, fID])
  else if Sender = fCoordinates then
    UnsetParams([fQuery, fID])
  else if Sender = fID then
    UnsetParams([fQuery, fCoordinates])
  else if (Sender = fCallback) then
    fMode.IsSet := False; // if callback is set, always json is returned
end;

class function TOWMCustomOptions.GetFirstSetParam(
  const a: array of TURLParamBase): TURLParamBase;
var
  c: TURLParamBase;
begin
  for c in a do
    if c.IsSet then
      exit(c);
  exit(nil);
end;

class procedure TOWMCustomOptions.UnsetParams(const a: array of TURLParamBase);
var
 c: TURLParamBase;
begin
 for c in a do
   c.Clear;
end;

class function TOWMCustomOptions.ParamsToString(const a: array of TURLParamBase
  ): UTF8String;
var
 c: TURLParamBase;
 ps: UTF8String;
 s: TStringList;
begin
  if length(a) > 0 then
  begin
    s := TStringList.Create;
    s.Delimiter := '&';
    s.QuoteChar := #0;
    try

      for c in a do
      begin
        if Assigned(c) then
        begin
          if c.IsSet then
          begin
            ps := c.GetAsText;
            s.Add(ps);
          end;
        end;
      end;

      Result := s.DelimitedText;
    finally
      s.Free;
    end;
  end
  else
    Result := '';
end;

constructor TOWMCustomOptions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCallback    := TURLParamCallback.Create(Self);
  {$Warnings off} // abstract method warning
  {$Hints off}
  fCoordinates := TURLParamCoordinate.Create(Self);
  {$Hints on}
  {$Warnings on}
  fCount       := TURLParamCount.Create(Self);
  fEndTime     := TURLParamEndTime.Create(Self);
  fFindType    := TURLParamFindType.Create(Self);
  fHistoryType := TURLParamHistoryType.Create(Self);
  fID          := TURLParamID.Create(Self);
  fLanguage    := TURLParamLanguage.Create(Self);
  fMode        := TURLParamMode.Create(Self);
  fQuery       := TURLParamQuery.Create(Self);
  fStartTime   := TURLParamStartTime.Create(Self);
  fUnits       := TURLParamUnits.Create(Self);
  fAPIKey      := TURLParamAPIKey.Create(Self);
  fMultipleIDs := TURLParamMultipleID.Create(Self);
end;

destructor TOWMCustomOptions.Destroy;
begin
  fCallback.Free;
  fCoordinates.Free;
  fCount.Free;
  fEndTime.Free;
  fFindType.Free;
  fHistoryType.Free;
  fID.Free;
  fLanguage.Free;
  fMode.Free;
  fQuery.Free;
  fStartTime.Free;
  fUnits.Free;
  fAPIKey.Free;
  fMultipleIDs.Free;
  inherited Destroy;
end;

function TOWMCustomOptions.GetAsText: UTF8String;
begin
  Result := ParamsToString([Location, Mode, Units, Language, Callback, Count,
    APIKey]);
end;

function TOWMCustomOptions.ToString: String;
begin
  Result := GetAsText;
end;

procedure TOWMCustomOptions.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
end;

function TOWMCustomOptions.GetLocation: TURLParamBase;
begin
  Result := GetFirstSetParam([fQuery, fCoordinates, fID]);
end;

{ TURLParamAPIKey }

class function TURLParamAPIKey.GetParamName: UTF8String;
begin
  Result := 'apikey';
end;

{ TURLParamCallback }

class function TURLParamCallback.GetParamName: UTF8String;
begin
  Result := 'callback';
end;

{ TUrlParamCount }

class function TURLParamCount.GetParamName: UTF8String;
begin
  Result := 'cnt';
end;

{ TURLParamID }

class function TURLParamID.GetParamName: UTF8String;
begin
  Result := 'id';
end;

{ TURLParamInt }

function TURLParamInt.GetValueAsString: UTF8String;
begin
  Result := IntToStr(fValue);
end;

procedure TURLParamInt.SetValue(aValue: _URLType);
begin
  inherited SetValue(EnsureRange(aValue, GetMinValue, GetMaxValue));
end;

function TURLParamInt.GetMinValue: Int64;
begin
  Result := 0;
end;

function TURLParamInt.GetMaxValue: Int64;
begin
  Result := High(Result);
end;

{ TURLParamQuery }

class function TURLParamQuery.GetParamName: UTF8String;
begin
  Result := 'q';
end;

{ TURLParamString }

function TURLParamString.GetValueAsString: UTF8String;
begin
  Result := fValue;
end;

{ TURLParamCoordinate }

procedure TURLParamCoordinate.SetLatitude(aValue: Double);
begin
  if (aValue < -180) or (aValue > 180) then
    raise ERangeError.CreateFmt('Latitude out of range (%f).', [aValue]);
  fValue.Latitude := aValue;
  fIsSet := True;
end;

procedure TURLParamCoordinate.SetLongitude(aValue: Double);
begin
  if (aValue < -180) or (aValue > 180) then
    raise ERangeError.CreateFmt('Longitude out of range (%f).', [aValue]);
  fValue.Longitude := aValue;
  fIsSet := True;
end;

function TURLParamCoordinate.GetAsText: UTF8String;
begin
  if IsSet then
    Result := Format(GetParamName, [fValue.Latitude, fValue.Longitude])
  else
    Result := '';
end;

class function TURLParamCoordinate.GetParamName: UTF8String;
begin
  Result := 'lat=%f&long=&f'
end;

{ TURLParamLanguage }

class function TURLParamLanguage.GetParamName: UTF8String;
begin
  Result := 'lang';
end;

function TURLParamLanguage.GetValueAsString: UTF8String;
begin
  case fValue of
      loEnglish           : Result := 'en';
      loRussian           : Result := 'ru';
      loItalian           : Result := 'it';
      loSpanish           : Result := 'sp';
      loUkrainian         : Result := 'ua';
      loGerman            : Result := 'de';
      loPortuguese        : Result := 'pt';
      loRomanian          : Result := 'ro';
      loPolish            : Result := 'pl';
      loFinnish           : Result := 'fi';
      loDutch             : Result := 'nl';
      loFrench            : Result := 'fr';
      loBulgarian         : Result := 'bg';
      loSwedish           : Result := 'se';
      loChineseTraditional: Result := 'zh_tw';
      loChineseSimplified : Result := 'zh_cn';
      loTurkish           : Result := 'tr ';
      loCzech             : Result := 'cz';
      loGalician          : Result := 'gl';
      loVietnamese        : Result := 'vi';
      loArabic            : Result := 'ar';
      loMacedonian        : Result := 'mk';
      loSlovak            : Result := 'sk';
  end;

end;

{ TURLParamUnits }

class function TURLParamUnits.GetParamName: UTF8String;
begin
  Result := 'units';
end;

function TURLParamUnits.GetValueAsString: UTF8String;
begin
  case fValue of
    uoImperial: Result := 'imperial';
    uoMetric  : Result := 'metric';
  end;
end;

{ TURLParamMode }

class function TURLParamMode.GetParamName: UTF8String;
begin
  Result := 'mode';
end;

function TURLParamMode.GetValueAsString: UTF8String;
begin
  case fValue of
    moXML : Result := 'xml';
    moJSON: Result := 'json';
    moHTML: Result := 'html';
  end;
end;

{ TURLParam }

procedure TURLParam.SetValue(aValue: _URLType);
begin
  fValue := aValue;
  fIsSet := True;
  NotifyChangeOwner;
end;

initialization
 {$I fpowm_component_images.lrs}
end.

