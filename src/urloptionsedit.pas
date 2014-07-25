{ Property editors for OpenWeatherMap URL options

  Copyright (C) 2014, Simon Ameis <simon.ameis@web.de>

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit urloptionsedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, StringsPropEditDlg, owmurloptions, Controls,
  owmdata;

type
  TMultipleIDsPropEditorFrm = class(TStringsPropEditorDlg)

  end;

  { TMulitpleIDsPropertyEditor }

  TMulitpleIDsPropertyEditor = class(TStringsPropertyEditor)
  public
    procedure Edit; override;
  end;

  { TOWMDataComponent }

  TOWMDataComponent = class(TComponent)
  private
    fForecast3h: TForecast3hData;
    fForecastDaily: TForecastDailyData;
    fFind: TFindData;
    fGroup: TGroupData;
    fHistoryCity: THistoryCityData;
    fHistoryStation: THistoryStationData;
    fWeather: TWeatherData;
    function GetFind: TFindData;
    function GetForecast3h: TForecast3hData;
    function GetForecastDaily: TForecastDailyData;
    function GetGroup: TGroupData;
    function GetHistoryCity: THistoryCityData;
    function GetHistoryStation: THistoryStationData;
    function GetWeather: TWeatherData;
  public
    destructor Destroy; override;
  published
    property Forecast3h: TForecast3hData read GetForecast3h;
    property ForecastDaily: TForecastDailyData read GetForecastDaily;
    property Find: TFindData read GetFind;
    property Group: TGroupData read GetGroup;
    property HistoryCity: THistoryCityData read GetHistoryCity;
    property HistoryStation: THistoryStationData read GetHistoryStation;
    property Weather: TWeatherData read GetWeather;
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('OpenWeatherMap', [TOWMDataComponent]);

  // Register Property Editors
  RegisterPropertyEditor(TypeInfo(TComponentName)      , TURLParamBase      , 'Name'       , THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer)             , TURLParamBase      , 'Tag'        , THiddenPropertyEditor);

  // TOWMCustomOptions
  RegisterPropertyEditor(TypeInfo(TURLParamAPIKey)     , TOWMCustomOptions  , 'APIKey'     , TClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TURLParamCallback)   , TOWMCustomOptions  , 'Callback'   , TClassPropertyEditor);

  // TOWMWeatherOptions
  RegisterPropertyEditor(TypeInfo(TURLParamMode)       , TOWMWeatherOptions , 'Mode'       , TClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TURLParamQuery)      , TOWMWeatherOptions , 'Query'      , TClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TURLParamUnits)      , TOWMWeatherOptions , 'Units'      , TClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TURLParamLanguage)   , TOWMWeatherOptions , 'Language'   , TClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TURLParamID)         , TOWMWeatherOptions , 'ID'         , TClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TURLParamCoordinate) , TOWMWeatherOptions , 'Coordinates', TClassPropertyEditor);

  // TOWMGroupOptions
  RegisterPropertyEditor(TypeInfo(TURLParamMode)       , TOWMGroupOptions   , 'Mode'       , TClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TURLParamUnits)      , TOWMGroupOptions   , 'Units'      , TClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TURLParamLanguage)   , TOWMGroupOptions   , 'Language'   , TClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TURLParamMultipleID) , TOWMGroupOptions   , 'MultipleIDs', TClassPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TMultipleIDS)        , TURLParamMultipleID, 'Value'      , TMulitpleIDsPropertyEditor);

  // TOWMForecastOptions
  RegisterPropertyEditor(TypeInfo(TURLParamCount)      , TOWMForecastOptions, 'Count'      , TClassPropertyEditor);

  // TOWMFindOptions
  RegisterPropertyEditor(TypeInfo(TURLParamQuery)      , TOWMFindOptions    , 'Query'      , TClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TURLParamCoordinate) , TOWMFindOptions    , 'Coordinates', TClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TURLParamCount)      , TOWMFindOptions    , 'Count'      , TClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TURLParamFindType)   , TOWMFindOptions    , 'FindType'   , TClassPropertyEditor);

  // TOWMHistoryOptions
  RegisterPropertyEditor(TypeInfo(TURLParamDateTime)   , TOWMHistoryOptions , 'StartTime'  , TClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TURLParamDateTime)   , TOWMHistoryOptions , 'EndTime'    , TClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TURLParamUnits)      , TOWMHistoryOptions , 'Units'      , TClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TURLParamID)         , TOWMHistoryOptions , 'ID'         , TClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TURLParamCount)      , TOWMHistoryOptions , 'Count'      , TClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TURLParamHistoryType),TOWMHistoryOptions  , 'HistoryType', TClassPropertyEditor);
end;

{ TOWMDataComponent }

function TOWMDataComponent.GetFind: TFindData;
begin
  if not Assigned(fFind) then
    fFind := TFindData.Create;
  Result := fFind;
end;

function TOWMDataComponent.GetForecast3h: TForecast3hData;
begin
  if not Assigned(fForecast3h) then
    fForecast3h := TForecast3hData.Create;
  Result := fForecast3h;
end;

function TOWMDataComponent.GetForecastDaily: TForecastDailyData;
begin
  if not Assigned(fForecastDaily) then
    fForecastDaily := TForecastDailyData.Create;
  Result := fForecastDaily;
end;

function TOWMDataComponent.GetGroup: TGroupData;
begin
  if not Assigned(fGroup) then
    fGroup := TGroupData.Create;
  Result := fGroup;
end;

function TOWMDataComponent.GetHistoryCity: THistoryCityData;
begin
  if not Assigned(fHistoryCity) then
    fHistoryCity := THistoryCityData.Create;
  Result := fHistoryCity;
end;

function TOWMDataComponent.GetHistoryStation: THistoryStationData;
begin
  if not Assigned(fHistoryStation) then
    fHistoryStation := THistoryStationData.Create;
  Result := fHistoryStation;
end;

function TOWMDataComponent.GetWeather: TWeatherData;
begin
  if not Assigned(fWeather) then
    fWeather := TWeatherData.Create(nil);
  Result := fWeather;
end;

destructor TOWMDataComponent.Destroy;
begin
  fForecast3h.Free;
  fForecastDaily.Free;
  fFind.Free;
  fGroup.Free;
  fHistoryCity.Free;
  fHistoryStation.Free;
  fWeather.Free;
  inherited Destroy;
end;

{ TMulitpleIDsPropertyEditor }

procedure TMulitpleIDsPropertyEditor.Edit;
var
  TheDialog: TStringsPropEditorDlg;
  s: TStringList;
  l: TMultipleIDS;
  i: Int64;
  line: String;
begin
  s := TStringList.Create;
  try
    l := TMultipleIDS(GetObjectValue);
    for i in l do
      s.Add(IntToStr(i));
    TheDialog := CreateDlg(s);
    if (TheDialog.ShowModal = mrOK) then
    begin
      l.Clear;
      for line in TheDialog.Memo.Lines do
        if TryStrToInt64(line, i) then
          l.Add(i);
    end;
  finally
    s.Free;
    TheDialog.Free;
  end;
end;

end.

