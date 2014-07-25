unit ufrmstreamtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  owmdata;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnLoadjson: TButton;
    BtnSavejson: TButton;
    Button1: TButton;
    MeJSON: TMemo;
    Memo1: TMemo;
    procedure BtnLoadjsonClick(Sender: TObject);
    procedure BtnSavejsonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    w: TForecast3hData;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses fpjson, fpjsonrtti, jsonparser, LResources, dateutils;

procedure ReplaceForecast3hToRain3h(root: TJSONObject);
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
    if assigned(rain) then
      rain.Add('Rain3h', rain.Extract('3h'));  // move to Rain3h

    snow := TJSONObject(ListObject.Find('snow', jtObject));
    if Assigned(snow) then
      snow.Add('Snow3h', snow.Extract('3h'));  // move to Snow3h

    if Assigned(ListObject.Find('id', jtObject)) then
      ListObject.Add('LocationID', ListObject.Extract('id'));  // find
  end;

  if Assigned(root.Find('type', jtString)) then
    root.Add('TickType', root.Extract('type'));
end;

procedure ReplaceForecastRain3hTo3h(root: TJSONObject);
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

    if Assigned(ListObject.Find('id', jtObject)) then
      ListObject.Add('LocationID', ListObject.Extract('id'));  // find
  end;

  if Assigned(root.Find('TickType')) then
    root.Add('type', root.Extract('TickType'));
end;

{ TForm1 }

procedure TForm1.BtnLoadjsonClick(Sender: TObject);
var
  p: TJSONParser;
  jo: TJSONData;
  s: TJSONDeStreamer;
begin
  if not Assigned(w) then
    w := TForecast3hData.Create;

  s := TJSONDeStreamer.Create(Self);
  p := TJSONParser.Create(MeJSON.Text, True);
  try
    jo := TJSONObject(p.Parse);
    // replace "3h" property with Rain3h for rain object
    ReplaceForecast3hToRain3h(TJSONObject(jo));
    s.JSONToObject(TJSONObject(jo), w);
  finally
    jo.Free;
    p.Free;
    s.Free;
  end;
end;

procedure TForm1.BtnSavejsonClick(Sender: TObject);
var
  s: TJSONStreamer;
  o: TJSONObject;
begin
  if not Assigned(w) then exit;
  s := TJSONStreamer.Create(self);
  o := nil;
  try
    o := s.ObjectToJSON(w);
    ReplaceForecastRain3hTo3h(o);
    Memo1.Text := o.AsJSON;
  finally
    o.Free;
    s.Destroy;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  s: TJSONStreamer;
  o: TJSONObject;
begin
  s := TJSONStreamer.Create(TComponent(Sender));
  o := s.ObjectToJSON(Sender);
  Memo1.Text := o.AsJSON;
  o.Free;
  s.Free;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  w.Free;
end;

end.

