unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, RTTICtrls, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, owmurloptions, PropEdits, urloptionsedit;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnQuery: TButton;
    BtnStream: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    RadioGroup1: TRadioGroup;
    RgStreamFmt: TRadioGroup;
    TILabel1: TTILabel;
    TIPropertyGrid1: TTIPropertyGrid;
    procedure BtnQueryClick(Sender: TObject);
    procedure BtnStreamClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    fURLOptions: array [0..4] of TOWMCustomOptions;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses math, fphttpclient, owmdata, LResources;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  fURLOptions[0] := TOWMWeatherOptions.Create(Self);
  fURLOptions[1] := TOWMGroupOptions.Create(Self);
  fURLOptions[2] := TOWMForecastOptions.Create(Self);
  fURLOptions[3] := TOWMFindOptions.Create(Self);
  fURLOptions[4] := TOWMHistoryOptions.Create(Self);
end;

procedure TForm1.BtnQueryClick(Sender: TObject);
var
  url: String;
  r: TStringList;
begin
  Memo1.Clear;
  Application.ProcessMessages;

  url := OWM_BASE_URL;
  case RadioGroup1.ItemIndex of
    0: url += OWM_ENDPOINT_WEATHER;
    1: url += OWM_ENDPOINT_GROUP;
    2: url += OWM_ENDPOINT_FORECAST;
    3: url += OWM_ENDPOINT_FIND;
    4: url += OWm_ENDPOINT_HISTORY_CITY;
  else
    exit;
  end;
  url += '?' + fURLOptions[RadioGroup1.ItemIndex].AsText;

  Memo1.Append('URL: ' + url);
  Application.ProcessMessages;

  r := TStringList.Create;
  try
    TFPHTTPClient.SimpleGet(url, r);
    Memo1.Lines.AddStrings(r);
  finally
    r.Destroy;
  end;
end;

procedure TForm1.BtnStreamClick(Sender: TObject);
var
  c: TComponent;
  s: TStringStream;
begin
  if not InRange(RadioGroup1.ItemIndex, low(fURLOptions), high(fURLOptions)) then
    exit;

  Memo1.Clear;
  s := TStringStream.Create('');
  try
    c := fURLOptions[RadioGroup1.ItemIndex];
    s.Size := 0;
    case RgStreamFmt.ItemIndex of
      0: WriteComponentAsTextToStream(s, c);
      1: WriteComponentAsBinaryToStream(s, c);
    end;
    Memo1.Lines.Add(c.ClassName);
    Memo1.Lines.Add(s.DataString);
    Memo1.Lines.Add('');
  finally
    s.Destroy;
  end;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  if InRange(RadioGroup1.ItemIndex, low(fURLOptions), high(fURLOptions)) then
  begin
    TIPropertyGrid1.TIObject := fURLOptions[RadioGroup1.ItemIndex];
    TILabel1.Link.TIObject   := fURLOptions[RadioGroup1.ItemIndex];
  end;
end;

end.

