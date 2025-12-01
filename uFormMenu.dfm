object FormMenu: TFormMenu
  Left = 0
  Top = 0
  Caption = 'Card'#225'pio - Lanchonete'
  ClientHeight = 700
  ClientWidth = 1200
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1200
    Height = 100
    Align = alTop
    BevelOuter = bvNone
    Color = 2697513
    ParentBackground = False
    TabOrder = 0
    object lblTitulo: TLabel
      Left = 24
      Top = 16
      Width = 165
      Height = 45
      Caption = 'CARD'#193'PIO'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -32
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object pnlFiltro: TPanel
      Left = 0
      Top = 60
      Width = 1200
      Height = 40
      Align = alBottom
      BevelOuter = bvNone
      Color = 2697513
      ParentBackground = False
      TabOrder = 0
      object lblFiltro: TLabel
        Left = 24
        Top = 12
        Width = 50
        Height = 13
        Caption = 'Pesquisar:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object edtFiltro: TEdit
        Left = 88
        Top = 9
        Width = 300
        Height = 21
        TabOrder = 0
        OnChange = edtFiltroChange
      end
      object chkApenasAtivos: TCheckBox
        Left = 408
        Top = 11
        Width = 120
        Height = 17
        Caption = 'Apenas Ativos'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 1
        OnClick = chkApenasAtivosClick
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 660
    Width = 1200
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    Color = 3815994
    ParentBackground = False
    TabOrder = 1
    object pnlCotacao: TPanel
      Left = 0
      Top = 0
      Width = 1200
      Height = 40
      Align = alClient
      BevelOuter = bvNone
      Color = 3815994
      ParentBackground = False
      TabOrder = 0
      object btnAtualizar: TButton
        Left = 8
        Top = 6
        Width = 120
        Height = 25
        Caption = 'Atualizar Card'#225'pio'
        TabOrder = 0
        OnClick = btnAtualizarClick
      end
    end
  end
  object ScrollBox: TScrollBox
    Left = 0
    Top = 100
    Width = 1200
    Height = 560
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clWhite
    ParentColor = False
    TabOrder = 2
  end
end
