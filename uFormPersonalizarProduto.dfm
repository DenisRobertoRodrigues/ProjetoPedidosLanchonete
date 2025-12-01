object FormPersonalizarProduto: TFormPersonalizarProduto
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Personalizar Produto'
  ClientHeight = 600
  ClientWidth = 700
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 700
    Height = 60
    Align = alTop
    BevelOuter = bvNone
    Color = clNavy
    ParentBackground = False
    TabOrder = 0
    object lblTitulo: TLabel
      Left = 16
      Top = 16
      Width = 250
      Height = 29
      Caption = 'Personalizar Produto'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -24
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 560
    Width = 700
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnConfirmar: TButton
      Left = 480
      Top = 7
      Width = 100
      Height = 26
      Caption = 'Confirmar'
      TabOrder = 0
      OnClick = btnConfirmarClick
    end
    object btnCancelar: TButton
      Left = 586
      Top = 7
      Width = 100
      Height = 26
      Caption = 'Cancelar'
      TabOrder = 1
      OnClick = btnCancelarClick
    end
  end
  object pnlCenter: TPanel
    Left = 0
    Top = 60
    Width = 700
    Height = 500
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object lblInfo: TLabel
      Left = 16
      Top = 16
      Width = 369
      Height = 13
      Caption = 
        'Desmarque ingredientes para remover ou marque para adicionar ao ' +
        'produto:'
    end
    object lblQuantidade: TLabel
      Left = 16
      Top = 440
      Width = 60
      Height = 13
      Caption = 'Quantidade:'
    end
    object gbIngredientesOriginais: TGroupBox
      Left = 16
      Top = 40
      Width = 320
      Height = 200
      Caption = ' Ingredientes do Produto '
      TabOrder = 0
      object chkListIngredientesOriginais: TCheckListBox
        Left = 8
        Top = 20
        Width = 304
        Height = 172
        ItemHeight = 15
        TabOrder = 0
        OnClickCheck = chkListIngredientesOriginaisClickCheck
      end
    end
    object gbIngredientesAdicionais: TGroupBox
      Left = 352
      Top = 40
      Width = 320
      Height = 200
      Caption = ' Ingredientes Adicionais '
      TabOrder = 1
      object chkListIngredientesAdicionais: TCheckListBox
        Left = 8
        Top = 20
        Width = 304
        Height = 172
        ItemHeight = 15
        TabOrder = 0
        OnClickCheck = chkListIngredientesAdicionaisClickCheck
      end
    end
    object pnlValores: TPanel
      Left = 16
      Top = 246
      Width = 656
      Height = 60
      BevelOuter = bvNone
      Color = 15790320
      ParentBackground = False
      TabOrder = 2
      object lblValorOriginalTexto: TLabel
        Left = 16
        Top = 12
        Width = 67
        Height = 13
        Caption = 'Valor Original:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblValorOriginal: TLabel
        Left = 120
        Top = 8
        Width = 56
        Height = 19
        Caption = 'R$ 0,00'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblValorFinalTexto: TLabel
        Left = 16
        Top = 36
        Width = 61
        Height = 13
        Caption = 'Valor Final:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblValorFinal: TLabel
        Left = 120
        Top = 32
        Width = 62
        Height = 19
        Caption = 'R$ 0,00'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object gbObservacao: TGroupBox
      Left = 16
      Top = 312
      Width = 656
      Height = 113
      Caption = ' Observa'#231#245'es Adicionais '
      TabOrder = 3
      object memoObservacao: TMemo
        Left = 8
        Top = 20
        Width = 640
        Height = 85
        TabOrder = 0
      end
    end
    object edtQuantidade: TEdit
      Left = 84
      Top = 437
      Width = 60
      Height = 21
      Alignment = taCenter
      TabOrder = 4
      Text = '1'
      OnChange = edtQuantidadeChange
    end
    object btnMenos: TButton
      Left = 150
      Top = 435
      Width = 30
      Height = 25
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
      OnClick = btnMenosClick
    end
    object btnMais: TButton
      Left = 186
      Top = 435
      Width = 30
      Height = 25
      Caption = '+'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 6
      OnClick = btnMaisClick
    end
  end
end
