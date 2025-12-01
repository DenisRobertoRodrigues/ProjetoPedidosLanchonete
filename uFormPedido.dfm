object FormPedido: TFormPedido
  Left = 0
  Top = 0
  Caption = 'Novo Pedido'
  ClientHeight = 700
  ClientWidth = 1100
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
    Width = 1100
    Height = 100
    Align = alTop
    BevelOuter = bvNone
    Color = 2697513
    ParentBackground = False
    TabOrder = 0
    object lblTitulo: TLabel
      Left = 24
      Top = 16
      Width = 175
      Height = 29
      Caption = 'NOVO PEDIDO'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -24
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object pnlCliente: TPanel
      Left = 0
      Top = 60
      Width = 1100
      Height = 40
      Align = alBottom
      BevelOuter = bvNone
      Color = 2697513
      ParentBackground = False
      TabOrder = 0
      object lblCliente: TLabel
        Left = 24
        Top = 12
        Width = 37
        Height = 13
        Caption = 'Cliente:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object cmbClientes: TComboBox
        Left = 72
        Top = 9
        Width = 350
        Height = 21
        Style = csDropDownList
        TabOrder = 0
      end
      object btnNovoCliente: TButton
        Left = 428
        Top = 7
        Width = 100
        Height = 25
        Caption = 'Novo Cliente'
        TabOrder = 1
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 660
    Width = 1100
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    Color = 3815994
    ParentBackground = False
    TabOrder = 1
    object pnlCotacao: TPanel
      Left = 0
      Top = 0
      Width = 1100
      Height = 40
      Align = alClient
      BevelOuter = bvNone
      Color = 3815994
      ParentBackground = False
      TabOrder = 0
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 100
    Width = 1100
    Height = 560
    ActivePage = tsCarrinho
    Align = alClient
    TabOrder = 2
    object tsProdutos: TTabSheet
      Caption = '1. Selecionar Produtos'
      object pnlProdutos: TPanel
        Left = 0
        Top = 0
        Width = 400
        Height = 532
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object lblFiltroProduto: TLabel
          Left = 16
          Top = 16
          Width = 50
          Height = 13
          Caption = 'Pesquisar:'
        end
        object edtFiltroProduto: TEdit
          Left = 16
          Top = 35
          Width = 360
          Height = 21
          TabOrder = 0
          OnChange = edtFiltroProdutoChange
        end
        object lstProdutos: TListBox
          Left = 16
          Top = 62
          Width = 360
          Height = 454
          ItemHeight = 13
          TabOrder = 1
          OnClick = lstProdutosClick
        end
      end
      object pnlDetalhesProduto: TPanel
        Left = 400
        Top = 0
        Width = 692
        Height = 532
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object lblDetalhesProduto: TLabel
          Left = 24
          Top = 16
          Width = 115
          Height = 13
          Caption = 'Detalhes do Produto'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblNomeProduto: TLabel
          Left = 24
          Top = 48
          Width = 138
          Height = 23
          Caption = 'Nome Produto'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -19
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblValorProduto: TLabel
          Left = 24
          Top = 77
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
        object lblIngredientesProduto: TLabel
          Left = 24
          Top = 115
          Width = 65
          Height = 13
          Caption = 'Ingredientes:'
        end
        object memoIngredientesProduto: TMemo
          Left = 24
          Top = 134
          Width = 640
          Height = 300
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object btnAdicionarProduto: TButton
          Left = 24
          Top = 450
          Width = 200
          Height = 40
          Caption = 'Adicionar ao Pedido'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          OnClick = btnAdicionarProdutoClick
        end
      end
    end
    object tsCarrinho: TTabSheet
      Caption = '2. Carrinho / Finalizar'
      ImageIndex = 1
      object GridCarrinho: TStringGrid
        Left = 0
        Top = 0
        Width = 1092
        Height = 400
        Align = alClient
        ColCount = 7
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
        TabOrder = 0
      end
      object pnlTotais: TPanel
        Left = 0
        Top = 400
        Width = 1092
        Height = 80
        Align = alBottom
        BevelOuter = bvNone
        Color = 15790320
        ParentBackground = False
        TabOrder = 1
        object lblTotalPedido: TLabel
          Left = 24
          Top = 16
          Width = 94
          Height = 16
          Caption = 'Total do Pedido:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblTotalPedidoValor: TLabel
          Left = 145
          Top = 12
          Width = 74
          Height = 23
          Caption = 'R$ 0,00'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGreen
          Font.Height = -19
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblTotalDolar: TLabel
          Left = 24
          Top = 48
          Width = 103
          Height = 13
          Caption = 'Total em D'#243'lar (US$):'
        end
        object lblTotalDolarValor: TLabel
          Left = 145
          Top = 44
          Width = 52
          Height = 16
          Caption = 'US$ 0,00'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
      end
      object pnlBotoesCarrinho: TPanel
        Left = 0
        Top = 480
        Width = 1092
        Height = 52
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        object btnRemoverItem: TButton
          Left = 24
          Top = 10
          Width = 120
          Height = 30
          Caption = 'Remover Item'
          Enabled = False
          TabOrder = 0
          OnClick = btnRemoverItemClick
        end
        object btnLimparCarrinho: TButton
          Left = 150
          Top = 10
          Width = 120
          Height = 30
          Caption = 'Limpar Carrinho'
          Enabled = False
          TabOrder = 1
          OnClick = btnLimparCarrinhoClick
        end
        object btnFinalizarPedido: TButton
          Left = 920
          Top = 6
          Width = 150
          Height = 40
          Caption = 'Finalizar Pedido'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          OnClick = btnFinalizarPedidoClick
        end
      end
    end
  end
end
