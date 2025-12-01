object FormIngrediente: TFormIngrediente
  Left = 0
  Top = 0
  Caption = 'Cadastro de Ingredientes'
  ClientHeight = 600
  ClientWidth = 900
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 900
    Height = 60
    Align = alTop
    BevelOuter = bvNone
    Color = clNavy
    ParentBackground = False
    TabOrder = 0
    object lblTitulo: TLabel
      Left = 16
      Top = 16
      Width = 378
      Height = 29
      Caption = 'Gerenciamento de Ingredientes'
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
    Width = 900
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pnlCenter: TPanel
    Left = 0
    Top = 60
    Width = 900
    Height = 500
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 900
      Height = 500
      ActivePage = tsLista
      Align = alClient
      TabOrder = 0
      object tsLista: TTabSheet
        Caption = 'Lista de Ingredientes'
        object pnlFiltro: TPanel
          Left = 0
          Top = 0
          Width = 892
          Height = 50
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblFiltro: TLabel
            Left = 16
            Top = 16
            Width = 50
            Height = 13
            Caption = 'Pesquisar:'
          end
          object edtFiltro: TEdit
            Left = 88
            Top = 13
            Width = 300
            Height = 21
            TabOrder = 0
            OnChange = edtFiltroChange
          end
          object chkApenasAtivos: TCheckBox
            Left = 408
            Top = 15
            Width = 120
            Height = 17
            Caption = 'Apenas Ativos'
            Checked = True
            State = cbChecked
            TabOrder = 1
            OnClick = chkApenasAtivosClick
          end
        end
        object GridIngredientes: TStringGrid
          Left = 0
          Top = 50
          Width = 892
          Height = 372
          Align = alClient
          ColCount = 6
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
          TabOrder = 1
          OnDblClick = GridIngredientesDblClick
        end
        object pnlBotoesLista: TPanel
          Left = 0
          Top = 422
          Width = 892
          Height = 50
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 2
          object btnNovo: TButton
            Left = 16
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Novo'
            TabOrder = 0
            OnClick = btnNovoClick
          end
          object btnEditar: TButton
            Left = 122
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Editar'
            TabOrder = 1
            OnClick = btnEditarClick
          end
          object btnExcluir: TButton
            Left = 228
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Excluir'
            TabOrder = 2
            OnClick = btnExcluirClick
          end
          object btnAtualizar: TButton
            Left = 334
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Atualizar'
            TabOrder = 3
            OnClick = btnAtualizarClick
          end
        end
      end
      object tsCadastro: TTabSheet
        Caption = 'Cadastro'
        ImageIndex = 1
        object lblNome: TLabel
          Left = 24
          Top = 24
          Width = 31
          Height = 13
          Caption = 'Nome:'
        end
        object lblValor: TLabel
          Left = 24
          Top = 75
          Width = 52
          Height = 13
          Caption = 'Valor (R$):'
        end
        object lblImagem: TLabel
          Left = 480
          Top = 24
          Width = 42
          Height = 13
          Caption = 'Imagem:'
        end
        object edtNome: TEdit
          Left = 24
          Top = 43
          Width = 400
          Height = 21
          TabOrder = 0
        end
        object edtValor: TEdit
          Left = 24
          Top = 94
          Width = 150
          Height = 21
          TabOrder = 1
          Text = '0,00'
          OnKeyPress = edtValorKeyPress
        end
        object chkAtivo: TCheckBox
          Left = 24
          Top = 133
          Width = 97
          Height = 17
          Caption = 'Ativo'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object pnlImagem: TPanel
          Left = 480
          Top = 43
          Width = 350
          Height = 300
          BevelOuter = bvNone
          BorderStyle = bsSingle
          TabOrder = 3
          object imgIngrediente: TImage
            Left = 0
            Top = 0
            Width = 346
            Height = 296
            Align = alClient
            Center = True
            Proportional = True
            Stretch = True
          end
        end
        object btnSelecionarImagem: TButton
          Left = 480
          Top = 349
          Width = 140
          Height = 30
          Caption = 'Selecionar Imagem'
          TabOrder = 4
          OnClick = btnSelecionarImagemClick
        end
        object btnRemoverImagem: TButton
          Left = 626
          Top = 349
          Width = 120
          Height = 30
          Caption = 'Remover Imagem'
          Enabled = False
          TabOrder = 5
          OnClick = btnRemoverImagemClick
        end
        object pnlBotoesCadastro: TPanel
          Left = 0
          Top = 422
          Width = 892
          Height = 50
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 6
          object btnSalvar: TButton
            Left = 24
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Salvar'
            TabOrder = 0
            OnClick = btnSalvarClick
          end
          object btnCancelar: TButton
            Left = 130
            Top = 10
            Width = 100
            Height = 30
            Caption = 'Cancelar'
            TabOrder = 1
            OnClick = btnCancelarClick
          end
        end
      end
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 'Imagens|*.jpg;*.jpeg;*.png;*.bmp;*.gif|Todos os arquivos|*.*'
    Title = 'Selecionar Imagem do Ingrediente'
    Left = 408
    Top = 232
  end
end
